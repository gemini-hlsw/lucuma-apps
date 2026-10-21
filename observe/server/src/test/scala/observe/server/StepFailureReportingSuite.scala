// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptyList
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.refined.auto.*
import lucuma.core.util.TimeSpan
import observe.common.test.*
import observe.model.ActionType
import observe.model.ClientId
import observe.model.CurrentConditions
import observe.model.Observer
import observe.model.SequenceStatus
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.model.enums.RunOverride
import observe.server.engine.Action.ActionState
import observe.server.engine.Engine
import observe.server.engine.user
import observe.server.gmos.GmosController
import observe.server.gmos.GmosControllerDisabled
import observe.server.gmos.GmosNorthController
import observe.server.odb.TestOdbProxy
import observe.server.odb.TestOdbProxy.*

import java.util.UUID

/**
 * Reproduces how step failures are reported to the ODB. A subsystem error or timeout is not an
 * abort.
 */
class StepFailureReportingSuite extends TestCommon {
  import TestCommon.*

  private val clientId: ClientId = ClientId(UUID.randomUUID())
  private val observer: Observer = Observer("Joe".refined)
  private val atomId: Atom.Id    = Atom.Id.fromUuid(UUID.randomUUID())

  private val singleStep: Atom[DynamicConfig.GmosNorth] =
    Atom(
      atomId,
      none,
      NonEmptyList.one(
        Step(
          stepId(1),
          dynamicCfg1,
          stepCfg1,
          telescopeCfg1,
          StepEstimate.Zero,
          ObserveClass.Science,
          Breakpoint.Disabled
        )
      )
    )

  /** GMOS whose configuration fails the way a subsystem timeout does. */
  private def gmosFailingConfig: GmosNorthController[IO] =
    new GmosControllerDisabled[IO, GmosController.GmosSite.North.type]("north"):
      override def applyConfig(
        config: GmosController.GmosConfig[GmosController.GmosSite.North.type]
      ): IO[Unit] =
        IO.raiseError(ObserveFailure.Timeout("GMOS configuration"))

  /** GMOS whose configuration fails once, then succeeds: a transient failure. */
  private def gmosFailingOnce(failed: Ref[IO, Boolean]): GmosNorthController[IO] =
    new GmosControllerDisabled[IO, GmosController.GmosSite.North.type]("north"):
      override def applyConfig(
        config: GmosController.GmosConfig[GmosController.GmosSite.North.type]
      ): IO[Unit] =
        failed
          .getAndSet(true)
          .flatMap: alreadyFailed =>
            IO.raiseError(ObserveFailure.Timeout("GMOS configuration")).unlessA(alreadyFailed)

  /**
   * GMOS whose exposure blocks until paused, and whose stop-after-pause behaves as `stopPaused`
   * says. This mimics the EPICS stop-and-wait command, which can time out during readout.
   */
  private def gmosPausable(
    pause:      Deferred[IO, Unit],
    stopPaused: IO[ObserveCommandResult]
  ): GmosNorthController[IO] =
    val stopResult = stopPaused
    new GmosControllerDisabled[IO, GmosController.GmosSite.North.type]("north"):
      override def observe(fileId: ImageFileId, expTime: TimeSpan): IO[ObserveCommandResult] =
        pause.get.as(ObserveCommandResult.Paused)
      override def pauseExposure: IO[Unit]                                                   = pause.complete(()).void
      override def stopPaused: IO[ObserveCommandResult]                                      = stopResult

  private def observeActionState(st: EngineState[IO]): Option[ActionState] =
    st.sequences
      .get(seqObsId1)
      .flatMap(_.seq.currentExecution.execution.find(_.kind === ActionType.Observe))
      .map(_.state.runState)

  private def sequenceStatus(st: EngineState[IO]): Option[SequenceStatus] =
    st.sequences.get(seqObsId1).map(_.seq.status)

  private def gmosSingleRunState(st: EngineState[IO]): Option[ActionState] =
    st.sequences
      .get(seqObsId1)
      .flatMap: sd =>
        sd.configActionCoord(stepId(1), Instrument.GmosNorth).map(sd.seq.getSingleState)

  /**
   * Drives the engine like `clientEventStream` does, including the ODB notification that the plain
   * `eventResultStream` used by other tests leaves out.
   */
  private final class RunningEngine(
    val odb: TestOdbProxy[IO],
    oe:      ObserveEngine[IO],
    states:  SignallingRef[IO, Option[EngineState[IO]]]
  ):
    def sendAndAwait(cmd: ObserveEngine[IO] => IO[Unit])(
      until: EngineState[IO] => Boolean
    ): IO[EngineState[IO]] =
      // `discrete` replays the current state first; skip it so a stale match can't satisfy `until`.
      cmd(oe) *> states.discrete.drop(1).unNone.find(until).compile.lastOrError

  private def withRunningEngine[A](
    gmos: GmosNorthController[IO]
  )(body: RunningEngine => IO[A]): IO[A] =
    for
      odb      <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, none, List(singleStep))
      services <- defaultServices.map(_.copy(odb = odb, gmosNorth = gmos))
      oseq     <- odb.read(seqObsId1)
      s0        = ODBSequencesLoader
                    .loadSequenceMod[IO](None, oseq, EngineState.instrumentLoaded(Instrument.GmosNorth))
                    .apply(EngineState.default[IO])
      rc       <- Ref.of[IO, CurrentConditions](CurrentConditions.Default)
      tr       <- ObserveEngine.createTranslator(Site.GS, services, rc, ExecutionEnvironment.Development)
      eng      <- Engine.build[IO](ObserveEngine.onStepComplete[IO](services.odb, tr))
      oe        = new ObserveEngineImpl[IO](eng, services, defaultSettings, tr, rc)
      states   <- SignallingRef.of[IO, Option[EngineState[IO]]](none)
      a        <- oe
                    .eventResultStream(s0)
                    .evalTap(oe.notifyODB(_).void)
                    .evalMap((_, s) => states.set(s.some))
                    .compile
                    .drain
                    .background
                    .use(_ => body(RunningEngine(odb, oe, states)))
    yield a

  private def start(oe: ObserveEngine[IO]): IO[Unit] =
    oe.startSequence(seqObsId1, user, observer, clientId, RunOverride.Override)

  test("a subsystem timeout fails the step without sending a step abort to the ODB") {
    withRunningEngine(gmosFailingConfig): engine =>
      for
        s1  <- engine.sendAndAwait(start)(sequenceStatus(_).exists(isFinished))
        evs <- engine.odb.outCapture
      yield
        assert(sequenceStatus(s1).exists(_.isError), s"status was ${sequenceStatus(s1)}")
        assert(!evs.contains(StepAbort(seqObsId1)), s"step abort was sent: $evs")
  }

  test("pressing play after a transient failure re-runs the same step to completion") {
    Ref
      .of[IO, Boolean](false)
      .flatMap: failed =>
        withRunningEngine(gmosFailingOnce(failed)): engine =>
          for
            s1  <- engine.sendAndAwait(start)(sequenceStatus(_).exists(isFinished))
            s2  <- engine.sendAndAwait(start)(sequenceStatus(_).exists(isFinished))
            evs <- engine.odb.outCapture
          yield
            assert(sequenceStatus(s1).exists(_.isError), s"status was ${sequenceStatus(s1)}")
            assertEquals(sequenceStatus(s2), SequenceStatus.Completed.some)
            assertEquals(evs.count(_ == StepStartStep(seqObsId1)), 2, s"events: $evs")
            assertEquals(evs.count(_ == StepEndStep(seqObsId1)), 1, s"events: $evs")
            assert(!evs.contains(StepAbort(seqObsId1)), s"step abort was sent: $evs")
  }

  test("configuring a subsystem after a failure runs it as a fresh step and records the run") {
    Ref
      .of[IO, Boolean](false)
      .flatMap: failed =>
        withRunningEngine(gmosFailingOnce(failed)): engine =>
          val configure: ObserveEngine[IO] => IO[Unit] =
            _.configureSubsystem(seqObsId1,
                                 observer,
                                 user,
                                 stepId(1),
                                 Instrument.GmosNorth,
                                 clientId
            )
          for
            s1 <- engine.sendAndAwait(start)(sequenceStatus(_).exists(isFinished))
            s2 <- engine.sendAndAwait(configure)(gmosSingleRunState(_).exists(_.completed))
            s3 <- engine.sendAndAwait(start)(sequenceStatus(_).exists(isFinished))
          yield
            assert(sequenceStatus(s1).exists(_.isError), s"status was ${sequenceStatus(s1)}")
            assertEquals(sequenceStatus(s2), SequenceStatus.Idle.some)
            assert(s2.sequences.get(seqObsId1).exists(_.seq.loadedStep.exists(_.done.isEmpty)))
            assertEquals(sequenceStatus(s3), SequenceStatus.Completed.some)
  }

  test("pausing an exposure sends a step pause and no step abort") {
    Deferred[IO, Unit].flatMap: pause =>
      withRunningEngine(gmosPausable(pause, ObserveCommandResult.Stopped.pure[IO])): engine =>
        for
          _   <- engine.sendAndAwait(start)(observeActionState(_).exists(_.started))
          s2  <- engine.sendAndAwait(_.pauseExposure(seqObsId1, observer, user, graceful = false)):
                   observeActionState(_).exists(_.paused)
          evs <- engine.odb.outCapture
        yield
          assert(observeActionState(s2).exists(_.paused))
          assert(evs.contains(StepPause(seqObsId1)), s"step pause was not sent: $evs")
          assert(!evs.contains(StepAbort(seqObsId1)), s"step abort was sent: $evs")
  }

  test("a subsystem cannot be configured while an exposure is paused") {
    Deferred[IO, Unit].flatMap: pause =>
      withRunningEngine(gmosPausable(pause, ObserveCommandResult.Stopped.pure[IO])): engine =>
        val configure: ObserveEngine[IO] => IO[Unit] =
          _.configureSubsystem(seqObsId1, observer, user, stepId(1), Instrument.GmosNorth, clientId)
        for
          _  <- engine.sendAndAwait(start)(observeActionState(_).exists(_.started))
          _  <- engine.sendAndAwait(_.pauseExposure(seqObsId1, observer, user, graceful = false)):
                  observeActionState(_).exists(_.paused)
          s3 <- engine.sendAndAwait(configure)(_ => true)
        yield
          assert(observeActionState(s3).exists(_.paused), "the paused exposure was lost")
          assertEquals(gmosSingleRunState(s3), ActionState.Idle.some)
  }

  test("a timeout while stopping a paused exposure fails the step without a step abort") {
    Deferred[IO, Unit].flatMap: pause =>
      val stopTimeout = IO.raiseError(ObserveFailure.Timeout("GMOS stop-and-wait"))
      withRunningEngine(gmosPausable(pause, stopTimeout)): engine =>
        for
          _   <- engine.sendAndAwait(start)(observeActionState(_).exists(_.started))
          _   <- engine.sendAndAwait(_.pauseExposure(seqObsId1, observer, user, graceful = false)):
                   observeActionState(_).exists(_.paused)
          s3  <- engine.sendAndAwait(_.stopExposure(seqObsId1, observer, user, graceful = false)):
                   sequenceStatus(_).exists(isFinished)
          evs <- engine.odb.outCapture
        yield
          assert(sequenceStatus(s3).exists(_.isError), s"status was ${sequenceStatus(s3)}")
          assert(evs.contains(StepPause(seqObsId1)), s"step pause was not sent: $evs")
          assert(!evs.contains(StepAbort(seqObsId1)), s"step abort was sent: $evs")
  }
}
