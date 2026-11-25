// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.refined.auto.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import observe.common.RemainingStepSubquery
import observe.common.test.*
import observe.model.CurrentConditions
import observe.model.Notification
import observe.model.Observation
import observe.model.Observer
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.RunOverride
import observe.server.engine.Action.ActionState
import observe.server.engine.ActionCoordsInSeq
import observe.server.engine.EngineEvent
import observe.server.engine.EventResult
import observe.server.engine.Response
import observe.server.engine.Result
import observe.server.engine.SequenceState
import observe.server.engine.user
import observe.server.odb.OdbProxy
import observe.server.odb.TestOdbProxy
import observe.server.odb.TestOdbProxy.OdbRead
import observe.server.odb.TestOdbProxy.RemainingStepsMode

import java.util.UUID
import scala.concurrent.duration.*

class SequenceSubsystemsSuite extends TestCommon:
  import TestCommon.*

  private val observer: Observer = Observer("Joe".refined)

  private val StepTimeout: FiniteDuration = 30.seconds

  private val DrainTime: FiniteDuration = 300.millis

  private val obs1: Observation.Id = observationId(1)
  private val obs2: Observation.Id = observationId(2)

  private val Tcs: Subsystem  = ObservatorySubsystem.Tcs
  private val Gcal: Subsystem = ObservatorySubsystem.Gcal

  private val North: Set[Subsystem]        = Set[Subsystem](Instrument.GmosNorth)
  private val South: Set[Subsystem]        = Set[Subsystem](Instrument.GmosSouth)
  private val NorthTcsGcal: Set[Subsystem] = North ++ Set(Tcs, Gcal)
  private val SouthTcsGcal: Set[Subsystem] = South ++ Set(Tcs, Gcal)
  private val TcsAndGcal: Set[Subsystem]   = Set(Tcs, Gcal)
  private val NoSubsystems: Set[Subsystem] = Set.empty

  private val darkConfig: StepConfig    = StepConfig.Dark
  private val biasConfig: StepConfig    = StepConfig.Bias
  private val scienceConfig: StepConfig = StepConfig.Science
  private val flatConfig: StepConfig    =
    StepConfig.Gcal(
      StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
      GcalFilter.None,
      GcalDiffuser.Visible,
      GcalShutter.Open
    )
  private val arcConfig: StepConfig     =
    StepConfig.Gcal(
      StepConfig.Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.CuArArc)),
      GcalFilter.None,
      GcalDiffuser.Visible,
      GcalShutter.Closed
    )

  /** Runs the engine against its state, which is kept between calls. */
  private case class Runner(oe: ObserveEngine[IO], state: Ref[IO, EngineState[IO]]):
    def run(
      f:     ObserveEngine[IO] => IO[Unit],
      until: (EventResult, EngineState[IO]) => Boolean,
      tap:   (EventResult, EngineState[IO]) => IO[Unit] = (_, _) => IO.unit
    ): IO[(EventResult, EngineState[IO])] =
      for
        _  <- f(oe)
        s0 <- state.get
        r  <- oe
                .eventResultStream(s0)
                .evalTap((result, st) => tap(result, st))
                .takeThrough((result, st) => !until(result, st))
                .compile
                .lastOrError
                .timeout(StepTimeout)
        _  <- state.set(r._2)
      yield r

    /** Lets whatever is still pending happen. */
    def drain: IO[EngineState[IO]] =
      for
        s0 <- state.get
        r  <- oe.eventResultStream(s0).interruptAfter(DrainTime).compile.last
        s1  = r.map(_._2).getOrElse(s0)
        _  <- state.set(s1)
      yield s1

    def modify(f: Endo[EngineState[IO]]): IO[Unit] = state.update(f)

  private case class Fixture(runner: Runner, odb: TestOdbProxy[IO])

  private def isExecuting(obsId: Observation.Id)(result: EventResult): Boolean =
    result match
      case EventResult.EngineUpdate(EngineEvent.Executing(id), _) => id === obsId
      case _                                                      => false

  private def isComplete(obsId: Observation.Id)(result: EventResult): Boolean =
    result match
      case EventResult.EngineUpdate(EngineEvent.SequenceComplete(id), _) => id === obsId
      case _                                                             => false

  private def isBusy(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.Busy(_, _, _, _))) => true
      case _                                                                      => false

  private def isClash(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.SubsystemClash(_, _, _))) => true
      case _                                                                             => false

  private def isLoaded(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.LoadSequence(_, _))) => true
      case _                                                                        => false

  private def atomId(n: Int): Atom.Id =
    Atom.Id.fromUuid(UUID.nameUUIDFromBytes(s"atom-$n".getBytes))

  private def testStepId(n: Int): Step.Id =
    Step.Id.fromUuid(UUID.nameUUIDFromBytes(s"step-$n".getBytes))

  private val StepConfigChange: StepEstimate =
    StepEstimate.fromMax(List(ConfigChangeEstimate("test", "test", 10.secondTimeSpan)), Nil)

  private def atom[D](n: Int, steps: Int, config: StepConfig, dynamic: D): Atom[D] =
    Atom(
      atomId(n),
      none,
      NonEmptyList.fromListUnsafe(
        List.tabulate(steps): i =>
          Step[D](
            testStepId(n * 100 + i),
            dynamic,
            config,
            telescopeCfg1,
            StepConfigChange,
            ObserveClass.DayCal,
            Breakpoint.Disabled
          )
      )
    )

  /** A GMOS North observation whose science sequence is one atom of `steps` steps. */
  private def gmosNorthObs(
    obsId:  Observation.Id,
    steps:  Int,
    config: StepConfig
  ): (Instrument, TestOdbProxy.InstrumentState) =
    Instrument.GmosNorth -> TestOdbProxy.InstrumentState(
      obsId,
      staticCfg1,
      TestOdbProxy.SequenceState(
        none,
        List(atom[DynamicConfig.GmosNorth](obsId.value.value.toInt, steps, config, dynamicCfg1))
      )
    )

  /** A GMOS South observation whose science sequence is one atom of `steps` steps. */
  private def gmosSouthObs(
    obsId:  Observation.Id,
    steps:  Int,
    config: StepConfig
  ): (Instrument, TestOdbProxy.InstrumentState) =
    Instrument.GmosSouth -> TestOdbProxy.InstrumentState(
      obsId,
      staticCfg2,
      TestOdbProxy.SequenceState(
        none,
        List(atom[DynamicConfig.GmosSouth](obsId.value.value.toInt, steps, config, dynamicCfg2))
      )
    )

  /** A GMOS South observation with a dark and then a science step, in two atoms. */
  private def gmosSouthDarkThenScience(
    obsId: Observation.Id
  ): (Instrument, TestOdbProxy.InstrumentState) =
    val n: Int = obsId.value.value.toInt * 10
    Instrument.GmosSouth -> TestOdbProxy.InstrumentState(
      obsId,
      staticCfg2,
      TestOdbProxy.SequenceState(
        none,
        List(
          atom[DynamicConfig.GmosSouth](n + 1, 1, darkConfig, dynamicCfg2),
          atom[DynamicConfig.GmosSouth](n + 2, 1, scienceConfig, dynamicCfg2)
        )
      )
    )

  private def fixture(
    sequences: List[(Instrument, TestOdbProxy.InstrumentState)],
    modes:     Map[Observation.Id, RemainingStepsMode] = Map.empty
  ): IO[Fixture] =
    for
      odb      <- TestOdbProxy.build[IO](sequences.toMap, remainingStepsModes = modes)
      services <- defaultServices.map(_.copy(odb = odb))
      oe       <-
        ObserveEngine.build(Site.GS, services, defaultSettings, ExecutionEnvironment.Development)
      state    <- Ref.of[IO, EngineState[IO]](EngineState.default[IO])
    yield Fixture(Runner(oe, state), odb)

  private def load(f: Fixture, instrument: Instrument, obsId: Observation.Id): IO[Unit] =
    f.runner
      .run(_.loadSequence(instrument, obsId, observer, user, clientId), (r, _) => isLoaded(r))
      .void

  private def start(obsId: Observation.Id)(oe: ObserveEngine[IO]): IO[Unit] =
    oe.startSequence(obsId, user, observer, clientId, RunOverride.Override)

  private def proceed(obsId: Observation.Id)(oe: ObserveEngine[IO]): IO[Unit] =
    oe.proceedAfterPrompt(obsId,
                          user,
                          observer,
                          SequenceType.Science,
                          clientId,
                          RunOverride.Override
    )

  private def sequenceOf(st: EngineState[IO], obsId: Observation.Id): SequenceData[IO] =
    st.sequences.getOrElse(obsId, fail(s"Observation $obsId is not loaded"))

  private def statusMod(
    obsId: Observation.Id
  )(f: SequenceStatus => SequenceStatus): Endo[EngineState[IO]] =
    EngineState.sequenceStateAt[IO](obsId).andThen(SequenceState.status[IO]).modify(f)

  private def holdMod(obsId: Observation.Id, hold: Set[Subsystem]): Endo[EngineState[IO]] =
    EngineState.atSequence[IO](obsId).modify(SequenceData.sequenceSubsystems[IO].replace(hold))

  /** Marks a loaded sequence as running, holding `hold`. */
  private def runningWith(obsId: Observation.Id, hold: Set[Subsystem]): Endo[EngineState[IO]] =
    statusMod(obsId)(_ => SequenceStatus.Running.Init).andThen(holdMod(obsId, hold))

  private def remainingReads(log: List[OdbRead], obsId: Observation.Id): Int =
    log.count(_ === OdbRead.RemainingSteps(obsId))

  private def step(config: StepConfig, observeClass: ObserveClass): RemainingStepSubquery.Data =
    RemainingStepSubquery.Data(config,
                               observeClass,
                               RemainingStepSubquery.Data.Estimate(TimeSpan.Zero)
    )

  private def holdOf(
    instrument:      Instrument,
    steps:           List[StepConfig],
    isNodAndShuffle: Boolean = false
  ): Set[Subsystem] =
    SequenceSubsystems.fromRemainingSteps(
      instrument,
      OdbProxy.RemainingSteps(isNodAndShuffle, steps.map(step(_, ObserveClass.Science)), none)
    )

  /** The remaining-steps read of a dark-only observation, so that it holds its instrument alone. */
  private val DarkOnly: RemainingStepsMode =
    RemainingStepsMode.Overridden(
      OdbProxy.RemainingSteps(
        false,
        List(step(darkConfig, ObserveClass.DayCal)),
        CalibrationRole.DaytimeCalibration.some
      )
    )

  test("only biases or darks left hold the instrument alone"):
    assertEquals(holdOf(Instrument.GmosNorth, List(darkConfig, biasConfig, darkConfig)), North)
    assertEquals(holdOf(Instrument.Flamingos2, List(biasConfig)),
                 Set[Subsystem](Instrument.Flamingos2)
    )
    assertEquals(holdOf(Instrument.GmosNorth, Nil), North)

  test("any science, flat or arc step left also holds TCS and GCAL"):
    List(scienceConfig, flatConfig, arcConfig).foreach: config =>
      assertEquals(holdOf(Instrument.GmosSouth, List(config)), SouthTcsGcal)

  test("steps further on count, not only the next one"):
    assertEquals(
      holdOf(Instrument.GmosSouth, List(darkConfig, biasConfig, darkConfig, flatConfig)),
      SouthTcsGcal
    )

  test("GHOST biases and darks also hold GCAL"):
    assertEquals(
      holdOf(Instrument.Ghost, List(biasConfig, darkConfig)),
      Set[Subsystem](Instrument.Ghost, Gcal)
    )
    assertEquals(
      holdOf(Instrument.Ghost, List(darkConfig, scienceConfig)),
      Set[Subsystem](Instrument.Ghost, Tcs, Gcal)
    )

  test("GMOS Nod&Shuffle biases and darks also hold GCAL"):
    assertEquals(
      holdOf(Instrument.GmosSouth, List(darkConfig, biasConfig), isNodAndShuffle = true),
      South + Gcal
    )
    assertEquals(
      holdOf(Instrument.GmosSouth, List(darkConfig, scienceConfig), isNodAndShuffle = true),
      SouthTcsGcal
    )

  test("a step whose kind cannot be told holds TCS and GCAL"):
    // Neither smart GCAL nor a Nod&Shuffle flat can be classified.
    assertEquals(
      holdOf(Instrument.GmosNorth, List(StepConfig.SmartGcal(SmartGcalType.Flat))),
      NorthTcsGcal
    )
    assertEquals(
      holdOf(Instrument.GmosNorth, List(flatConfig), isNodAndShuffle = true),
      NorthTcsGcal
    )

  test("the hold of a step agrees with the translator"):
    def translated(config: StepConfig): IO[Set[Subsystem]] =
      val obs: (Instrument, TestOdbProxy.InstrumentState) = gmosNorthObs(obs1, 1, config)
      for
        odb      <- TestOdbProxy.build[IO](Map(obs))
        odbData  <- odb.read(obs1)
        services <- defaultServices
        current  <- Ref.of[IO, CurrentConditions](CurrentConditions.Default)
        t        <- SeqTranslate(Site.GS, services, current, ExecutionEnvironment.Development)
        stepGen  <- IO.fromOption(t.nextStep(odbData, SequenceType.Science.asLeft)._2)(
                      new RuntimeException("The translator gave no step")
                    )
      yield stepGen.subsystems
    List(darkConfig, scienceConfig, flatConfig).traverse_ : config =>
      translated(config).map: subsystems =>
        assertEquals(
          SequenceSubsystems.fromRemainingSteps(
            Instrument.GmosNorth,
            OdbProxy.RemainingSteps(false, List(step(config, ObserveClass.DayCal)), none)
          ),
          subsystems
        )

  test("a running sequence blocks a start needing its subsystems and the refusal names them"):
    TestControl.executeEmbed:
      for
        f  <-
          fixture(List(gmosNorthObs(obs1, 3, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- load(f, Instrument.GmosSouth, obs2)
        _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        r  <- f.runner.run(start(obs2), (r, _) => isBusy(r))
        st <- f.runner.drain
      yield
        r._1 match
          case EventResult.UserCommandResponse(
                _,
                _,
                Some(SeqEvent.Busy(id, _, subsystems, heldBy))
              ) =>
            assertEquals(id, obs2)
            assertEquals(subsystems.toSet, TcsAndGcal)
            assertEquals(heldBy, List(obs1))
          case other => fail(s"Expected a refusal: $other")
        assertEquals(sequenceOf(st, obs2).seq.status, SequenceStatus.Idle)
        assert(sequenceOf(st, obs2).loadedStep.isEmpty)
        assertEquals(sequenceOf(st, obs2).sequenceSubsystems, NoSubsystems)

  test("two dark-only sequences on different instruments run in parallel"):
    TestControl.executeEmbed:
      for
        f <- fixture(List(gmosNorthObs(obs1, 2, darkConfig), gmosSouthObs(obs2, 2, darkConfig)))
        _ <- load(f, Instrument.GmosNorth, obs1)
        _ <- load(f, Instrument.GmosSouth, obs2)
        _ <- f.runner.modify(runningWith(obs1, North))
        r <- f.runner.run(start(obs2), (r, _) => isExecuting(obs2)(r) || isBusy(r))
      yield
        assert(isExecuting(obs2)(r._1), s"Expected the second sequence to run: ${r._1}")
        assertEquals(sequenceOf(r._2, obs2).sequenceSubsystems, South)
        assertEquals(ObserveEngine.subsystemsInUse(r._2), South ++ North)

  test("a sequence holds what its remaining steps need and keeps it while it runs"):
    TestControl.executeEmbed:
      for
        f     <- fixture(List(gmosNorthObs(obs1, 3, scienceConfig)))
        _     <- load(f, Instrument.GmosNorth, obs1)
        seen  <- Ref.of[IO, List[Set[Subsystem]]](Nil)
        tap    = (_: EventResult, st: EngineState[IO]) =>
                   seen.update: holds =>
                     st.sequences
                       .get(obs1)
                       .filter(_.seq.status.isRunning)
                       .fold(holds)(s => holds :+ s.sequenceSubsystems)
        _     <- f.runner.run(start(obs1), (r, _) => isComplete(obs1)(r), tap)
        holds <- seen.get
      yield
        assert(holds.nonEmpty)
        assert(holds.forall(_ === NorthTcsGcal), s"Holds while running: $holds")

  test("a later step that needs TCS and GCAL is held from the start"):
    TestControl.executeEmbed:
      for
        f <- fixture(List(gmosSouthDarkThenScience(obs2)))
        _ <- load(f, Instrument.GmosSouth, obs2)
        r <- f.runner.run(start(obs2), (r, _) => isExecuting(obs2)(r))
      yield
        assertEquals(sequenceOf(r._2, obs2).stepSubsystems, South)
        assertEquals(sequenceOf(r._2, obs2).sequenceSubsystems, SouthTcsGcal)

  test("a running sequence keeps its hold between two steps"):
    TestControl.executeEmbed:
      for
        f  <-
          fixture(List(gmosNorthObs(obs1, 3, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- load(f, Instrument.GmosSouth, obs2)
        // Running with no step loaded is what the gap between two steps looks like.
        _  <- f.runner.modify(
                runningWith(obs1, NorthTcsGcal).andThen(
                  EngineState
                    .sequenceStateAt[IO](obs1)
                    .andThen(SequenceState.loadedStep[IO])
                    .replace(none)
                )
              )
        s0 <- f.runner.state.get
        r  <- f.runner.run(start(obs2), (r, _) => isBusy(r))
      yield
        assert(sequenceOf(s0, obs1).loadedStep.isEmpty)
        assertEquals(ObserveEngine.subsystemsInUse(s0), NorthTcsGcal)
        assert(isBusy(r._1))

  test("a held sequence releases its hold and the next start reads the remaining steps again"):
    TestControl.executeEmbed:
      for
        f   <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)))
        _   <- load(f, Instrument.GmosNorth, obs1)
        // An idle sequence left with the hold of its previous run.
        _   <- f.runner.modify(holdMod(obs1, NorthTcsGcal))
        s0  <- f.runner.state.get
        n   <- f.odb.readLog.map(_.length)
        r   <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r))
        log <- f.odb.readLog
      yield
        assertEquals(ObserveEngine.subsystemsInUse(s0), NoSubsystems)
        assert(isExecuting(obs1)(r._1))
        assertEquals(remainingReads(log.drop(n), obs1), 1)
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, North)

  test("a sequence waiting at the prompt keeps what it holds"):
    TestControl.executeEmbed:
      for
        f  <- fixture(List(gmosNorthObs(obs1, 1, scienceConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        _  <- f.runner.modify(statusMod(obs1)(_.withWaitingUserPrompt(true)))
        st <- f.runner.state.get
      yield assertEquals(ObserveEngine.subsystemsInUse(st), NorthTcsGcal)

  test("proceeding from the prompt runs with what it already holds, without reading again"):
    TestControl.executeEmbed:
      for
        f   <- fixture(List(gmosNorthObs(obs1, 2, scienceConfig)))
        _   <- load(f, Instrument.GmosNorth, obs1)
        _   <- f.runner.modify(
                 runningWith(obs1, NorthTcsGcal).andThen(
                   statusMod(obs1)(_.withWaitingUserPrompt(true))
                 )
               )
        n   <- f.odb.readLog.map(_.length)
        r   <- f.runner.run(proceed(obs1), (r, _) => isExecuting(obs1)(r) || isClash(r))
        log <- f.odb.readLog
      yield
        assert(isExecuting(obs1)(r._1), s"Expected the sequence to run: ${r._1}")
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)
        assertEquals(remainingReads(log.drop(n), obs1), 0)
        assert(log.drop(n).contains(OdbRead.Step(obs1)))

  test("the remaining steps are read before the step at start"):
    TestControl.executeEmbed:
      for
        f   <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)))
        _   <- load(f, Instrument.GmosNorth, obs1)
        n   <- f.odb.readLog.map(_.length)
        _   <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r))
        log <- f.odb.readLog
      yield assertEquals(log.drop(n), List(OdbRead.RemainingSteps(obs1), OdbRead.Step(obs1)))

  test("a failed remaining-steps read refuses the start and leaves the sequence idle"):
    TestControl.executeEmbed:
      for
        f      <- fixture(
                    List(gmosNorthObs(obs1, 2, scienceConfig)),
                    Map(obs1 -> RemainingStepsMode.Failing)
                  )
        _      <- load(f, Instrument.GmosNorth, obs1)
        n      <- f.odb.readLog.map(_.length)
        sent   <- f.odb.outCapture.map(_.length)
        r      <- f.runner.run(start(obs1), (r, _) => refusal(r).isDefined)
        st     <- f.runner.drain
        log    <- f.odb.readLog
        events <- f.odb.outCapture
      yield
        assertStillRunning(r._1, "start the sequence")
        assertEquals(log.drop(n), List(OdbRead.RemainingSteps(obs1)))
        assert(sequenceOf(st, obs1).seq.status.isIdle, s"${sequenceOf(st, obs1).seq.status}")
        assertEquals(sequenceOf(st, obs1).sequenceSubsystems, NoSubsystems)
        assertEquals(ObserveEngine.subsystemsInUse(st), NoSubsystems)
        assertEquals(events.drop(sent), Nil)

  test("a newly loaded sequence holds nothing"):
    TestControl.executeEmbed:
      for
        f  <- fixture(List(gmosNorthObs(obs1, 1, darkConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        s0 <- f.runner.state.get
        _  <- f.runner.modify(holdMod(obs1, NorthTcsGcal))
        _  <- f.runner.run(
                _.unloadSequence(obs1, user, clientId),
                (_, st) => !st.sequences.contains(obs1)
              )
        _  <- load(f, Instrument.GmosNorth, obs1)
        s1 <- f.runner.state.get
      yield
        assertEquals(sequenceOf(s0, obs1).sequenceSubsystems, NoSubsystems)
        assertEquals(sequenceOf(s1, obs1).sequenceSubsystems, NoSubsystems)

  /** The step loaded in a running sequence waiting for nothing. */
  private def loadedStepSubsystems(s: SequenceData[IO]): Set[Subsystem] =
    if s.seq.status.isRunning && !s.seq.status.isWaitingUserPrompt then s.stepSubsystems
    else Set.empty

  /** 1 if two running sequences have loaded steps sharing a subsystem. */
  private def stepOverlap(st: EngineState[IO]): Int =
    val steps: List[(Observation.Id, Set[Subsystem])] =
      st.sequences.values.toList.map(s => (s.obsId, loadedStepSubsystems(s)))
    val overlapping: Boolean                          =
      steps.exists((id, ss) =>
        steps.exists((other, os) => id =!= other && ss.intersect(os).nonEmpty)
      )
    if overlapping then 1 else 0

  /** Marks a sequence as running with a loaded step needing `subsystems`, holding `hold`. */
  private def runningWithStep(
    obsId:      Observation.Id,
    instrument: Instrument,
    subsystems: Set[Subsystem],
    hold:       Set[Subsystem]
  ): Endo[EngineState[IO]] =
    loadSequenceWithSubsystems(obsId, subsystems, EngineState.instrumentLoaded(instrument))
      .andThen(runningWith(obsId, hold))

  test("a step clashes with the loaded steps and holds of the other running sequences"):
    val gcal: Set[Subsystem]     = Set(Gcal)
    val north: Set[Subsystem]    = Set(Instrument.GmosNorth, Gcal)
    val south: Set[Subsystem]    = Set(Instrument.GmosSouth, Gcal)
    val both: EngineState[IO]    =
      runningWithStep(obs1, Instrument.GmosNorth, north, North)
        .andThen(runningWithStep(obs2, Instrument.GmosSouth, south, South))
        .apply(EngineState.default[IO])
    val held: EngineState[IO]    = statusMod(obs2)(_ => SequenceStatus.Idle)(both)
    val prompt: EngineState[IO]  = statusMod(obs2)(_.withWaitingUserPrompt(true))(both)
    val holding: EngineState[IO] =
      holdMod(obs2, South ++ gcal)
        .andThen(
          EngineState.sequenceStateAt[IO](obs2).andThen(SequenceState.loadedStep[IO]).replace(none)
        )
        .apply(both)
    assertEquals(ObserveEngine.stepClash(both, obs1), gcal)
    assertEquals(ObserveEngine.stepClash(both, obs2), gcal)
    assertEquals(ObserveEngine.stepClash(held, obs1), NoSubsystems)
    assertEquals(ObserveEngine.stepClash(prompt, obs1), gcal)
    assertEquals(ObserveEngine.stepClash(holding, obs1), gcal)

  test("a sequence reaching a step beyond its hold that another holds is held, never run"):
    // obs2's read says dark-only, but a science step follows its dark one.
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(gmosNorthObs(obs1, 2, scienceConfig), gmosSouthDarkThenScience(obs2)),
                Map(obs2 -> DarkOnly)
              )
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- load(f, Instrument.GmosSouth, obs2)
        _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        r  <- f.runner.run(start(obs2), (r, _) => isClash(r) || isBusy(r) || isComplete(obs2)(r))
        st <- f.runner.drain
      yield r._1 match
        case EventResult.UserCommandResponse(
              _,
              _,
              Some(SeqEvent.SubsystemClash(id, stepId, subsystems))
            ) =>
          assertEquals(id, obs2)
          assertEquals(subsystems, TcsAndGcal)
          assertEquals(sequenceOf(st, obs2).seq.status, SequenceStatus.Idle)
          assertEquals(sequenceOf(st, obs2).loadedStep.map(_.id), stepId.some)
          assertEquals(ObserveEngine.subsystemsInUse(st), NorthTcsGcal)
        case other => fail(s"Expected a subsystem clash: $other")

  test("two sequences racing for TCS and GCAL never run overlapping steps"):
    // obs2's read says dark-only, but a science step follows its dark one.
    TestControl.executeEmbed:
      for
        f       <- fixture(
                     List(gmosNorthObs(obs1, 6, scienceConfig), gmosSouthDarkThenScience(obs2)),
                     Map(obs2 -> DarkOnly)
                   )
        _       <- load(f, Instrument.GmosNorth, obs1)
        _       <- load(f, Instrument.GmosSouth, obs2)
        tracker <- Ref.of[IO, Int](0)
        tap      = (_: EventResult, st: EngineState[IO]) => tracker.update(_.max(stepOverlap(st)))
        started <- Ref.of[IO, Boolean](false)
        // obs1 starts once obs2 is running its dark step, which holds the instrument alone.
        tapBoth  = (r: EventResult, st: EngineState[IO]) =>
                     tap(r, st) >>
                       started
                         .getAndSet(true)
                         .flatMap(done => start(obs1)(f.runner.oe).unlessA(done))
                         .whenA(isExecuting(obs2)(r))
        bothDone = (st: EngineState[IO]) => st.sequences.values.forall(_.seq.status.isCompleted)
        r       <- f.runner.run(
                     start(obs2),
                     (r, st) => isClash(r) || isBusy(r) || bothDone(st),
                     tapBoth
                   )
        st      <- f.runner.drain
        overlap <- tracker.get
      yield
        r._1 match
          // Either one may reach its step second; that one is held.
          case EventResult.UserCommandResponse(
                _,
                _,
                Some(SeqEvent.SubsystemClash(id, _, subsystems))
              ) =>
            assertEquals(subsystems, TcsAndGcal)
            assertEquals(sequenceOf(st, id).seq.status, SequenceStatus.Idle)
            assert(sequenceOf(st, id).loadedStep.isDefined)
          case _ => ()
        assertEquals(overlap, 0)

  test("a sequence reaching a step beyond its hold that nobody holds grows its hold"):
    TestControl.executeEmbed:
      for
        f       <- fixture(List(gmosSouthDarkThenScience(obs2)), Map(obs2 -> DarkOnly))
        _       <- load(f, Instrument.GmosSouth, obs2)
        first   <- Ref.of[IO, Option[Set[Subsystem]]](none)
        tap      = (r: EventResult, st: EngineState[IO]) =>
                     first
                       .update(_.orElse(st.sequences.get(obs2).map(_.sequenceSubsystems)))
                       .whenA(isExecuting(obs2)(r))
        r       <- f.runner.run(start(obs2), (r, _) => isComplete(obs2)(r), tap)
        initial <- first.get
      yield
        assertEquals(initial, South.some)
        assertEquals(sequenceOf(r._2, obs2).sequenceSubsystems, SouthTcsGcal)

  test("a sequence that is held, failed, aborted or completed holds nothing"):
    TestControl.executeEmbed:
      for
        f  <- fixture(List(gmosNorthObs(obs1, 1, scienceConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        st <- f.runner.state.get
      yield
        val stopped: List[SequenceStatus] =
          List(
            SequenceStatus.Idle,
            SequenceStatus.Failed("error"),
            SequenceStatus.Aborted,
            SequenceStatus.Completed
          )
        stopped.foreach: status =>
          assertEquals(
            ObserveEngine.subsystemsInUse(statusMod(obs1)(_ => status)(st)),
            NoSubsystems,
            s"$status"
          )

  test("a subsystem held by a running sequence cannot be configured by hand"):
    TestControl.executeEmbed:
      for
        f <-
          fixture(List(gmosNorthObs(obs1, 2, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)))
        _ <- load(f, Instrument.GmosNorth, obs1)
        _ <- load(f, Instrument.GmosSouth, obs2)
        _ <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        r <- f.runner.run(
               _.configureSubsystem(obs2, observer, user, testStepId(200), Tcs, clientId),
               (r, _) =>
                 r match
                   case EventResult.UserCommandResponse(
                         _,
                         _,
                         Some(SeqEvent.SubsystemBusy(_, _, _, _))
                       ) =>
                     true
                   case EventResult.UserCommandResponse(
                         _,
                         _,
                         Some(SeqEvent.StartSubsystemConfig(_, _, _))
                       ) =>
                     true
                   case _ => false
             )
      yield r._1 match
        case EventResult.UserCommandResponse(
              _,
              _,
              Some(SeqEvent.SubsystemBusy(id, _, subsystem, _))
            ) =>
          assertEquals(id, obs2)
          assertEquals(subsystem, Tcs)
        case other => fail(s"Expected the configuration to be refused: $other")

  test("a subsystem of a running sequence cannot be configured, and its step is left alone"):
    TestControl.executeEmbed:
      for
        f  <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)))
        _  <- load(f, Instrument.GmosNorth, obs1)
        _  <- f.runner.modify(runningWith(obs1, North))
        s0 <- f.runner.state.get
        r  <- f.runner.run(
                _.configureSubsystem(obs1, observer, user, testStepId(101), Gcal, clientId),
                (r, _) =>
                  r match
                    case EventResult.UserCommandResponse(_, _, Some(SeqEvent.NotifyUser(_, _))) =>
                      true
                    case EventResult.UserCommandResponse(
                          _,
                          _,
                          Some(SeqEvent.StartSubsystemConfig(_, _, _))
                        ) =>
                      true
                    case EventResult.UserCommandResponse(
                          _,
                          _,
                          Some(SeqEvent.SubsystemBusy(_, _, _, _))
                        ) =>
                      true
                    case _                                                                      => false
              )
      yield
        r._1 match
          case EventResult.UserCommandResponse(
                _,
                _,
                Some(SeqEvent.NotifyUser(Notification.SequenceNotIdle(id, _), _))
              ) =>
            assertEquals(id, obs1)
          case other => fail(s"Expected the configuration to be refused: $other")
        assertEquals(
          sequenceOf(r._2, obs1).loadedStep.map(_.id),
          sequenceOf(s0, obs1).loadedStep.map(_.id)
        )
        assert(sequenceOf(r._2, obs1).seq.status.isRunning)

  test("a failed sequence keeps what it holds until the actions of its step have finished"):
    val started: Endo[EngineState[IO]]  =
      EngineState.sequenceStateAt[IO](obs1).modify(_.start(0))
    val finished: Endo[EngineState[IO]] =
      EngineState
        .sequenceStateAt[IO](obs1)
        .modify(_.mark(0)(Result.OK(Response.Configured(Instrument.GmosNorth))))
    val failed: EngineState[IO]         =
      runningWithStep(obs1, Instrument.GmosNorth, NorthTcsGcal, NorthTcsGcal)
        .andThen(started)
        .andThen(statusMod(obs1)(_ => SequenceStatus.Failed("error")))
        .apply(EngineState.default[IO])
    assertEquals(ObserveEngine.subsystemsInUse(failed), NorthTcsGcal)
    assertEquals(ObserveEngine.heldByOther(failed, obs2, TcsAndGcal), List(obs1))
    assertEquals(ObserveEngine.subsystemsInUse(finished(failed)), NoSubsystems)

  test("a failed sequence with an action still running does not block a restart"):
    val started: Endo[EngineState[IO]] =
      EngineState.sequenceStateAt[IO](obs1).modify(_.start(0))
    val failed: EngineState[IO]        =
      runningWithStep(obs1, Instrument.GmosNorth, NorthTcsGcal, NorthTcsGcal)
        .andThen(started)
        .andThen(statusMod(obs1)(_ => SequenceStatus.Failed("error")))
        .apply(EngineState.default[IO])
    assertEquals(ObserveEngine.subsystemsInUse(failed), NorthTcsGcal)
    assertEquals(ObserveEngine.restartBlockers(failed), none)

  test("a failed sequence cannot be unloaded while an action of its step is still running"):
    val started: Endo[EngineState[IO]]                         =
      EngineState.sequenceStateAt[IO](obs1).modify(_.start(0))
    val finished: Endo[EngineState[IO]]                        =
      EngineState
        .sequenceStateAt[IO](obs1)
        .modify(_.mark(0)(Result.OK(Response.Configured(Instrument.GmosNorth))))
    val failed: Endo[EngineState[IO]]                          =
      runningWithStep(obs1, Instrument.GmosNorth, NorthTcsGcal, NorthTcsGcal)
        .andThen(started)
        .andThen(statusMod(obs1)(_ => SequenceStatus.Failed("error")))
    def unload(f: Fixture): IO[(EventResult, EngineState[IO])] =
      f.runner.run(
        _.unloadSequence(obs1, user, clientId),
        (r, _) =>
          r match
            case EventResult.UserCommandResponse(_, _, Some(SeqEvent.NotifyUser(_, _)))  => true
            case EventResult.UserCommandResponse(_, _, Some(SeqEvent.UnloadSequence(_))) => true
            case _                                                                       => false
      )
    TestControl.executeEmbed:
      for
        f       <- fixture(Nil)
        _       <- f.runner.modify(failed)
        refused <- unload(f)
        _       <- f.runner.modify(finished)
        done    <- unload(f)
      yield
        assert(refused._2.sequences.contains(obs1), "It was unloaded with an action still running")
        assertEquals(ObserveEngine.subsystemsInUse(refused._2), NorthTcsGcal)
        assert(!done._2.sequences.contains(obs1))

  private def configCoords(
    st:        EngineState[IO],
    obsId:     Observation.Id,
    subsystem: Subsystem
  ): ActionCoordsInSeq =
    val s: SequenceData[IO] = sequenceOf(st, obsId)
    s.seq.loadedStep
      .flatMap(step => s.configActionCoord(step.id, subsystem))
      .getOrElse(fail(s"The loaded step of $obsId does not configure $subsystem"))

  /** Loads `obsId` and its first step, which only a start or a manual configuration reads. */
  private def loadWithStep(f: Fixture, obsId: Observation.Id): IO[Unit] =
    load(f, Instrument.GmosSouth, obsId) >>
      f.runner
        .run(
          _.configureSubsystem(obsId, observer, user, testStepId(100), Tcs, clientId),
          (_, st) =>
            val runs: List[ActionState] =
              sequenceOf(st, obsId).seq.getSingleActionStates.values.toList
            runs.nonEmpty && !runs.exists(_.started)
        )
        .void

  /** Marks the manual configuration of `subsystem` in the loaded step as running. */
  private def configuring(obsId: Observation.Id, subsystem: Subsystem): Endo[EngineState[IO]] =
    st =>
      EngineState
        .sequenceStateAt[IO](obsId)
        .modify(_.startSingle(configCoords(st, obsId, subsystem)))(st)

  private def configured(obsId: Observation.Id, subsystem: Subsystem): Endo[EngineState[IO]] =
    st =>
      EngineState
        .sequenceStateAt[IO](obsId)
        .modify(
          _.completeSingle(configCoords(st, obsId, subsystem), Response.Configured(subsystem))
        )(
          st
        )

  private def refusal(result: EventResult): Option[Notification] =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.NotifyUser(n, _))) => n.some
      case _                                                                      => none

  private def assertStillRunning(result: EventResult, action: String): Unit =
    refusal(result) match
      case Some(Notification.ActionFailed(id, refused, _)) =>
        assertEquals(id, obs1)
        assertEquals(refused, action)
      case other                                           => fail(s"Expected a refusal to $action: $other")

  test("an observation is not unloaded while one of its subsystems is being configured by hand"):
    def unload(f: Fixture): IO[(EventResult, EngineState[IO])] =
      f.runner.run(
        _.unloadSequence(obs1, user, clientId),
        (r, st) => refusal(r).isDefined || !st.sequences.contains(obs1)
      )
    TestControl.executeEmbed:
      for
        f       <- fixture(List(gmosSouthObs(obs1, 1, scienceConfig)))
        _       <- loadWithStep(f, obs1)
        _       <- f.runner.modify(configuring(obs1, Tcs))
        refused <- unload(f)
        _       <- f.runner.modify(configured(obs1, Tcs))
        done    <- unload(f)
      yield
        assert(refused._2.sequences.contains(obs1), "It was unloaded while configuring TCS")
        assertEquals(ObserveEngine.subsystemsInUse(refused._2), Set(Tcs))
        assert(!done._2.sequences.contains(obs1))

  test("a sequence is not started while one of its subsystems is being configured by hand"):
    TestControl.executeEmbed:
      for
        f <- fixture(List(gmosSouthObs(obs1, 1, scienceConfig)))
        _ <- loadWithStep(f, obs1)
        _ <- f.runner.modify(configuring(obs1, Tcs))
        r <- f.runner.run(
               start(obs1),
               (r, st) => refusal(r).isDefined || ObserveEngine.subsystemsInUse(st) =!= Set(Tcs)
             )
      yield
        assertStillRunning(r._1, "start the sequence")
        assertEquals(ObserveEngine.subsystemsInUse(r._2), Set(Tcs))
        assert(!sequenceOf(r._2, obs1).seq.status.isRunning)

  test("another step is not configured by hand while a subsystem is being configured"):
    def configure(f: Fixture, step: Int): IO[(EventResult, EngineState[IO])] =
      f.runner.run(
        _.configureSubsystem(obs1, observer, user, testStepId(step), Gcal, clientId),
        (r, st) => refusal(r).isDefined || ObserveEngine.subsystemsInUse(st) =!= Set(Tcs)
      )
    TestControl.executeEmbed:
      for
        f        <- fixture(List(gmosSouthObs(obs1, 2, scienceConfig)))
        _        <- loadWithStep(f, obs1)
        _        <- f.runner.modify(configuring(obs1, Tcs))
        refused  <- configure(f, 101)
        sameStep <- configure(f, 100)
      yield
        assertStillRunning(refused._1, "configure a subsystem")
        assertEquals(ObserveEngine.subsystemsInUse(refused._2), Set(Tcs))
        assertEquals(ObserveEngine.subsystemsInUse(sameStep._2), TcsAndGcal)

  test("acquisition is not skipped while a subsystem is being configured by hand"):
    TestControl.executeEmbed:
      for
        f <- fixture(List(gmosSouthObs(obs1, 1, scienceConfig)))
        _ <- loadWithStep(f, obs1)
        _ <- f.runner.modify(configuring(obs1, Tcs))
        r <- f.runner.run(
               _.skipAcquisition(obs1, user, observer, clientId),
               (r, st) =>
                 r match
                   case EventResult.UserCommandResponse(_,
                                                        _,
                                                        Some(SeqEvent.SequenceTypeChanged(_))
                       ) =>
                     true
                   case _ =>
                     refusal(r).isDefined || ObserveEngine.subsystemsInUse(st) =!= Set(Tcs)
             )
      yield
        assertStillRunning(r._1, "skip acquisition")
        assertEquals(ObserveEngine.subsystemsInUse(r._2), Set(Tcs))

  test("a failed sequence is not configured by hand while an action of its step is still running"):
    val failed: Endo[EngineState[IO]] =
      runningWithStep(obs1, Instrument.GmosNorth, North, North)
        .andThen(EngineState.sequenceStateAt[IO](obs1).modify(_.start(0)))
        .andThen(statusMod(obs1)(_ => SequenceStatus.Failed("error")))
    TestControl.executeEmbed:
      for
        f <- fixture(Nil)
        _ <- f.runner.modify(failed)
        r <- f.runner.run(
               _.configureSubsystem(obs1, observer, user, testStepId(100), Gcal, clientId),
               (r, st) => refusal(r).isDefined || ObserveEngine.subsystemsInUse(st) =!= North
             )
      yield
        assertStillRunning(r._1, "configure a subsystem")
        assertEquals(ObserveEngine.subsystemsInUse(r._2), North)
