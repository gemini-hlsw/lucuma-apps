// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepDigest
import lucuma.core.model.sequence.StepDigests
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.refined.auto.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import observe.common.test.*
import observe.model.CurrentConditions
import observe.model.Notification
import observe.model.Observation
import observe.model.Observer
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.RunOverride
import observe.server.engine.EngineEvent
import observe.server.engine.EventResult
import observe.server.engine.SequenceState
import observe.server.engine.user
import observe.server.odb.OdbProxy
import observe.server.odb.TestOdbProxy
import observe.server.odb.TestOdbProxy.DigestMode
import observe.server.odb.TestOdbProxy.OdbRead

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
  private val scienceConfig: StepConfig = StepConfig.Science

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

  private def isFallback(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(
            _,
            _,
            Some(SeqEvent.NotifyUser(Notification.DigestUnavailable(_, _), _))
          ) =>
        true
      case _ => false

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
    digests:   Map[Observation.Id, DigestMode] = Map.empty
  ): IO[Fixture] =
    for
      odb      <- TestOdbProxy.build[IO](sequences.toMap, digestModes = digests)
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

  private def claimMod(obsId: Observation.Id, claim: Set[Subsystem]): Endo[EngineState[IO]] =
    EngineState.atSequence[IO](obsId).modify(SequenceData.sequenceSubsystems[IO].replace(claim))

  /** Marks a loaded sequence as running, holding `claim`. */
  private def runningWith(obsId: Observation.Id, claim: Set[Subsystem]): Endo[EngineState[IO]] =
    statusMod(obsId)(_ => SequenceStatus.Running.Init).andThen(claimMod(obsId, claim))

  private def stepDigest(count: Int): StepDigest =
    StepDigest(NonNegInt.unsafeFrom(count), CategorizedTime.Zero)

  private def counts(
    bias:    Int = 0,
    dark:    Int = 0,
    arc:     Int = 0,
    flat:    Int = 0,
    science: Int = 0
  ): StepDigests =
    StepDigests(
      stepDigest(bias),
      stepDigest(dark),
      stepDigest(arc),
      stepDigest(flat),
      stepDigest(science)
    )

  private def sequenceDigest(steps: StepDigests): SequenceDigest =
    SequenceDigest.Zero.copy(steps = steps)

  private def digestOf(
    acquisition: StepDigests,
    science:     StepDigests,
    upToDate:    Boolean = true
  ): OdbProxy.Digest =
    CalculatedValue(
      if upToDate then CalculationState.Ready else CalculationState.Pending,
      ExecutionDigest.Zero
        .copy(acquisition = sequenceDigest(acquisition), science = sequenceDigest(science))
        .some
    )

  private def claimOf(instrument: Instrument, digest: OdbProxy.Digest): Set[Subsystem] =
    SequenceSubsystems.fromDigest(instrument, digest.some)

  /** The digest of a dark-only observation, so that it claims its instrument alone. */
  private val DarkOnly: ExecutionDigest =
    ExecutionDigest.Zero.copy(science = sequenceDigest(counts(dark = 1)))

  private def digestReads(log: List[OdbRead], obsId: Observation.Id): Int =
    log.count(_ === OdbRead.Digest(obsId))

  test("a digest with only biases or darks claims the instrument alone"):
    assertEquals(
      claimOf(Instrument.GmosNorth, digestOf(counts(), counts(dark = 3, bias = 1))),
      North
    )
    assertEquals(
      claimOf(Instrument.Flamingos2, digestOf(counts(dark = 1), counts())),
      Set[Subsystem](Instrument.Flamingos2)
    )

  test("a digest with science, flat or arc steps also claims TCS and GCAL"):
    List(counts(science = 1), counts(flat = 1), counts(arc = 1)).foreach: steps =>
      assertEquals(
        claimOf(Instrument.GmosSouth, digestOf(counts(), steps)),
        SouthTcsGcal
      )
      assertEquals(
        claimOf(Instrument.GmosSouth, digestOf(steps, counts(dark = 1))),
        SouthTcsGcal
      )

  test("GHOST biases and darks also claim GCAL"):
    List(counts(bias = 1), counts(dark = 1)).foreach: steps =>
      assertEquals(
        claimOf(Instrument.Ghost, digestOf(counts(), steps)),
        Set[Subsystem](Instrument.Ghost, Gcal)
      )
    assertEquals(
      claimOf(Instrument.Ghost, digestOf(counts(), counts(science = 1))),
      Set[Subsystem](Instrument.Ghost, Tcs, Gcal)
    )

  test("a digest that is not up to date claims the instrument, TCS and GCAL"):
    assertEquals(
      claimOf(Instrument.Flamingos2, digestOf(counts(), counts(dark = 1), upToDate = false)),
      Set[Subsystem](Instrument.Flamingos2, Tcs, Gcal)
    )

  test("a missing digest claims the instrument, TCS and GCAL"):
    assertEquals(
      SequenceSubsystems.fromDigest(Instrument.Igrins2, none),
      Set[Subsystem](Instrument.Igrins2, Tcs, Gcal)
    )
    assertEquals(
      SequenceSubsystems.fromDigest(Instrument.Igrins2,
                                    digestOf(counts(), counts()).copy(value = none).some
      ),
      Set[Subsystem](Instrument.Igrins2, Tcs, Gcal)
    )

  test("the digest claim agrees with the translator"):
    def translated(obs: (Instrument, TestOdbProxy.InstrumentState)): IO[Set[Subsystem]] =
      for
        odb      <- TestOdbProxy.build[IO](Map(obs))
        odbData  <- odb.read(obs._2.obsId)
        services <- defaultServices
        current  <- Ref.of[IO, CurrentConditions](CurrentConditions.Default)
        t        <- SeqTranslate(Site.GS, services, current, ExecutionEnvironment.Development)
        stepGen  <- IO.fromOption(t.nextStep(odbData, SequenceType.Science.asLeft)._2)(
                      new RuntimeException("The translator gave no step")
                    )
      yield stepGen.subsystems + Instrument.GmosNorth
    for
      dark    <- translated(gmosNorthObs(obs1, 1, darkConfig))
      science <- translated(gmosNorthObs(obs2, 1, scienceConfig))
    yield
      assertEquals(claimOf(Instrument.GmosNorth, digestOf(counts(), counts(dark = 1))), dark)
      assertEquals(claimOf(Instrument.GmosNorth, digestOf(counts(), counts(science = 1))), science)

  test("a running sequence blocks a start needing its subsystems and the refusal names them"):
    for
      f  <- fixture(List(gmosNorthObs(obs1, 3, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)))
      _  <- load(f, Instrument.GmosNorth, obs1)
      _  <- load(f, Instrument.GmosSouth, obs2)
      _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
      r  <- f.runner.run(start(obs2), (r, _) => isBusy(r))
      st <- f.runner.drain
    yield
      r._1 match
        case EventResult.UserCommandResponse(_,
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
    for
      f <- fixture(
             List(gmosNorthObs(obs1, 2, darkConfig), gmosSouthObs(obs2, 2, darkConfig))
           )
      _ <- load(f, Instrument.GmosNorth, obs1)
      _ <- load(f, Instrument.GmosSouth, obs2)
      _ <- f.runner.modify(runningWith(obs1, North))
      r <- f.runner.run(start(obs2), (r, _) => isExecuting(obs2)(r) || isBusy(r))
    yield
      assert(isExecuting(obs2)(r._1), s"Expected the second sequence to run: ${r._1}")
      assertEquals(sequenceOf(r._2, obs2).sequenceSubsystems, South)
      assertEquals(ObserveEngine.subsystemsInUse(r._2), South ++ North)

  test("a sequence started alone claims what its digest says and keeps it while it runs"):
    for
      f      <- fixture(List(gmosNorthObs(obs1, 3, scienceConfig)))
      _      <- load(f, Instrument.GmosNorth, obs1)
      seen   <- Ref.of[IO, List[Set[Subsystem]]](Nil)
      tap     = (_: EventResult, st: EngineState[IO]) =>
                  seen.update: claims =>
                    st.sequences
                      .get(obs1)
                      .filter(_.seq.status.isRunning)
                      .fold(claims)(s => claims :+ s.sequenceSubsystems)
      _      <- f.runner.run(start(obs1), (r, _) => isComplete(obs1)(r), tap)
      claims <- seen.get
    yield
      assert(claims.nonEmpty)
      assert(claims.forall(_ === NorthTcsGcal), s"Claims while running: $claims")

  test("a running sequence keeps its claim between two steps"):
    for
      f  <- fixture(
              List(gmosNorthObs(obs1, 3, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig))
            )
      _  <- load(f, Instrument.GmosNorth, obs1)
      _  <- load(f, Instrument.GmosSouth, obs2)
      // Running with no step loaded is what the gap between two steps looks like.
      _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
      s0 <- f.runner.state.get
      r  <- f.runner.run(start(obs2), (r, _) => isBusy(r))
    yield
      assert(sequenceOf(s0, obs1).loadedStep.isEmpty)
      assertEquals(ObserveEngine.subsystemsInUse(s0), NorthTcsGcal)
      assert(isBusy(r._1))

  test("a held sequence releases its claim and the next start reads the digest again"):
    for
      f   <- fixture(
               List(gmosNorthObs(obs1, 2, darkConfig)),
               Map(obs1 -> DigestMode.UpToDate)
             )
      _   <- load(f, Instrument.GmosNorth, obs1)
      // An idle sequence left holding the claim of its previous run.
      _   <- f.runner.modify(claimMod(obs1, NorthTcsGcal))
      s0  <- f.runner.state.get
      r   <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r))
      log <- f.odb.readLog
    yield
      assertEquals(ObserveEngine.subsystemsInUse(s0), NoSubsystems)
      assert(isExecuting(obs1)(r._1))
      assertEquals(digestReads(log, obs1), 1)
      assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, North)

  test("a sequence waiting at the prompt keeps what it holds"):
    for
      f  <- fixture(List(gmosNorthObs(obs1, 1, scienceConfig)))
      _  <- load(f, Instrument.GmosNorth, obs1)
      _  <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
      _  <- f.runner.modify(statusMod(obs1)(_.withWaitingUserPrompt(true)))
      st <- f.runner.state.get
    yield assertEquals(ObserveEngine.subsystemsInUse(st), NorthTcsGcal)

  test("proceeding from the prompt runs with what it already holds, without reading the digest"):
    for
      f   <- fixture(List(gmosNorthObs(obs1, 2, scienceConfig)))
      _   <- load(f, Instrument.GmosNorth, obs1)
      _   <- f.runner.modify(
               runningWith(obs1, NorthTcsGcal).andThen(statusMod(obs1)(_.withWaitingUserPrompt(true)))
             )
      n   <- f.odb.readLog.map(_.length)
      r   <- f.runner.run(proceed(obs1), (r, _) => isExecuting(obs1)(r) || isBusy(r))
      log <- f.odb.readLog
    yield
      assert(isExecuting(obs1)(r._1), s"Expected the sequence to run: ${r._1}")
      assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)
      assertEquals(digestReads(log.drop(n), obs1), 0)

  test("the digest is read before the step at start"):
    for
      f   <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)))
      _   <- load(f, Instrument.GmosNorth, obs1)
      n   <- f.odb.readLog.map(_.length)
      _   <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r))
      log <- f.odb.readLog
    yield
      val reads: List[OdbRead] = log.drop(n)
      assertEquals(reads.headOption, OdbRead.Digest(obs1).some)
      assert(reads.contains(OdbRead.Step(obs1)))

  test("a digest that becomes up to date while polling is used"):
    TestControl.executeEmbed:
      for
        f   <- fixture(
                 List(gmosNorthObs(obs1, 2, darkConfig)),
                 Map(obs1 -> DigestMode.UpToDateAfter(1))
               )
        _   <- load(f, Instrument.GmosNorth, obs1)
        r   <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r))
        log <- f.odb.readLog
      yield
        assertEquals(digestReads(log, obs1), 2)
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, North)

  test("a digest read that fails is tried again"):
    TestControl.executeEmbed:
      for
        f        <- fixture(
                      List(gmosNorthObs(obs1, 2, darkConfig)),
                      Map(obs1 -> DigestMode.FailingFirst(2))
                    )
        _        <- load(f, Instrument.GmosNorth, obs1)
        (n, tap) <- countingFallbacks
        r        <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r), tap)
        log      <- f.odb.readLog
        notified <- n.get
      yield
        assertEquals(digestReads(log, obs1), 3)
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, North)
        assertEquals(notified, 0)

  /** Counts the claim fallback notifications of a run. */
  private def countingFallbacks: IO[(Ref[IO, Int], (EventResult, EngineState[IO]) => IO[Unit])] =
    Ref
      .of[IO, Int](0)
      .map: count =>
        (count, (r, _) => count.update(_ + 1).whenA(isFallback(r)))

  test(
    "a digest that never gets up to date is polled until the max wait, then claims conservatively"
  ):
    TestControl.executeEmbed:
      for
        f        <- fixture(
                      List(gmosNorthObs(obs1, 2, darkConfig)),
                      Map(obs1 -> DigestMode.Stale)
                    )
        _        <- load(f, Instrument.GmosNorth, obs1)
        (n, tap) <- countingFallbacks
        r        <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r), tap)
        log      <- f.odb.readLog
        notified <- n.get
      yield
        // The last read and the time limit fall on the same instant.
        val reads: Int =
          (SequenceSubsystems.DigestMaxWait.toMillis / SequenceSubsystems.DigestPollInterval.toMillis).toInt
        assert(Set(reads, reads + 1).contains(digestReads(log, obs1)))
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)
        assertEquals(notified, 1)

  test("a digest read that hangs is given up at the max wait"):
    TestControl.executeEmbed:
      for
        f        <- fixture(
                      List(gmosNorthObs(obs1, 2, darkConfig)),
                      Map(obs1 -> DigestMode.Hanging)
                    )
        _        <- load(f, Instrument.GmosNorth, obs1)
        (n, tap) <- countingFallbacks
        r        <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r), tap)
        log      <- f.odb.readLog
        notified <- n.get
      yield
        assertEquals(digestReads(log, obs1), 1)
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)
        assertEquals(notified, 1)

  private def isStartPending(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.StartPending(_))) => true
      case _                                                                     => false

  /** The commands refused because the sequence is starting, as (observation, action). */
  private def startingRefusal(result: EventResult): Option[(Observation.Id, String)] =
    result match
      case EventResult.UserCommandResponse(
            _,
            _,
            Some(SeqEvent.NotifyUser(Notification.SequenceStarting(obsId, action), _))
          ) =>
        (obsId, action).some
      case _ => none

  /** Calls `f` once, when the start of a sequence becomes pending. */
  private def onStartPending(runner: Runner)(
    f: ObserveEngine[IO] => IO[Unit]
  ): IO[(EventResult, EngineState[IO]) => IO[Unit]] =
    Ref
      .of[IO, Boolean](false)
      .map: done =>
        (result, _) =>
          done.getAndSet(true).flatMap(f(runner.oe).unlessA).whenA(isStartPending(result))

  test(
    "while a start waits for the digest the sequence holds nothing and refuses commands"
  ):
    TestControl.executeEmbed:
      for
        f        <- fixture(
                      List(gmosNorthObs(obs1, 2, scienceConfig)),
                      Map(obs1 -> DigestMode.Hanging)
                    )
        _        <- load(f, Instrument.GmosNorth, obs1)
        refused  <- Ref.of[IO, List[(Observation.Id, String)]](Nil)
        pending  <- Ref.of[IO, Option[EngineState[IO]]](none)
        commands <- onStartPending(f.runner): oe =>
                      start(obs1)(oe) >> oe.unloadSequence(obs1, user, clientId)
        r        <- f.runner.run(
                      start(obs1),
                      (r, _) => isExecuting(obs1)(r),
                      (r, st) =>
                        pending.update(_.orElse(st.some)).whenA(isStartPending(r)) >>
                          refused.update(_ ++ startingRefusal(r).toList) >>
                          commands(r, st)
                    )
        waiting  <- pending.get
        refusals <- refused.get
      yield
        val st: EngineState[IO] = waiting.getOrElse(fail("The start never became pending"))
        assert(sequenceOf(st, obs1).seq.status.isStartPending)
        assertEquals(ObserveEngine.subsystemsInUse(st), NoSubsystems)
        assertEquals(refusals.toSet, Set(obs1 -> "start", obs1 -> "unload"))
        assert(sequenceOf(r._2, obs1).seq.status.isRunning)
        assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)

  test("other sequences run while a start waits for the digest"):
    TestControl.executeEmbed:
      for
        f     <- fixture(
                   List(gmosNorthObs(obs1, 1, darkConfig), gmosSouthObs(obs2, 1, darkConfig)),
                   Map(obs1 -> DigestMode.Hanging)
                 )
        _     <- load(f, Instrument.GmosNorth, obs1)
        _     <- load(f, Instrument.GmosSouth, obs2)
        order <- Ref.of[IO, List[Observation.Id]](Nil)
        other <- onStartPending(f.runner)(start(obs2))
        _     <- f.runner.run(
                   start(obs1),
                   (r, _) => isExecuting(obs1)(r),
                   (r, st) =>
                     order.update(_ ++ List(obs1, obs2).filter(id => isExecuting(id)(r))) >>
                       other(r, st)
                 )
        seen  <- order.get
      yield assertEquals(seen.headOption, obs2.some)

  test(
    "an unreadable or missing digest does not stop the start, claims conservatively and notifies"
  ):
    TestControl.executeEmbed:
      List(DigestMode.Failing, DigestMode.Missing).traverse: mode =>
        for
          f        <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)), Map(obs1 -> mode))
          _        <- load(f, Instrument.GmosNorth, obs1)
          (n, tap) <- countingFallbacks
          r        <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r) || isBusy(r), tap)
          notified <- n.get
        yield
          assert(isExecuting(obs1)(r._1), s"Expected the sequence to run with $mode: ${r._1}")
          assertEquals(sequenceOf(r._2, obs1).sequenceSubsystems, NorthTcsGcal)
          assertEquals(notified, 1)

  test("an up to date digest sends no fallback notification"):
    for
      f        <- fixture(List(gmosNorthObs(obs1, 2, darkConfig)))
      _        <- load(f, Instrument.GmosNorth, obs1)
      (n, tap) <- countingFallbacks
      _        <- f.runner.run(start(obs1), (r, _) => isExecuting(obs1)(r), tap)
      _        <- f.runner.drain
      notified <- n.get
    yield assertEquals(notified, 0)

  test("the fallback notification is sent even when the start is refused"):
    TestControl.executeEmbed:
      for
        f        <- fixture(
                      List(gmosNorthObs(obs1, 3, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)),
                      Map(obs2 -> DigestMode.Missing)
                    )
        _        <- load(f, Instrument.GmosNorth, obs1)
        _        <- load(f, Instrument.GmosSouth, obs2)
        _        <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
        (n, tap) <- countingFallbacks
        r        <- f.runner.run(start(obs2), (r, _) => isBusy(r), tap)
        notified <- n.get
      yield
        assert(isBusy(r._1))
        assertEquals(notified, 1)

  test("a newly loaded sequence has an empty claim"):
    for
      f  <- fixture(List(gmosNorthObs(obs1, 1, darkConfig)))
      _  <- load(f, Instrument.GmosNorth, obs1)
      s0 <- f.runner.state.get
      _  <- f.runner.modify(claimMod(obs1, NorthTcsGcal))
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

  /** Marks a sequence as running with a loaded step needing `subsystems`, holding `claim`. */
  private def runningWithStep(
    obsId:      Observation.Id,
    instrument: Instrument,
    subsystems: Set[Subsystem],
    claim:      Set[Subsystem]
  ): Endo[EngineState[IO]] =
    loadSequenceWithSubsystems(obsId, subsystems, EngineState.instrumentLoaded(instrument))
      .andThen(runningWith(obsId, claim))

  test("a step clashes with the loaded steps and claims of the other running sequences"):
    val gcal: Set[Subsystem]     = Set(Gcal)
    val north: Set[Subsystem]    = Set(Instrument.GmosNorth, Gcal)
    val south: Set[Subsystem]    = Set(Instrument.GmosSouth, Gcal)
    val both: EngineState[IO]    =
      runningWithStep(obs1, Instrument.GmosNorth, north, North)
        .andThen(runningWithStep(obs2, Instrument.GmosSouth, south, South))
        .apply(EngineState.default[IO])
    val held: EngineState[IO]    = statusMod(obs2)(_ => SequenceStatus.Idle)(both)
    val prompt: EngineState[IO]  = statusMod(obs2)(_.withWaitingUserPrompt(true))(both)
    val claimed: EngineState[IO] =
      claimMod(obs2, South ++ gcal)
        .andThen(
          EngineState.sequenceStateAt[IO](obs2).andThen(SequenceState.loadedStep[IO]).replace(none)
        )
        .apply(both)
    assertEquals(ObserveEngine.stepClash(both, obs1), gcal)
    assertEquals(ObserveEngine.stepClash(both, obs2), gcal)
    assertEquals(ObserveEngine.stepClash(held, obs1), NoSubsystems)
    assertEquals(ObserveEngine.stepClash(prompt, obs1), gcal)
    assertEquals(ObserveEngine.stepClash(claimed, obs1), gcal)

  test("a sequence reaching a step outside its claim that another holds is held, never run"):
    // obs2 is claimed as dark-only but has a science step after its dark one.
    for
      f       <- fixture(
                   List(gmosNorthObs(obs1, 6, scienceConfig), gmosSouthDarkThenScience(obs2)),
                   Map(obs2 -> DigestMode.Overridden(DarkOnly))
                 )
      _       <- load(f, Instrument.GmosNorth, obs1)
      _       <- load(f, Instrument.GmosSouth, obs2)
      tracker <- Ref.of[IO, Int](0)
      tap      = (_: EventResult, st: EngineState[IO]) => tracker.update(_.max(stepOverlap(st)))
      started <- Ref.of[IO, Boolean](false)
      // obs1 starts once obs2 is running its dark step, which claims the instrument alone.
      tapBoth  = (r: EventResult, st: EngineState[IO]) =>
                   tap(r, st) >>
                     started
                       .getAndSet(true)
                       .flatMap(done =>
                         f.runner.oe
                           .startSequence(obs1, user, observer, clientId, RunOverride.Override)
                           .unlessA(done)
                       )
                       .whenA(isExecuting(obs2)(r))
      r       <- f.runner.run(start(obs2), (r, _) => isClash(r) || isBusy(r), tapBoth)
      st      <- f.runner.drain
      overlap <- tracker.get
    yield
      r._1 match
        case EventResult.UserCommandResponse(_,
                                             _,
                                             Some(SeqEvent.SubsystemClash(id, _, subsystems))
            ) =>
          // Either one may reach its step second; that one is held.
          assert(Set(obs1, obs2).contains(id))
          assertEquals(subsystems, TcsAndGcal)
          assertEquals(sequenceOf(st, id).seq.status, SequenceStatus.Idle)
          assert(sequenceOf(st, id).loadedStep.isDefined)
        case other => fail(s"Expected a subsystem clash: $other")
      assertEquals(overlap, 0)

  test("a sequence reaching a step outside its claim that nobody holds grows its claim"):
    for
      f       <- fixture(
                   List(gmosSouthDarkThenScience(obs2)),
                   Map(obs2 -> DigestMode.Overridden(DarkOnly))
                 )
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
    for
      f <- fixture(List(gmosNorthObs(obs1, 2, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)))
      _ <- load(f, Instrument.GmosNorth, obs1)
      _ <- load(f, Instrument.GmosSouth, obs2)
      _ <- f.runner.modify(runningWith(obs1, NorthTcsGcal))
      r <- f.runner.run(
             _.configureSubsystem(obs2, observer, user, testStepId(200), Tcs, clientId),
             (r, _) =>
               r match
                 case EventResult.UserCommandResponse(_,
                                                      _,
                                                      Some(SeqEvent.SubsystemBusy(_, _, _, _))
                     ) =>
                   true
                 case EventResult
                       .UserCommandResponse(_, _, Some(SeqEvent.StartSubsystemConfig(_, _, _))) =>
                   true
                 case _ => false
           )
    yield r._1 match
      case EventResult.UserCommandResponse(_,
                                           _,
                                           Some(SeqEvent.SubsystemBusy(id, _, subsystem, _))
          ) =>
        assertEquals(id, obs2)
        assertEquals(subsystem, Tcs)
      case other => fail(s"Expected the configuration to be refused: $other")

  test("a pending start is neither idle nor running and cannot be unloaded"):
    val status: SequenceStatus = SequenceStatus.StartPending
    assert(!status.isIdle)
    assert(!status.isRunning)
    assert(!status.canUnload)
    assert(status.isInProcess)
