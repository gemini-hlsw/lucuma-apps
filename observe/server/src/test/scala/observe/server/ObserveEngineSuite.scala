// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import coulomb.integrations.cats.all.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.refined.auto.*
import lucuma.core.refined.given
import lucuma.schemas.model.ModeSignalToNoise
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as ODBObservation
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment.FirstScienceTarget
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment.GuideEnvironment
import observe.common.test.*
import observe.model
import observe.model.ClientId
import observe.model.Conditions
import observe.model.Observer
import observe.model.Operator
import observe.model.SequenceStatus
import observe.model.UserPrompt
import observe.model.enums.Resource.Gcal
import observe.model.enums.Resource.TCS
import observe.model.enums.RunOverride
import observe.server.SeqEvent.RequestConfirmation
import observe.server.engine.DummyExecutionZipper
import observe.server.engine.DummyStepGen
import observe.server.engine.EngineHandle
import observe.server.engine.Event
import observe.server.engine.EventResult
import observe.server.engine.EventResult.Outcome
import observe.server.engine.EventResult.SystemUpdate
import observe.server.engine.LoadedStep
import observe.server.engine.SequenceState
import observe.server.engine.SystemEvent
import observe.server.engine.user
import observe.server.odb.OdbObservationData
import observe.server.odb.TestOdbProxy
import observe.server.odb.TestOdbProxy.StepStartStep
import observe.server.tcs.DummyTargetKeywordsReader
import observe.server.tcs.DummyTcsKeywordsReader
import observe.server.tcs.TargetKeywordsReader

import java.util.UUID

class ObserveEngineSuite extends TestCommon {

  import TestCommon.*

  private val clientId: model.ClientId.Type = ClientId(UUID.randomUUID())
  private val observer: Observer            = Observer("Joe".refined)
  private val operator: Operator            = Operator("Joe".refined)

  private case class EngineObserver[F[_]: Async](
    oe:           ObserveEngine[F],
    initialState: EngineState[F] = EngineState.default[F]
  ) {
    private val state: Ref[F, EngineState[F]] = Ref.unsafe(initialState)

    def executeAndWait(
      f:     ObserveEngine[F] => F[Unit],
      until: PartialFunction[(EventResult, EngineState[F]), Boolean],
      tap:   PartialFunction[(EventResult, EngineState[F]), F[Unit]] = PartialFunction.empty
    ): F[EngineState[F]] =
      for
        _ <- f(oe)
        s <- state.get
        r <- oe.eventResultStream(s)
               //  .evalTap(r => cats.effect.Sync[F].delay(println(s"**** ${r._1}"))) // Uncomment for debugging
               .evalTap((r, s) => tap.lift((r, s)).getOrElse(Async[F].unit))
               .takeThrough(r => !until.lift(r).getOrElse(false))
               .compile
               .last
        s1 = r.get._2
        _ <- state.set(s1)
      yield s1

    def executeAndWaitResult(
      f:     ObserveEngine[F] => F[Unit],
      until: PartialFunction[EventResult, Boolean],
      tap:   PartialFunction[(EventResult, EngineState[F]), F[Unit]] = PartialFunction.empty
    ): F[EngineState[F]] =
      executeAndWait(f, r => until.lift(r._1).getOrElse(false), tap)

    def executeAndWaitState(
      f:     ObserveEngine[F] => F[Unit],
      until: PartialFunction[EngineState[F], Boolean],
      tap:   PartialFunction[(EventResult, EngineState[F]), F[Unit]] = PartialFunction.empty
    ): F[EngineState[F]] =
      executeAndWait(f, r => until.lift(r._2).getOrElse(false), tap)
  }

  test("ObserveEngine setOperator should set operator's name") {
    val s0 = EngineState.default[IO]
    (for {
      oe <- observeEngine
      sf <- advanceN(oe, s0, oe.setOperator(user, operator), 2)
    } yield sf.flatMap(EngineState.operator.get).exists { op =>
      op === operator
    }).assert
  }

  test("ObserveEngine setImageQuality should set Image Quality condition") {
    val iq = ImageQuality.Preset.PointEight.toImageQuality
    val s0 = EngineState.default[IO]

    (for {
      oe <- observeEngine
      sf <- advanceN(oe, s0, oe.setImageQuality(iq, user, clientId), 2)
    } yield sf.flatMap(EngineState.conditions.andThen(Conditions.iq).get).exists { op =>
      op === iq
    }).assert

  }

  test("ObserveEngine setWaterVapor should set Water Vapor condition") {
    val wv = WaterVapor.VeryDry
    val s0 = EngineState.default[IO]
    (for {
      oe <- observeEngine
      sf <- advanceN(oe, s0, oe.setWaterVapor(wv, user, clientId), 2)
    } yield sf.flatMap(EngineState.conditions.andThen(Conditions.wv).get).exists { op =>
      op === wv
    }).assert
  }

  test("ObserveEngine setCloudExtinction should set Cloud Extinction condition") {
    val ce = CloudExtinction.Preset.TwoPointZero.toCloudExtinction
    val s0 = EngineState.default[IO]
    (for {
      oe <- observeEngine
      sf <- advanceN(oe, s0, oe.setCloudExtinction(ce, user, clientId), 2)
    } yield sf.flatMap(EngineState.conditions.andThen(Conditions.ce).get).exists { op =>
      op === ce
    }).assert
  }

  test("ObserveEngine setSkyBackground should set Sky Background condition") {
    val sb = SkyBackground.Bright
    val s0 = EngineState.default[IO]
    (for {
      oe <- observeEngine
      sf <- advanceN(oe, s0, oe.setSkyBackground(sb, user, clientId), 2)
    } yield sf.flatMap(EngineState.conditions.andThen(Conditions.sb).get).exists { op =>
      op === sb
    }).assert
  }

  test("ObserveEngine setObserver should set observer's name") {
    val s0 = loadDefaultSequence(seqObsId1)
      .apply(EngineState.default[IO])
    (for {
      oe <- observeEngine
      sf <-
        advanceN(oe, s0, oe.setObserver(seqObsId1, user, observer), 2)
    } yield sf
      .flatMap(EngineState.atSequence(seqObsId1).getOption)
      .flatMap(_.observer)
      .exists { op =>
        op === observer
      }).assert
  }

  test("ObserveEngine should not run 2nd sequence because it's using the same resource") {
    for {
      odb                  <-
        TestOdbProxy.build[IO](
          Map(
            Instrument.GmosSouth -> TestOdbProxy.InstrumentState(
              seqObsId2,
              staticCfg2,
              TestOdbProxy.SequenceState(none, List.empty)
            ),
            Instrument.GmosNorth -> TestOdbProxy.InstrumentState(
              seqObsId1,
              staticCfg1,
              TestOdbProxy.SequenceState(
                Atom( // We need a step for checks to run
                  atomId1,
                  none,
                  NonEmptyList.one:
                    Step[DynamicConfig.GmosNorth](
                      stepId(1),
                      dynamicCfg1,
                      stepCfg1,
                      telescopeCfg1,
                      StepEstimate.Zero,
                      ObserveClass.Science,
                      Breakpoint.Disabled
                    )
                ).some,
                List.empty
              )
            )
          )
        )
      (eng, observeEngine) <- bothEngines(defaultSystems.map(_.copy(odb = odb)))
      eo                    = EngineObserver(observeEngine)
      _                    <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosNorth, seqObsId1, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId1, clientId))
                ) =>
              true
          }
        )
      _                    <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosSouth, seqObsId2, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId2, clientId))
                ) =>
              true
          }
        )
      _                    <-
        eng.offer:
          Event.modifyState[IO]:
            EngineHandle
              .modifySequenceState[IO](seqObsId2):
                SequenceState.status.replace(SequenceStatus.Running.Init) >>>
                  SequenceState.loadedStep.replace:
                    LoadedStep(DummyStepGen, DummyExecutionZipper).some
              .as(SeqEvent.NullSeqEvent)
      _                    <-
        eo.executeAndWait(
          _.start(seqObsId1, user, observer, clientId, RunOverride.Default),
          {
            case (EventResult.UserCommandResponse(
                    _,
                    Outcome.Ok,
                    Some(SeqEvent.Busy(seqObsId1, clientId))
                  ),
                  state
                ) => // Loaded step should have been cleared
              state.selected.gmosNorth.exists(_.loadedStep.isEmpty)
          }
        )
    } yield () // If the test completes, it's correct.
  }

  test("ObserveEngine should run 2nd sequence when there are no shared resources") {
    for {
      odb                  <-
        TestOdbProxy.build[IO](
          Map(
            Instrument.GmosSouth -> TestOdbProxy.InstrumentState(
              seqObsId2,
              staticCfg2,
              TestOdbProxy.SequenceState(none, List.empty)
            ),
            Instrument.GmosNorth -> TestOdbProxy.InstrumentState(
              seqObsId1,
              staticCfg1,
              TestOdbProxy.SequenceState(
                Atom( // We need a step for checks to run
                  atomId1,
                  none,
                  NonEmptyList.one:
                    Step[DynamicConfig.GmosNorth](
                      stepId(1),
                      dynamicCfg1,
                      stepCfg1,
                      telescopeCfg1,
                      StepEstimate.Zero,
                      ObserveClass.Science,
                      Breakpoint.Disabled
                    )
                ).some,
                List.empty
              )
            )
          )
        )
      (eng, observeEngine) <- bothEngines(defaultSystems.map(_.copy(odb = odb)))
      eo                    = EngineObserver(observeEngine)
      _                    <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosNorth, seqObsId1, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId1, clientId))
                ) =>
              true
          }
        )
      _                    <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosSouth, seqObsId2, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId2, clientId))
                ) =>
              true
          }
        )
      _                    <-
        eng.offer:
          Event.modifyState[IO]:
            EngineHandle
              .modifySequenceState[IO](seqObsId2):
                SequenceState.status.replace(SequenceStatus.Running.Init) >>>
                  SequenceState.loadedStep.replace:
                    LoadedStep(
                      DummyStepGen.copy(resources = Set(Instrument.GmosSouth)),
                      DummyExecutionZipper
                    ).some
              .as(SeqEvent.NullSeqEvent)
      _                    <-
        eo.executeAndWaitResult(
          _.start(seqObsId1, user, observer, clientId, RunOverride.Default),
          {
            case (EventResult.UserCommandResponse(
                  _,
                  Outcome.Ok,
                  Some(SeqEvent.AcquisitionCompleted(seqObsId1))
                )) =>
              true
          }
        )
    } yield () // If the test completes, it's correct.
  }

  test("ObserveEngine configSystem should run a system configuration") {
    val s0 = loadSequenceWithResources(
      seqObsId1,
      Set(Instrument.GmosNorth, TCS),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    )
      .apply(EngineState.default[IO])

    val acquisitionStep = NonEmptyList.one:
      Step[DynamicConfig.GmosNorth](
        stepId(1),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      )

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, acquisitionStep)

    (for {
      odb <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      oe  <- observeEngineWithODB(odb)
      sf  <- advanceN(
               oe,
               s0,
               oe.configSystem(
                 seqObsId1,
                 observer,
                 user,
                 stepId(1),
                 TCS,
                 clientId
               ),
               3
             )
    } yield sf
      .flatMap(EngineState.atSequence(seqObsId1).getOption)
      .flatMap(s => s.configActionCoord(stepId(1), TCS).map(s.seq.getSingleState))
      .exists(_.started)).assert
  }

  test("ObserveEngine should not run a system configuration if sequence is running") {
    val s0 = (loadSequenceWithResources(
      seqObsId1,
      Set(Instrument.GmosNorth, TCS),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      EngineState
        .sequenceStateAt[IO](seqObsId1)
        .andThen(SequenceState.status[IO])
        .replace(SequenceStatus.Running.Init)).apply(EngineState.default[IO])

    val acquisitionStep = NonEmptyList.one:
      Step[DynamicConfig.GmosNorth](
        stepId(1),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      )

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, acquisitionStep)

    (for {
      odb <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      oe  <- observeEngineWithODB(odb)
      sf  <- advanceOne(
               oe,
               s0,
               oe.configSystem(
                 seqObsId1,
                 observer,
                 user,
                 stepId(1),
                 TCS,
                 clientId
               )
             )
    } yield sf
      .flatMap(EngineState.atSequence(seqObsId1).getOption)
      .flatMap(s => s.configActionCoord(stepId(1), TCS).map(s.seq.getSingleState))
      .exists(_.isIdle)).assert
  }

  test("ObserveEngine should not run a system configuration if system is in use") {
    val s0 = (loadSequenceWithResources(
      seqObsId1,
      Set(Instrument.GmosNorth, TCS),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      loadSequenceWithResources(
        seqObsId2,
        Set(Instrument.GmosSouth, TCS),
        EngineState.instrumentLoaded(Instrument.GmosSouth)
      ) >>>
      EngineState
        .sequenceStateAt[IO](seqObsId1)
        .andThen(SequenceState.status[IO])
        .replace(SequenceStatus.Running.Init)).apply(EngineState.default[IO])

    (for {
      oe <- observeEngine
      sf <-
        advanceN(
          oe,
          s0,
          oe.configSystem(seqObsId2, observer, user, stepId(1), TCS, clientId),
          3
        )
    } yield sf
      .flatMap(
        EngineState
          .atSequence(seqObsId2)
          .getOption
      )
      .flatMap(s => s.configActionCoord(stepId(1), TCS).map(s.seq.getSingleState))
      .exists(_.isIdle)).assert
  }

  test(
    "ObserveEngine should run a system configuration when other sequence is running with other systems"
  ) {
    val s0 = (loadSequenceWithResources(
      seqObsId1,
      Set(Instrument.GmosNorth, TCS),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      loadSequenceWithResources(
        seqObsId2,
        Set(Instrument.GmosNorth, Gcal),
        EngineState.instrumentLoaded(Instrument.GmosSouth)
      ) >>>
      EngineState
        .sequenceStateAt[IO](seqObsId1)
        .andThen(SequenceState.status[IO])
        .replace(SequenceStatus.Running.Init)).apply(EngineState.default[IO])

    val acquisitionStep = NonEmptyList.one:
      Step[DynamicConfig.GmosNorth](
        stepId(1),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      )

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, acquisitionStep)

    (for {
      odb <- TestOdbProxy.buildGmosNorth[IO](seqObsId2, staticCfg1, acqAtom.some, List.empty)
      oe  <- observeEngineWithODB(odb)
      sf  <- advanceN(
               oe,
               s0,
               oe.configSystem(seqObsId2, observer, user, stepId(1), Gcal, clientId),
               3
             )
    } yield sf
      .flatMap(EngineState.atSequence(seqObsId2).getOption)
      .flatMap(s => s.configActionCoord(stepId(1), Gcal).map(s.seq.getSingleState))
      .exists(_.started)).assert
  }

  private def testTargetSequenceData(
    targetName: NonEmptyString
  ): (ODBObservation, NonEmptyList[Step[DynamicConfig.GmosNorth]]) = {
    val reqConditions = ConstraintSet(
      ImageQuality.Preset.PointTwo,
      CloudExtinction.Preset.PointFive,
      SkyBackground.Dark,
      WaterVapor.Median,
      ElevationRange.ByHourAngle.Default
    )

    val stepList: NonEmptyList[Step[DynamicConfig.GmosNorth]] =
      NonEmptyList.one:
        Step[DynamicConfig.GmosNorth](
          stepId1,
          dynamicCfg1,
          StepConfig.Science,
          telescopeCfg1,
          StepEstimate.Zero,
          ObserveClass.Science,
          Breakpoint.Disabled
        )

    val obs = ODBObservation(
      id = seqObsId1,
      title = "Test Observation".refined,
      ODBObservation.Program(
        Program.Id(PosLong.unsafeFrom(123)),
        None,
        ODBObservation.Program.Goa(NonNegInt.unsafeFrom(0))
      ),
      TargetEnvironment(
        Some(FirstScienceTarget(Target.Id.fromLong(1).get, targetName)),
        GuideEnvironment(List.empty)
      ),
      reqConditions,
      List.empty,
      ModeSignalToNoise.Spectroscopy(none, none)
    )

    (obs, stepList)
  }

  private def systemsWithTargetName(name: String): IO[Systems[IO]] =
    defaultSystems.map:
      _.copy(tcsKeywordReader = new DummyTcsKeywordsReader.DummyTcsKeywordReaderImpl[IO] {
        override def sourceATarget: TargetKeywordsReader[IO] =
          new DummyTargetKeywordsReader.DummyTargetKeywordsReaderImpl[IO] {
            override def objectName: IO[String] = name.pure[IO]
          }
      })

  test("ObserveEngine start should start the sequence if it passes the target check") {
    val (obs, steps) = testTargetSequenceData("proof".refined)

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = ODBSequencesLoader
      .loadSequenceMod[IO](
        none,
        OdbObservationData(
          obs,
          InstrumentExecutionConfig.GmosNorth(
            ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
          )
        ),
        EngineState.instrumentLoaded(Instrument.GmosNorth)
      )
      .apply(EngineState.default[IO])

    (for {
      acqAtomId <- IO.randomUUID.map(Atom.Id.fromUuid)
      odb       <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      oe        <- observeEngineWithODB(odb, systemsWithTargetName("proof"))
      sf        <- advanceOne(oe, s0, oe.start(seqObsId1, user, observer, clientId, RunOverride.Default))
    } yield sf
      .flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)
      .exists(_.status.isRunning)).assert
  }

  test(
    "ObserveEngine start should not start the sequence if it fails the target check for science observations"
  ) {
    val (obs, steps) = testTargetSequenceData("proof".refined)

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = ODBSequencesLoader
      .loadSequenceMod[IO](
        none,
        OdbObservationData(
          obs,
          InstrumentExecutionConfig.GmosNorth(
            ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
          )
        ),
        EngineState.instrumentLoaded(Instrument.GmosNorth)
      )
      .apply(EngineState.default[IO])

    (for {
      odb <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      oe  <- observeEngineWithODB(odb, systemsWithTargetName("proof1"))
      sf  <- advanceOne(oe, s0, oe.start(seqObsId1, user, observer, clientId, RunOverride.Default))
    } yield sf
      .flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)
      .exists(_.status.isIdle)).assert
  }

  test(
    "ObserveEngine start should start the sequence that fails the target check if forced"
  ) {
    val (obs, steps) = testTargetSequenceData("proof".refined)

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = ODBSequencesLoader
      .loadSequenceMod[IO](
        none,
        OdbObservationData(
          obs,
          InstrumentExecutionConfig.GmosNorth(
            ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
          )
        ),
        EngineState.instrumentLoaded(Instrument.GmosNorth)
      )
      .apply(EngineState.default[IO])

    (for {
      systems <- systemsWithTargetName("proof1")
      odb     <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      oe      <- observeEngineWithODB(odb)
      sf      <- advanceOne(oe, s0, oe.start(seqObsId1, user, observer, clientId, RunOverride.Override))
    } yield sf
      .flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)
      .exists(_.status.isRunning)).assert
  }

  private val testConditionsSequenceData
    : (ODBObservation, NonEmptyList[Step[DynamicConfig.GmosNorth]]) = {

    val reqConditions = ConstraintSet(
      ImageQuality.Preset.PointTwo,
      CloudExtinction.Preset.PointFive,
      SkyBackground.Dark,
      WaterVapor.Median,
      ElevationRange.ByHourAngle.Default
    )

    val stepList: NonEmptyList[Step[DynamicConfig.GmosNorth]] =
      NonEmptyList.one:
        Step[DynamicConfig.GmosNorth](
          stepId1,
          dynamicCfg1,
          StepConfig.Science,
          telescopeCfg1,
          StepEstimate.Zero,
          ObserveClass.Science,
          Breakpoint.Disabled
        )

    val obs = ODBObservation(
      id = seqObsId1,
      title = "Test Observation".refined,
      ODBObservation.Program(
        Program.Id(PosLong.unsafeFrom(123)),
        None,
        ODBObservation.Program.Goa(NonNegInt.unsafeFrom(0))
      ),
      TargetEnvironment(None, GuideEnvironment(List.empty)),
      reqConditions,
      List.empty,
      ModeSignalToNoise.Spectroscopy(none, none)
    )

    (obs, stepList)
  }

  test("ObserveEngine start should start the sequence if it passes the conditions check") {
    val (obs, steps) = testConditionsSequenceData

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = (ODBSequencesLoader.loadSequenceMod[IO](
      None,
      OdbObservationData(
        obs,
        InstrumentExecutionConfig.GmosNorth(
          ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
        )
      ),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      EngineState.conditions
        .andThen(Conditions.iq)
        .replace(ImageQuality.Preset.PointTwo.toImageQuality.some) >>>
      EngineState.conditions.andThen(Conditions.wv).replace(WaterVapor.Median.some) >>>
      EngineState.conditions.andThen(Conditions.sb).replace(SkyBackground.Dark.some) >>>
      EngineState.conditions
        .andThen(Conditions.ce)
        .replace(CloudExtinction.Preset.PointFive.toCloudExtinction.some))
      .apply(EngineState.default[IO])

    (for {
      odb           <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      observeEngine <- observeEngineWithODB(odb)
      sf            <- advanceOne(
                         observeEngine,
                         s0,
                         observeEngine.start(
                           seqObsId1,
                           user,
                           observer,
                           clientId,
                           RunOverride.Default
                         )
                       )
    } yield sf.flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)).map { s =>
      assert(s.exists(_.status.isRunning))
      assert(s.flatMap(_.loadedStep).exists(_.id === stepId1))
    }
  }

  test("ObserveEngine start should not start the sequence if it fails the conditions check") {
    val (obs, steps) = testConditionsSequenceData

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = (ODBSequencesLoader.loadSequenceMod[IO](
      None,
      OdbObservationData(
        obs,
        InstrumentExecutionConfig.GmosNorth(
          ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
        )
      ),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      EngineState.conditions
        .andThen(Conditions.iq)
        .replace(ImageQuality.Preset.OnePointZero.toImageQuality.some) >>>
      EngineState.conditions.andThen(Conditions.wv).replace(WaterVapor.Dry.some) >>>
      EngineState.conditions.andThen(Conditions.sb).replace(SkyBackground.Darkest.some) >>>
      EngineState.conditions
        .andThen(Conditions.ce)
        .replace(CloudExtinction.Preset.OnePointZero.toCloudExtinction.some))
      .apply(EngineState.default[IO])

    for {
      odb           <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      observeEngine <- observeEngineWithODB(odb)
      result        <-
        observeEngine.start(
          seqObsId1,
          user,
          observer,
          clientId,
          RunOverride.Default
        ) *>
          observeEngine.eventResultStream(s0).take(1).compile.last
    } yield result
      .map { case (out, sf) =>
        assert(EngineState.sequenceStateAt[IO](seqObsId1).getOption(sf).exists(_.status.isIdle))
        assert(out match {
          case EventResult.UserCommandResponse(
                _,
                Outcome.Ok,
                Some(RequestConfirmation(UserPrompt.ChecksOverride(_, stpid, _), _))
              ) =>
            stpid === stepId1
          case _ => false
        })
      }
      .getOrElse(fail("Running ObserveEngine produced no output"))
  }

  test("ObserveEngine start should start the sequence that fails conditions check if forced") {
    val (obs, steps) = testConditionsSequenceData

    val acqAtom = Atom[DynamicConfig.GmosNorth](atomId1, none, steps)

    val s0 = (ODBSequencesLoader.loadSequenceMod[IO](
      None,
      OdbObservationData(
        obs,
        InstrumentExecutionConfig.GmosNorth(
          ExecutionConfig(staticCfg1, ExecutionSequence(acqAtom, List.empty, false).some, none)
        )
      ),
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    ) >>>
      EngineState.conditions
        .andThen(Conditions.iq)
        .replace(ImageQuality.Preset.OnePointZero.toImageQuality.some) >>>
      EngineState.conditions.andThen(Conditions.wv).replace(WaterVapor.Dry.some) >>>
      EngineState.conditions.andThen(Conditions.sb).replace(SkyBackground.Darkest.some) >>>
      EngineState.conditions
        .andThen(Conditions.ce)
        .replace(CloudExtinction.Preset.OnePointZero.toCloudExtinction.some))
      .apply(EngineState.default[IO])

    for {
      odb           <- TestOdbProxy.buildGmosNorth[IO](seqObsId1, staticCfg1, acqAtom.some, List.empty)
      observeEngine <- observeEngineWithODB(odb)
      sf            <-
        advanceN(
          observeEngine,
          s0,
          observeEngine
            .start(seqObsId1, user, observer, clientId, RunOverride.Override),
          3
        )
    } yield {
      assert(
        sf.flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)
          .exists(_.status.isRunning)
      )
      assert(
        sf.flatMap(EngineState.sequenceStateAt[IO](seqObsId1).getOption)
          .flatMap(_.loadedStep)
          .exists(_.id === stepId1)
      )
    }
  }

  test("ObserveEngine proceedAfterPrompt should load the next step and restart the execution") {
    val acquisitionStepCount = 2
    val scienceAtomCount     = 2
    val scienceStepCount     = 2
    val firstScienceStepId   = acquisitionStepCount * 2 + 1

    val acquisitionSteps = NonEmptyList(
      Step[DynamicConfig.GmosNorth](
        stepId(1),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      ),
      List
        .range(2, acquisitionStepCount + 1)
        .map(i =>
          Step[DynamicConfig.GmosNorth](
            stepId(i),
            dynamicCfg1,
            stepCfg1,
            telescopeCfg1,
            StepEstimate.Zero,
            ObserveClass.Science,
            Breakpoint.Disabled
          )
        )
    )
    val scienceSteps     = NonEmptyList(
      Step[DynamicConfig.GmosNorth](
        stepId(firstScienceStepId),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      ),
      List
        .range(firstScienceStepId + 1, firstScienceStepId + scienceStepCount)
        .map(i =>
          Step[DynamicConfig.GmosNorth](
            stepId(i),
            dynamicCfg1,
            stepCfg1,
            telescopeCfg1,
            StepEstimate.Zero,
            ObserveClass.Science,
            Breakpoint.Disabled
          )
        )
    )

    for {
      acqAtomId            <- IO.randomUUID.map(Atom.Id.fromUuid)
      atomIds              <- List
                                .fill(scienceAtomCount)(IO.randomUUID)
                                .parSequence
                                .map(_.map(Atom.Id.fromUuid))
      odb                  <- TestOdbProxy.buildGmosNorth[IO](
                                seqObsId1,
                                staticCfg1,
                                Atom[DynamicConfig.GmosNorth](acqAtomId, none, acquisitionSteps).some,
                                atomIds.map(i => Atom[DynamicConfig.GmosNorth](i, none, scienceSteps))
                              )
      (eng, observeEngine) <- bothEngines(defaultSystems.map(_.copy(odb = odb)))
      eo                    = EngineObserver(observeEngine)
      _                    <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosNorth, seqObsId1, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId1, clientId))
                ) =>
              true
          }
        )
      _                    <- eng.offer:
                                Event.modifyState[IO]:
                                  EngineHandle
                                    .modifySequenceState[IO](seqObsId1):
                                      SequenceState.status.replace:
                                        SequenceStatus.Running.Init.withWaitingUserPrompt(true)
                                    .as(SeqEvent.NullSeqEvent)
      r                    <-
        eo.executeAndWaitResult(
          _.proceedAfterPrompt(seqObsId1, user, observer, SequenceType.Acquisition),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.AcquisitionCompleted(seqObsId1))
                ) =>
              true
          }
        )
      s                    <-
        eo.executeAndWaitResult(
          _.proceedAfterPrompt(seqObsId1, user, observer, SequenceType.Science),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.SequenceCompleted(seqObsId1))
                ) =>
              true
          }
        )
    } yield {
      r.sequences
        .get(seqObsId1)
        .map(_.seq.status)
        .map(s => assertEquals(s, SequenceStatus.Running.Init.withWaitingUserPrompt(true)))
        .getOrElse(fail("Sequence id not loaded"))
      s.sequences
        .get(seqObsId1)
        .map(_.seq.status)
        .map(s => assertEquals(s, SequenceStatus.Completed))
        .getOrElse(fail("Sequence id not loaded"))
    }
  }

  test("ObserveEngine should automatically load new science steps and atoms") {
    val atomCount = 2
    val stepCount = 3

    val steps = NonEmptyList(
      Step[DynamicConfig.GmosNorth](
        stepId(1 + stepCount),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      ),
      List
        .range(2, stepCount + 1)
        .map(i =>
          Step[DynamicConfig.GmosNorth](
            stepId(i + stepCount),
            dynamicCfg1,
            stepCfg1,
            telescopeCfg1,
            StepEstimate.Zero,
            ObserveClass.Science,
            Breakpoint.Disabled
          )
        )
    )

    for {
      atomIds        <- List
                          .fill(atomCount)(IO.randomUUID)
                          .parSequence
                          .map(_.map(Atom.Id.fromUuid))
      odb            <- TestOdbProxy.buildGmosNorth[IO](
                          seqObsId1,
                          staticCfg1,
                          none,
                          atomIds.map(i => Atom[DynamicConfig.GmosNorth](i, none, steps))
                        )
      systems        <- defaultSystems.map(_.copy(odb = odb))
      observeEngine  <-
        ObserveEngine.build(Site.GS, systems, defaultSettings, ExecutionEnvironment.Development)
      eo              = EngineObserver(observeEngine)
      _              <-
        eo.executeAndWaitResult(
          _.loadSequence(Instrument.GmosNorth, seqObsId1, observer, user, clientId),
          {
            case EventResult.UserCommandResponse(
                  _,
                  _,
                  Some(SeqEvent.LoadSequence(seqObsId1, clientId))
                ) =>
              true
          }
        )
      executedSteps  <- IO.ref(0)
      r              <-
        eo.executeAndWaitResult(
          _.start(seqObsId1, user, observer, clientId, RunOverride.Override),
          { case EventResult.UserCommandResponse(_, _, Some(SeqEvent.SequenceCompleted(_))) =>
            true
          },
          tap = {
            case (EventResult.SystemUpdate(SystemEvent.StepComplete(obsId), Outcome.Ok), _)
                if obsId == seqObsId1 =>
              executedSteps.update(_ + 1).void
          }
        )
      completedSteps <- executedSteps.get
    } yield assertEquals(completedSteps, stepCount)
  }

  def assertStep(l: List[TestOdbProxy.OdbEvent]): List[TestOdbProxy.OdbEvent] = {
    def isDatasetStartExposure(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetStartExposure(obsId, _) => obsId === seqObsId1
      case _                                           => false
    }

    def isDatasetEndExposure(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetEndExposure(obsId, _) => obsId === seqObsId1
      case _                                         => false
    }

    def isDatasetStartReadout(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetStartReadout(obsId, _) => obsId === seqObsId1
      case _                                          => false
    }

    def isDatasetEndReadout(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetEndReadout(obsId, _) => obsId === seqObsId1
      case _                                        => false
    }

    def isDatasetStartWrite(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetStartWrite(obsId, _) => obsId === seqObsId1
      case _                                        => false
    }

    def isDatasetEndWrite(ev: TestOdbProxy.OdbEvent): Boolean = ev match {
      case TestOdbProxy.DatasetEndWrite(obsId, _) => obsId === seqObsId1
      case _                                      => false
    }

    val chk = List(
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepStartStep(seqObsId1)),
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepStartConfigure(seqObsId1)),
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepEndConfigure(seqObsId1)),
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepStartObserve(seqObsId1)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetStartExposure(ev)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetEndExposure(ev)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetStartReadout(ev)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetEndReadout(ev)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetStartWrite(ev)),
      (ev: TestOdbProxy.OdbEvent) => assert(isDatasetEndWrite(ev)),
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepEndObserve(seqObsId1)),
      (ev: TestOdbProxy.OdbEvent) => assertEquals(ev, TestOdbProxy.StepEndStep(seqObsId1))
    )

    assert(l.length >= chk.length)
    chk.zip(l).foreach { case (f, x) => f(x) }
    l.drop(chk.length)
  }

  def assertAtom(
    l:             List[TestOdbProxy.OdbEvent],
    atomStepCount: Int
  ): List[TestOdbProxy.OdbEvent] = {
    val ss = (1 until atomStepCount).foldLeft(assertStep(l)) { case (b, _) =>
      assertStep(b)
    }

    ss
  }

  test("ObserveEngine start should run the sequence and produce the ODB events") {
    val atomCount = 2
    val stepCount = 2

    val firstEvents = List(
      TestOdbProxy.VisitStart(seqObsId1),
      TestOdbProxy.SequenceStart(seqObsId1)
    )

    def steps(k: Int) = NonEmptyList(
      Step[DynamicConfig.GmosNorth](
        stepId(1 + k),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      ),
      List
        .range(2, stepCount + 1)
        .map(i =>
          Step[DynamicConfig.GmosNorth](
            stepId(i + k),
            dynamicCfg1,
            stepCfg1,
            telescopeCfg1,
            StepEstimate.Zero,
            ObserveClass.Science,
            Breakpoint.Disabled
          )
        )
    )

    for {
      atomIds       <- List
                         .fill(atomCount)(IO.delay(java.util.UUID.randomUUID()))
                         .parSequence
                         .map(_.map(Atom.Id.fromUuid))
      odb           <-
        TestOdbProxy.buildGmosNorth[IO](
          seqObsId1,
          staticCfg1,
          none,
          atomIds.zipWithIndex.map((i, k) => Atom[DynamicConfig.GmosNorth](i, none, steps(2 * k)))
        )
      systems       <- defaultSystems.map(_.copy(odb = odb))
      oseq          <- odb.read(seqObsId1)
      seqo          <- generateStepGen(oseq, systems)
      s0             = ODBSequencesLoader
                         .loadSequenceMod[IO](
                           None,
                           oseq,
                           EngineState.instrumentLoaded(Instrument.GmosNorth)
                         )
                         .apply(EngineState.default[IO])
      observeEngine <-
        ObserveEngine.build(Site.GS, systems, defaultSettings, ExecutionEnvironment.Development)
      eo             = EngineObserver(observeEngine, s0)
      _             <- eo.executeAndWaitState(
                         _.start(seqObsId1, user, observer, clientId, RunOverride.Override),
                         _.sequences.get(seqObsId1).forall(x => isFinished(x.seq.status))
                       )
      res           <- odb.outCapture
    } yield {
      assertEquals(res.take(firstEvents.length), firstEvents)
      val rest = (1 until atomCount).foldLeft(
        assertAtom(res.drop(firstEvents.length), stepCount)
      ) { case (b, _) =>
        assertAtom(b, stepCount)
      }
      assertEquals(rest.length, 0)
    }
  }

  test(
    "ObserveEngine should run the sequence and load new events, executing the last data provided by the ODB"
  ) {
    val atomCount = 2
    val stepCount = 2

    val firstEvents = List(
      TestOdbProxy.VisitStart(seqObsId1),
      TestOdbProxy.SequenceStart(seqObsId1)
    )

    def steps(k: Int) = NonEmptyList(
      Step[DynamicConfig.GmosNorth](
        stepId(1 + k),
        dynamicCfg1,
        stepCfg1,
        telescopeCfg1,
        StepEstimate.Zero,
        ObserveClass.Science,
        Breakpoint.Disabled
      ),
      List
        .range(2, stepCount + 1)
        .map(i =>
          Step[DynamicConfig.GmosNorth](
            stepId(i + k),
            dynamicCfg1,
            stepCfg1,
            telescopeCfg1,
            StepEstimate.Zero,
            ObserveClass.Science,
            Breakpoint.Disabled
          )
        )
    )

    // Ugly flag but it would be quite complicated to avoid it
    var addStep = true
    for {
      atomIds       <- List
                         .fill(atomCount)(IO.delay(java.util.UUID.randomUUID()))
                         .parSequence
                         .map(_.map(Atom.Id.fromUuid))
      odb           <-
        TestOdbProxy.buildGmosNorth[IO](
          seqObsId1,
          staticCfg1,
          none,
          atomIds.zipWithIndex.map((i, k) => Atom[DynamicConfig.GmosNorth](i, none, steps(2 * k))),
          s =>
            if (addStep) {
              // Only once we add an extra setp at the beggining of the atom
              addStep = false
              s.copy(science =
                s.science.map(s =>
                  if (atomIds.headOption.exists(_ === s.id))
                    s.copy(steps = steps(5).head :: s.steps)
                  else s
                )
              )
            } else s
        )
      systems       <- defaultSystems.map(_.copy(odb = odb))
      oseq          <- odb.read(seqObsId1)
      seqo          <- generateStepGen(oseq, systems)
      s0             = ODBSequencesLoader
                         .loadSequenceMod[IO](
                           None,
                           oseq,
                           EngineState.instrumentLoaded(Instrument.GmosNorth)
                         )
                         .apply(EngineState.default[IO])
      observeEngine <-
        ObserveEngine.build(Site.GS, systems, defaultSettings, ExecutionEnvironment.Development)
      eo             = EngineObserver(observeEngine, s0)
      _             <- eo.executeAndWaitState(
                         _.start(seqObsId1, user, observer, clientId, RunOverride.Override),
                         _.sequences.get(seqObsId1).forall(x => isFinished(x.seq.status))
                       )
      res           <- odb.outCapture
    } yield {
      assertEquals(res.take(firstEvents.length), firstEvents)
      val rest = (1 until atomCount).foldLeft {
        assertAtom(res.drop(firstEvents.length), 3)
      } { case (b, _) =>
        assertAtom(b, 2)
      }
      assertEquals(rest.length, 0)
    }
  }
}
