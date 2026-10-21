// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosPreImaging
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig as CoreTelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.refined.auto.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as ODBObservation
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment.GuideEnvironment
import observe.common.test.*
import observe.model.ActionType
import observe.model.ClientId
import observe.model.CurrentConditions
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.model.SubsystemOverrides
import observe.model.config.*
import observe.model.config.SubsystemControlModes
import observe.model.dhs.*
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.SubsystemControlMode
import observe.server.ODBSequencesLoader
import observe.server.engine.Action
import observe.server.engine.Engine
import observe.server.engine.Response
import observe.server.engine.Result
import observe.server.engine.Result.PartialVal
import observe.server.engine.Result.PauseContext
import observe.server.odb.OdbObservationData
import observe.server.odb.OdbProxy
import org.http4s.Uri
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import org.typelevel.otel4s.trace.Tracer

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

trait TestCommon extends munit.CatsEffectSuite {
  import TestCommon.*
  given Logger[IO] = NoOpLogger.impl[IO]
  given Tracer[IO] = Tracer.noop[IO]

  val defaultServices: IO[Services[IO]] = Services.dummy[IO]

  val observeEngine: IO[ObserveEngine[IO]] =
    defaultServices.flatMap:
      ObserveEngine.build(Site.GS, _, defaultSettings, ExecutionEnvironment.Development)

  def bothEngines(
    services: IO[Services[IO]] = defaultServices
  ): IO[(Engine[IO], ObserveEngine[IO])] =
    for
      services <- services
      rc       <- Ref.of[IO, CurrentConditions](CurrentConditions.Default)
      tr       <- ObserveEngine.createTranslator(Site.GS, services, rc, ExecutionEnvironment.Development)
      eng      <- Engine.build[IO](ObserveEngine.onStepComplete[IO](services.odb, tr))
    yield (eng, new ObserveEngineImpl[IO](eng, services, defaultSettings, tr, rc))

  def observeEngineWithODB(
    odb:      OdbProxy[IO],
    services: IO[Services[IO]] = defaultServices,
    settings: ObserveEngineConfiguration = defaultSettings
  ): IO[ObserveEngine[IO]] =
    services.flatMap(sys =>
      ObserveEngine.build(
        Site.GN,
        sys.copy(odb = odb),
        settings,
        ExecutionEnvironment.Development
      )
    )

  def advanceOne(
    oe:  ObserveEngine[IO],
    s0:  EngineState[IO],
    put: IO[Unit]
  ): IO[Option[EngineState[IO]]] =
    advanceN(oe, s0, put, 1L)

  def advanceN(
    oe:  ObserveEngine[IO],
    s0:  EngineState[IO],
    put: IO[Unit],
    n:   Long
  ): IO[Option[EngineState[IO]]] =
    (put *> oe
      .eventResultStream(s0)
      // .evalTap: (r, s) =>
      //   IO.println(s"******** Event result: ${pprint(r)}, new state: ${pprint(s)}")
      .take(n)
      .compile
      .last)
      .map(_.map(_._2))

  def isFinished(status: SequenceStatus): Boolean = status match {
    case SequenceStatus.Idle      => true
    case SequenceStatus.Completed => true
    case SequenceStatus.Failed(_) => true
    case _                        => false
  }

}

object TestCommon {

  given Logger[IO] = NoOpLogger.impl[IO]

  val defaultSettings: ObserveEngineConfiguration = ObserveEngineConfiguration(
    odbHttp = uri"localhost",
    odbWs = uri"localhost",
    navigateHttp = uri"localhost",
    dhsServer = uri"http://localhost/",
    subsystemControlModes = SubsystemControlModes(
      altair = SubsystemControlMode.Simulated,
      gems = SubsystemControlMode.Simulated,
      dhs = SubsystemControlMode.Simulated,
      flamingos2 = SubsystemControlMode.Simulated,
      gcal = SubsystemControlMode.Simulated,
      gmos = SubsystemControlMode.Simulated,
      gnirs = SubsystemControlMode.Simulated,
      ghost = SubsystemControlMode.Simulated,
      igrins2 = SubsystemControlMode.Simulated,
      gsaoi = SubsystemControlMode.Simulated,
      gws = SubsystemControlMode.Simulated,
      nifs = SubsystemControlMode.Simulated,
      niri = SubsystemControlMode.Simulated,
      tcs = SubsystemControlMode.Simulated
    ),
    odbNotifications = false,
    instForceError = false,
    failAt = 0,
    10.seconds,
    GpiUriSettings(uri"vm://localhost:8888/xmlrpc"),
    GpiUriSettings(uri"http://localhost:8888/xmlrpc"),
    GhostUriSettings(uri"vm://localhost:8888/xmlrpc"),
    GhostUriSettings(uri"http://localhost:8888/xmlrpc"),
    Igrins2UriSettings(uri"vm://localhost:8888/xmlrpc"),
    Igrins2UriSettings(uri"http://localhost:8888/xmlrpc"),
    "",
    Some("127.0.0.1"),
    0,
    3.seconds,
    10.seconds,
    32
  )

  def configure[F[_]: Applicative](subsystem: Subsystem): F[Result] =
    Result.OK(Response.Configured(subsystem)).pure[F].widen

  def pendingAction[F[_]: Applicative](subsystem: Subsystem): Action[F] =
    engine.fromF[F](ActionType.Configure(subsystem), configure(subsystem))

  def odbAction[F[_]: Applicative]: Action[F] =
    engine.fromF(ActionType.OdbEvent, Result.OK(Response.Ignored).pure[F])

  def running[F[_]: Applicative](subsystem: Subsystem): Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, Nil))(
        pendingAction(subsystem)
      )

  def done[F[_]: Applicative](subsystem: Subsystem): Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Completed(Response.Configured(subsystem)), Nil))(
        pendingAction(subsystem)
      )

  def failed[F[_]: Applicative](resource: Subsystem): Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Failed(Result.Error("boom")), Nil))(
        pendingAction(resource)
      )

  private val fileId = ImageFileId("fileId")

  def observing[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](ActionType.Observe, Result.OK(Response.Observed(fileId)).pure[F].widen)
      )

  case class PartialValue(s: String) extends PartialVal

  def observingPartial[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](ActionType.Observe,
                        Result.Partial(PartialValue("Value")).pure[F].widen,
                        Result.OK(Response.Ignored).pure[F].widen
        )
      )

  def fileIdReady[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId)))
      )(observing)

  def observed[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Completed(Response.Observed(fileId)),
                     List(FileIdAllocated(fileId))
        )
      )(observing)

  def observePartial[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId)))
      )(
        observingPartial
      )

  def paused[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Paused(new PauseContext {}), List(FileIdAllocated(fileId)))
      )(observing)

  val seqName1: Observation.Name           = "GS-2018B-Q-0-1"
  val seqObsId1: Observation.Id            = observationId(1)
  val seqName2: Observation.Name           = "GS-2018B-Q-0-2"
  val seqObsId2: Observation.Id            = observationId(2)
  val seqName3: Observation.Name           = "GS-2018B-Q-0-3"
  val seqObsId3: Observation.Id            = observationId(3)
  val clientId: ClientId                   = ClientId(UUID.randomUUID)
  val atomId1: Atom.Id                     =
    Atom.Id(UUID.fromString("5fbe2ac4-5ba7-11ee-8c99-0242ac120002"))
  val stepId1: Step.Id                     =
    Step.Id(UUID.fromString("91806878-5173-4da9-ae2b-d7bf32407871"))
  val staticCfg1: StaticConfig.GmosNorth   = StaticConfig.GmosNorth(
    GmosNorthStageMode.FollowXy,
    GmosNorthDetector.Hamamatsu,
    MosPreImaging.IsNotMosPreImaging,
    None
  )
  val staticCfg2: StaticConfig.GmosSouth   = StaticConfig.GmosSouth(
    GmosSouthStageMode.FollowXyz,
    GmosSouthDetector.Hamamatsu,
    MosPreImaging.IsNotMosPreImaging,
    None
  )
  val dynamicCfg1: DynamicConfig.GmosNorth = DynamicConfig.GmosNorth(
    TimeSpan.unsafeFromMicroseconds(5000000000L),
    GmosCcdMode(
      GmosXBinning.Two,
      GmosYBinning.Two,
      GmosAmpCount.Three,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ),
    GmosDtax.Two,
    GmosRoi.CentralSpectrum,
    GmosGratingConfig
      .North(
        GmosNorthGrating.B1200_G5301,
        GmosGratingOrder.One,
        Wavelength.unsafeFromIntPicometers(400000)
      )
      .some,
    None,
    GmosFpuMask.Builtin[GmosNorthFpu](GmosNorthFpu.LongSlit_0_50).some
  )

  val dynamicCfg2: DynamicConfig.GmosSouth = DynamicConfig.GmosSouth(
    TimeSpan.unsafeFromMicroseconds(5000000000L),
    GmosCcdMode(
      GmosXBinning.Two,
      GmosYBinning.Two,
      GmosAmpCount.Three,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ),
    GmosDtax.Two,
    GmosRoi.CentralSpectrum,
    GmosGratingConfig
      .South(
        GmosSouthGrating.B1200_G5321,
        GmosGratingOrder.One,
        Wavelength.unsafeFromIntPicometers(400000)
      )
      .some,
    None,
    GmosFpuMask.Builtin[GmosSouthFpu](GmosSouthFpu.LongSlit_0_50).some
  )

  val stepCfg1: StepConfig = StepConfig.Science

  val telescopeCfg1: CoreTelescopeConfig =
    CoreTelescopeConfig(Offset.Zero, StepGuideState.Enabled)

  val defaultTargetEnvironment: TargetEnvironment =
    TargetEnvironment(
      asterism = List.empty,
      firstScienceTarget = None,
      useBlindOffset = false,
      blindOffsetTarget = None,
      basePosition = None,
      guideEnvironment = GuideEnvironment(List.empty)
    )

  def odbObservation(id: Observation.Id): ODBObservation =
    ODBObservation(
      id = id,
      title = "Test Observation".refined,
      Timestamp.fromInstantTruncatedAndBounded(Instant.ofEpochSecond(31816800)).some,
      ODBObservation.Program(
        Program.Id(PosLong.unsafeFrom(123)),
        None,
        ODBObservation.Program.Goa(NonNegInt.unsafeFrom(0))
      ),
      defaultTargetEnvironment.some,
      ConstraintSet(
        ImageQuality.Preset.PointOne,
        CloudExtinction.Preset.PointOne,
        SkyBackground.Dark,
        WaterVapor.Median,
        ElevationRange.ByAirMass.Default
      ),
      ODBObservation.SchedulingConstraints(List.empty),
      List.empty
    )

  /** Builds an OdbObservationData for testing with GmosNorth execution config. */
  def gmosNorthOdbData(id: Observation.Id): OdbObservationData =
    OdbObservationData(
      odbObservation(id),
      InstrumentExecutionConfig.GmosNorth(
        ExecutionConfig(staticCfg1, none, none)
      )
    )

  /** Builds an OdbObservationData with a custom ODBObservation. */
  def gmosNorthOdbDataWith(obs: ODBObservation): OdbObservationData =
    OdbObservationData(
      obs,
      InstrumentExecutionConfig.GmosNorth(
        ExecutionConfig(staticCfg1, none, none)
      )
    )

  /** Builds a GmosSouth OdbObservationData for testing. */
  def gmosSouthOdbData(id: Observation.Id): OdbObservationData =
    OdbObservationData(
      odbObservation(id),
      InstrumentExecutionConfig.GmosSouth(
        ExecutionConfig(
          staticCfg2,
          none,
          none
        )
      )
    )

  val stepGen: StepGen.GmosNorth[IO] =
    StepGen.GmosNorth[IO](
      atomId = atomId1,
      sequenceType = SequenceType.Science,
      id = stepId(1),
      dataId = Monoid.empty[DataId],
      subsystems = Set(Instrument.GmosNorth, ObservatorySubsystem.Tcs),
      obsControl = _ => InstrumentHandler.Uncontrollable,
      generator = StepActionsGen(
        odbAction[IO],
        odbAction[IO],
        configs = Map(),
        odbAction[IO],
        odbAction[IO],
        post = (_, _) => List(NonEmptyList.one(pendingAction[IO](Instrument.GmosNorth))),
        odbAction[IO],
        odbAction[IO]
      ),
      instConfig = dynamicCfg1,
      config = stepCfg1,
      telescopeConfig = telescopeCfg1,
      breakpoint = Breakpoint.Disabled
    )

  def stepGenWithSubsystems(
    stepIdx:    Int,
    subsystems: Set[Subsystem]
  ): StepGen.GmosNorth[IO] =
    StepGen.GmosNorth[IO](
      atomId = atomId1,
      sequenceType = SequenceType.Science,
      id = stepId(stepIdx),
      dataId = Monoid.empty[DataId],
      subsystems = subsystems,
      obsControl = _ => InstrumentHandler.Uncontrollable,
      generator = StepActionsGen(
        odbAction[IO],
        odbAction[IO],
        configs =
          subsystems.map(r => r -> { (_: SubsystemOverrides) => pendingAction[IO](r) }).toMap,
        odbAction[IO],
        odbAction[IO],
        post = (_, _) => Nil,
        odbAction[IO],
        odbAction[IO]
      ),
      instConfig = dynamicCfg1,
      config = stepCfg1,
      telescopeConfig = telescopeCfg1,
      breakpoint = Breakpoint.Disabled
    )

  def generateStepGen[F[_]: {Async, Logger}](
    odbObsData: OdbObservationData,
    services:   Services[F]
  ): F[Option[StepGen[F]]] = for {
    c  <- Ref.of[F, CurrentConditions](CurrentConditions.Default)
    st <- SeqTranslate(Site.GS, services, c, ExecutionEnvironment.Development)
  } yield st.nextStep(odbObsData, SequenceType.Acquisition.asLeft)._2

  def loadSequence(
    id:   Observation.Id,
    sg:   Option[StepGen.GmosNorth[IO]],
    lens: monocle.Lens[EngineState[IO], Option[SequenceData[IO]]]
  ): cats.Endo[EngineState[IO]] =
    ODBSequencesLoader.loadSequenceMod[IO](None, gmosNorthOdbData(id), lens) >>>
      lens.some
        .andThen(SequenceData.seq)
        .modify:
          _.withLoadedStepGen(sg, SubsystemOverrides.AllEnabled, HeaderExtraData.Default)

  /**
   * Convenience: build a GmosNorth sequence with a single step and default subsystems. Replaces the
   * old `sequence(obsId)` helper.
   */
  def loadDefaultSequence(
    id: Observation.Id
  ): cats.Endo[EngineState[IO]] =
    loadSequence(id, stepGen.some, EngineState.instrumentLoaded(Instrument.GmosNorth))

  /**
   * Convenience: build a GmosNorth sequence with a single step and configurable subsystems.
   */
  def loadSequenceWithSubsystems(
    id:         Observation.Id,
    subsystems: Set[Subsystem],
    lens:       monocle.Lens[EngineState[IO], Option[SequenceData[IO]]]
  ): cats.Endo[EngineState[IO]] =
    loadSequence(id, stepGenWithSubsystems(1, subsystems).some, lens)
}
