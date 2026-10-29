// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.kernel.Sync
import cats.syntax.all.*
import fs2.Pipe
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.syntax.*
import lucuma.core.enums
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.Instrument
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.Site
import lucuma.core.enums.SlewStage
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.GuideConfig
import lucuma.core.model.IntPercent
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.Observation
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.navigate.LightSource
import monocle.Lens
import monocle.syntax.all.focus
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.BafflesState
import navigate.model.CommandResult
import navigate.model.Distance
import navigate.model.FocalPlaneOffset
import navigate.model.GuideState
import navigate.model.GuidersQualityValues
import navigate.model.HandsetAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.LightPath
import navigate.model.NavigateCommand
import navigate.model.NavigateCommand.*
import navigate.model.NavigateEvent
import navigate.model.NavigateEvent.*
import navigate.model.NavigateState
import navigate.model.PointingCorrections
import navigate.model.PwfsMechsState
import navigate.model.RotatorAngle
import navigate.model.RotatorTrackConfig
import navigate.model.SlewOptions
import navigate.model.SwapConfig
import navigate.model.Target
import navigate.model.TargetOffsets
import navigate.model.TcsConfig
import navigate.model.TelescopeState
import navigate.model.TrackingConfig
import navigate.model.WfsConfiguration
import navigate.model.config.NavigateEngineConfiguration
import navigate.model.config.SubsystemControlMode
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.DomeMode
import navigate.model.enums.LightSink
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.QlMode
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.acm.GeminiApplyCommand
import navigate.server.ephemeris.EphemerisUpdater
import navigate.server.tcs.TcsBaseControllerEpics.WfsGuideStates
import navigate.stateengine.Handler
import navigate.stateengine.StateEngine
import navigate.stateengine.StateEngine.Event
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import org.http4s.dsl.io.*
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.LocalDate
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import NavigateEvent.NullEvent

trait NavigateEngine[F[_]] {
  val services: Services[F]
  def eventStream: Stream[F, NavigateEvent]
  def mcsPark: F[CommandResult]
  def mcsFollow(enable:                              Boolean): F[CommandResult]
  def mcsUnwrap: F[CommandResult]
  def scsFollow(enable:                              Boolean): F[CommandResult]
  def rotStop(useBrakes:                             Boolean): F[CommandResult]
  def rotPark: F[CommandResult]
  def rotFollow(enable:                              Boolean): F[CommandResult]
  def rotUnwrap: F[CommandResult]
  def rotMove(angle:                                 RotatorAngle): F[CommandResult]
  def rotTrackingConfig(cfg:                         RotatorTrackConfig): F[CommandResult]
  def tcsConfig(config:                              TcsConfig): F[CommandResult]
  def slew(
    slewOptions: SlewOptions,
    tcsConfig:   TcsConfig,
    oid:         Option[Observation.Id]
  ): F[CommandResult]
  def instrumentSpecifics(instrumentSpecificsParams: InstrumentSpecifics): F[CommandResult]
  def pwfs1Target(target:                            Target): F[CommandResult]
  def pwfs1ProbeTracking(config:                     TrackingConfig): F[CommandResult]
  def pwfs1Park: F[CommandResult]
  def pwfs1Follow(enable:                            Boolean): F[CommandResult]
  def pwfs1Unwrap: F[CommandResult]
  def pwfs2Target(target:                            Target): F[CommandResult]
  def pwfs2ProbeTracking(config:                     TrackingConfig): F[CommandResult]
  def pwfs2Park: F[CommandResult]
  def pwfs2Follow(enable:                            Boolean): F[CommandResult]
  def pwfs2Unwrap: F[CommandResult]
  def oiwfsTarget(target:                            Target): F[CommandResult]
  def oiwfsProbeTracking(config:                     TrackingConfig): F[CommandResult]
  def oiwfsPark: F[CommandResult]
  def oiwfsFollow(enable:                            Boolean): F[CommandResult]
  def enableGuide(config:                            TelescopeGuideConfig): F[CommandResult]
  def disableGuide: F[CommandResult]
  def pwfs1Observe(period:                           TimeSpan): F[CommandResult]
  def pwfs1StopObserve: F[CommandResult]
  def pwfs1CircularBuffer(enable:                    Boolean): F[CommandResult]
  def pwfs1QlMode(mode:                              QlMode): F[CommandResult]
  def getPwfs1Configuration: F[WfsConfiguration]
  def getPwfs1ConfigurationStream: Resource[F, Stream[F, WfsConfiguration]]
  def pwfs2Observe(period:                           TimeSpan): F[CommandResult]
  def pwfs2StopObserve: F[CommandResult]
  def pwfs2CircularBuffer(enable:                    Boolean): F[CommandResult]
  def pwfs2QlMode(mode:                              QlMode): F[CommandResult]
  def getPwfs2Configuration: F[WfsConfiguration]
  def getPwfs2ConfigurationStream: Resource[F, Stream[F, WfsConfiguration]]
  def oiwfsObserve(period:                           TimeSpan): F[CommandResult]
  def oiwfsStopObserve: F[CommandResult]
  def oiwfsCircularBuffer(enable:                    Boolean): F[CommandResult]
  def oiwfsQlMode(mode:                              QlMode): F[CommandResult]
  def getOiwfsConfiguration: F[WfsConfiguration]
  def getOiwfsConfigurationStream: Resource[F, Stream[F, WfsConfiguration]]
  def acObserve(period:                              TimeSpan): F[CommandResult]
  def acStopObserve: F[CommandResult]
  def swapTarget(swapConfig:                         SwapConfig): F[CommandResult]
  def restoreTarget(config:                          TcsConfig): F[CommandResult]
  def m1Park: F[CommandResult]
  def m1Unpark: F[CommandResult]
  def m1OpenLoopOff: F[CommandResult]
  def m1OpenLoopOn: F[CommandResult]
  def m1ZeroFigure: F[CommandResult]
  def m1LoadAoFigure: F[CommandResult]
  def m1LoadNonAoFigure: F[CommandResult]
  def lightPathConfig(from:                          LightSource, to:              LightSink): F[CommandResult]
  def acquisitionAdj(offset:                         Offset, iaa:                  Option[Angle], ipa: Option[Angle]): F[CommandResult]
  def wfsSky(wfs:                                    GuideProbe, period:           TimeSpan): F[CommandResult]
  def targetAdjust(
    target:            VirtualTelescope,
    handsetAdjustment: HandsetAdjustment,
    openLoops:         Boolean
  ): F[CommandResult]
  def targetOffsetAbsorb(target:                     VirtualTelescope): F[CommandResult]
  def targetOffsetClear(target:                      VirtualTelescope, openLoops:  Boolean): F[CommandResult]
  def originAdjust(handsetAdjustment:                HandsetAdjustment, openLoops: Boolean): F[CommandResult]
  def offset(offset:                                 Offset, guiding:              Boolean): F[CommandResult]
  def centralWavelength(wavelength:                  Wavelength): F[CommandResult]
  def configureStep(
    offset:     Option[Offset],
    wavelength: Option[Wavelength],
    lightPath:  Option[LightPath],
    defocus:    Option[Distance],
    guiding:    Boolean
  ): F[CommandResult]
  def originOffsetAbsorb: F[CommandResult]
  def originOffsetClear(openLoops:                   Boolean): F[CommandResult]
  def pointingAdjust(handsetAdjustment:              HandsetAdjustment): F[CommandResult]
  def pointingOffsetClearLocal: F[CommandResult]
  def pointingOffsetAbsorbGuide: F[CommandResult]
  def pointingOffsetClearGuide: F[CommandResult]
  // AC/HRFWS setup
  def acLens(l:                                      AcLens): F[CommandResult]
  def acNdFilter(nd:                                 AcNdFilter): F[CommandResult]
  def acFilter(flt:                                  AcFilter): F[CommandResult]
  def acWindowSize(wnd:                              AcWindow): F[CommandResult]
  // PWFS1 mechanisms
  def pwfs1Filter(filter:                            PwfsFilter): F[CommandResult]
  def pwfs1FieldStop(fieldStop:                      PwfsFieldStop): F[CommandResult]
  // PWFS2 mechanisms
  def pwfs2Filter(filter:                            PwfsFilter): F[CommandResult]
  def pwfs2FieldStop(fieldStop:                      PwfsFieldStop): F[CommandResult]
  // AG commmands
  def agScienceFoldPark: F[CommandResult]
  def agPickoffMirrorPark: F[CommandResult]
  def agAoFoldPark: F[CommandResult]
  def agAllPark: F[CommandResult]
  // ECS commands
  def ecsEnableDome(mode:                            DomeMode): F[CommandResult]
  def ecsDisableDome: F[CommandResult]
  def ecsDomePark: F[CommandResult]
  def ecsEnableShutters(mode:                        ShutterMode): F[CommandResult]
  def ecsDisableShutters: F[CommandResult]
  def ecsShuttersPark: F[CommandResult]
  def ecsMoveEastVentGate(position:                  IntPercent): F[CommandResult]
  def ecsCloseEastVentGate: F[CommandResult]
  def ecsMoveWestVentGate(position:                  IntPercent): F[CommandResult]
  def ecsCloseWestVentGate: F[CommandResult]

  def getGuideState: F[GuideState]
  def getGuidersQuality: F[GuidersQualityValues]
  def getTelescopeState: F[TelescopeState]
  def getNavigateState: F[NavigateState]
  def getNavigateStateStream: Stream[F, NavigateState]
  def getInstrumentPort(instrument: Instrument): F[Option[Int]]
  def getGuideDemand: F[GuideConfig]
  def getWfsGuideStates: F[WfsGuideStates]
  def getTargetAdjustments: F[TargetOffsets]
  def getPointingOffset: F[PointingCorrections]
  def getOriginOffset: F[FocalPlaneOffset]
  def getAcMechsState: F[AcMechsState]
  def getPwfs1MechsState: F[PwfsMechsState]
  def getPwfs2MechsState: F[PwfsMechsState]
  def getBafflesState: F[BafflesState]

  def refreshEphemerides(date: Option[LocalDate]): F[CommandResult]
}

object NavigateEngine {

  def failIfNoEmitsWithin[F[_]: Async, A](
    timeout: FiniteDuration,
    msg:     String
  ): Pipe[F, A, A] = in => {
    import scala.concurrent.TimeoutException
    val now = Temporal[F].realTime

    Stream.eval(now.flatMap(Ref[F].of)).flatMap { lastActivityAt =>
      in.evalTap(_ => now.flatMap(lastActivityAt.set))
        .concurrently {
          Stream.repeatEval {
            (now, lastActivityAt.get)
              .mapN(_ - _)
              .flatMap { elapsed =>
                val t = timeout - elapsed

                Sync[F]
                  .raiseError[Unit](new TimeoutException(msg))
                  .whenA(t <= 0.nanos) >> Temporal[F].sleep(t)
              }
          }
        }
    }
  }

  private case class NavigateEngineImpl[F[_]: {Temporal, Logger, Async}](
    site:     Site,
    services: Services[F],
    conf:     NavigateEngineConfiguration,
    engine:   StateEngine[F, State, NavigateEvent],
    stateRef: Ref[F, State],
    topic:    Topic[F, NavigateState]
  ) extends NavigateEngine[F]
      with Http4sClientDsl[F] {

    // We want to support some retries to observe
    private val clientWithRetry = {
      val max    = 4
      val policy = RetryPolicy[F] { (attempts: Int) =>
        if (attempts >= max) None
        else 10.milliseconds.some
      }
      Retry[F](policy)(services.client)
    }

    private def postTelescopeGuideConfig(gc: GuideConfig): F[Unit] = {
      val postRequest: Request[F] =
        POST(
          gc.asJson,
          conf.observe / "api" / "observe" / "guide"
        )

      // Update guide state in observe if not simulated
      (Logger[F].info(s"Update guide state in observe") *>
        clientWithRetry
          .expect[String](postRequest)
          .void
          .handleErrorWith(r => Logger[F].error(r)("Error posting guide configuration to observe")))
        .whenA(conf.subsystemControlModes.observe === SubsystemControlMode.Full)
    }

    private def navigateState(s: State): NavigateState =
      NavigateState(onSwappedTarget = s.onSwappedTarget)

    override def eventStream: Stream[F, NavigateEvent] =
      engine.process(startState).evalMap { case (s, o) =>
        logEvent(o) *> stateRef.set(s) *> topic.publish1(navigateState(s)).as(o)
      }

    override def mcsPark: F[CommandResult] =
      simpleCommand(engine, McsPark, services.tcsCommon.mcsPark)

    override def mcsFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, McsFollow(enable), services.tcsCommon.mcsFollow(enable))

    override def scsFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, ScsFollow(enable), services.tcsCommon.scsFollow(enable))

    override def rotStop(useBrakes: Boolean): F[CommandResult] =
      simpleCommand(engine, CrcsStop(useBrakes), services.tcsCommon.rotStop(useBrakes))

    override def rotPark: F[CommandResult] =
      simpleCommand(engine, CrcsPark, services.tcsCommon.rotPark)

    override def rotFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, CrcsFollow(enable), services.tcsCommon.rotFollow(enable))

    override def rotMove(angle: RotatorAngle): F[CommandResult] =
      simpleCommand(engine, CrcsMove(angle), services.tcsCommon.rotMove(angle))

    // tcsConfig/slew apply (or clear) per-guider tracking as part of the TcsConfig; keep
    // the remembered last-applied tracking config for each guider up to date, the same way
    // pwfs1ProbeTracking/pwfs2ProbeTracking/oiwfsProbeTracking do for their own mutations.
    // A guider not included in the config is not tracking.
    private def recordWfsTrackingConfig(config: TcsConfig): State => State =
      _.focus(_.wfsTrackingConfig).replace(
        WfsGuideStates(
          config.pwfs1.fold(TrackingConfig.noTracking)(_.tracking),
          config.pwfs2.fold(TrackingConfig.noTracking)(_.tracking),
          config.oiwfs.fold(TrackingConfig.noTracking)(_.tracking)
        )
      )

    override def tcsConfig(config: TcsConfig): F[CommandResult] = command(
      engine,
      TcsConfigure(config),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(false))
        .flatMap(_ => cats.data.State.modify[State](recordWfsTrackingConfig(config)))
        .as(stateRef.get.flatMap(st => services.tcsCommon.tcsConfig(config)(st.guideConfig)))
    )

    override def slew(
      slewOptions: SlewOptions,
      tcsConfig:   TcsConfig,
      oid:         Option[Observation.Id]
    ): F[CommandResult] =
      Logger[F].info(s"Starting slew to ${oid}") *>
        command(
          engine,
          Slew(slewOptions, tcsConfig, oid),
          cats.data.State
            .modify[State](
              _.focus(_.onSwappedTarget).replace(false)
            )
            .flatMap(_ => cats.data.State.modify[State](recordWfsTrackingConfig(tcsConfig)))
            .flatMap(_ =>
              cats.data.State
                .modify[State](
                  _.focus(_.guideConfig.tcsGuide)
                    .replace(GuideOff)
                )
                .whenA(slewOptions.stopGuide.value)
            )
            .as(
              services.tcsCommon
                .slew(slewOptions, tcsConfig)
                // if succesful send an event to the odb
                .flatTap(_ => oid.traverse_(services.odb.addSlewEvent(_, SlewStage.StartSlew)))
            )
        )

    override def swapTarget(swapConfig: SwapConfig): F[CommandResult] = command(
      engine,
      SwapTarget(swapConfig),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(true))
        .as(
          services.tcsCommon.swapTarget(swapConfig)
        )
    )

    override def restoreTarget(config: TcsConfig): F[CommandResult] = command(
      engine,
      RestoreTarget(config),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(false))
        .as(
          services.tcsCommon.restoreTarget(config)
        )
    )

    override def instrumentSpecifics(
      instrumentSpecificsParams: InstrumentSpecifics
    ): F[CommandResult] =
      simpleCommand(
        engine,
        InstSpecifics(instrumentSpecificsParams),
        services.tcsCommon.instrumentSpecifics(instrumentSpecificsParams)
      )

    override def pwfs1Target(target: Target): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Target(target),
      services.tcsCommon.pwfs1Target(target)
    )

    override def pwfs1ProbeTracking(config: TrackingConfig): F[CommandResult] = command(
      engine,
      Pwfs1ProbeTracking(config),
      cats.data.State
        .modify[State](_.focus(_.wfsTrackingConfig.pwfs1).replace(config))
        .as(services.tcsCommon.pwfs1ProbeTracking(config))
    )

    override def pwfs1Park: F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Park,
      services.tcsCommon.pwfs1Park
    )

    override def pwfs1Follow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Follow(enable),
      services.tcsCommon.pwfs1Follow(enable)
    )

    override def pwfs2Target(target: Target): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Target(target),
      services.tcsCommon.pwfs2Target(target)
    )

    override def pwfs2ProbeTracking(config: TrackingConfig): F[CommandResult] = command(
      engine,
      Pwfs2ProbeTracking(config),
      cats.data.State
        .modify[State](_.focus(_.wfsTrackingConfig.pwfs2).replace(config))
        .as(services.tcsCommon.pwfs2ProbeTracking(config))
    )

    override def pwfs2Park: F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Park,
      services.tcsCommon.pwfs2Park
    )

    override def pwfs2Follow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Follow(enable),
      services.tcsCommon.pwfs2Follow(enable)
    )

    override def oiwfsTarget(target: Target): F[CommandResult] = simpleCommand(
      engine,
      OiwfsTarget(target),
      services.tcsCommon.oiwfsTarget(target)
    )

    override def oiwfsProbeTracking(config: TrackingConfig): F[CommandResult] = command(
      engine,
      OiwfsProbeTracking(config),
      cats.data.State
        .modify[State](_.focus(_.wfsTrackingConfig.oiwfs).replace(config))
        .as(services.tcsCommon.oiwfsProbeTracking(config))
    )

    override def oiwfsPark: F[CommandResult] = simpleCommand(
      engine,
      OiwfsPark,
      services.tcsCommon.oiwfsPark
    )

    override def oiwfsFollow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      OiwfsFollow(enable),
      services.tcsCommon.oiwfsFollow(enable)
    )

    override def rotTrackingConfig(config: RotatorTrackConfig): F[CommandResult] =
      simpleCommand(
        engine,
        RotatorTrackingConfig(config),
        services.tcsCommon.rotTrackingConfig(config)
      )

    override def enableGuide(config: TelescopeGuideConfig): F[CommandResult] =
      command(
        engine,
        EnableGuide(config),
        cats.data.State
          .modify[State](_.focus(_.guideConfig.tcsGuide).replace(config))
          .as(
            services.tcsCommon.enableGuide(config)
          )
      ) <* postTelescopeGuideConfig(GuideConfig(config, None))

    override def disableGuide: F[CommandResult] = command(
      engine,
      DisableGuide,
      cats.data.State
        .modify[State](
          _.focus(_.guideConfig.tcsGuide)
            .replace(GuideOff)
        )
        .as(
          services.tcsCommon.disableGuide
        )
    ) <* postTelescopeGuideConfig(GuideConfig.defaultGuideConfig)

    override def pwfs1Observe(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Observe(period),
      services.tcsCommon.pwfs1Observe(period)
    )

    override def pwfs1StopObserve: F[CommandResult] = simpleCommand(
      engine,
      Pwfs1StopObserve,
      services.tcsCommon.pwfs1StopObserve
    )

    override def pwfs2Observe(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Observe(period),
      services.tcsCommon.pwfs2Observe(period)
    )

    override def pwfs2StopObserve: F[CommandResult] = simpleCommand(
      engine,
      Pwfs2StopObserve,
      services.tcsCommon.pwfs2StopObserve
    )

    override def oiwfsObserve(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      OiwfsObserve(period),
      services.tcsCommon.oiwfsObserve(period)
    )

    override def oiwfsStopObserve: F[CommandResult] = simpleCommand(
      engine,
      OiwfsStopObserve,
      services.tcsCommon.oiwfsStopObserve
    )

    override def acObserve(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      AcObserve(period),
      services.tcsCommon.hrwfsObserve(period)
    )

    override def acStopObserve: F[CommandResult] = simpleCommand(
      engine,
      AcStopObserve,
      services.tcsCommon.hrwfsStopObserve
    )

    override def getGuideState: F[GuideState] = services.tcsCommon.getGuideState

    override def getGuidersQuality: F[GuidersQualityValues] = services.tcsCommon.getGuideQuality

    override def getTelescopeState: F[TelescopeState] = services.tcsCommon.getTelescopeState

    override def getNavigateState: F[NavigateState] =
      stateRef.get.map(s => NavigateState(s.onSwappedTarget))

    override def getNavigateStateStream: Stream[F, NavigateState] =
      topic
        .subscribe(1024)
        .mapAccumulate[Option[NavigateState], Option[NavigateState]](none) { (acc, ss) =>
          (ss.some, if (acc.contains(ss)) none else ss.some)
        }
        .map(_._2)
        .unNone

    override def m1Park: F[CommandResult] = simpleCommand(
      engine,
      M1Park,
      services.tcsCommon.m1Park
    )

    override def m1Unpark: F[CommandResult] = simpleCommand(
      engine,
      M1Unpark,
      services.tcsCommon.m1Unpark
    )

    override def m1OpenLoopOff: F[CommandResult] = simpleCommand(
      engine,
      M1OpenLoopOff,
      services.tcsCommon.m1UpdateOff
    )

    override def m1OpenLoopOn: F[CommandResult] = simpleCommand(
      engine,
      M1OpenLoopOn,
      services.tcsCommon.m1UpdateOn
    )

    override def m1ZeroFigure: F[CommandResult] = simpleCommand(
      engine,
      M1ZeroFigure,
      services.tcsCommon.m1ZeroFigure
    )

    override def m1LoadAoFigure: F[CommandResult] = simpleCommand(
      engine,
      M1LoadAoFigure,
      services.tcsCommon.m1LoadAoFigure
    )

    override def m1LoadNonAoFigure: F[CommandResult] = simpleCommand(
      engine,
      M1LoadNonAoFigure,
      services.tcsCommon.m1LoadNonAoFigure
    )

    override def lightPathConfig(from: LightSource, to: LightSink): F[CommandResult] =
      simpleCommand(
        engine,
        LightPathConfig(from, to),
        services.tcsCommon.lightPath(from, to)
      )

    override def getInstrumentPort(instrument: Instrument): F[Option[Int]] =
      services.tcsCommon.getInstrumentPort(instrument)

    override def acquisitionAdj(
      offset: Offset,
      iaa:    Option[Angle],
      ipa:    Option[Angle]
    ): F[CommandResult] =
      simpleCommand(
        engine,
        AcquisitionAdjust(offset, ipa, iaa),
        stateRef.get.flatMap(s =>
          services.tcsCommon.acquisitionAdj(offset, ipa, iaa)(s.guideConfig)
        )
      )

    override def wfsSky(wfs: GuideProbe, period: TimeSpan): F[CommandResult] = wfs match {
      case enums.GuideProbe.PWFS1                                        =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => services.tcsCommon.pwfs1Sky(period)(s.guideConfig))
        )
      case enums.GuideProbe.PWFS2                                        =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => services.tcsCommon.pwfs2Sky(period)(s.guideConfig))
        )
      case enums.GuideProbe.GmosOIWFS | enums.GuideProbe.Flamingos2OIWFS =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => services.tcsCommon.oiwfsSky(period)(s.guideConfig))
        )
      case enums.GuideProbe.AltairAOWFS                                  =>
        ???
    }

    override def getGuideDemand: F[GuideConfig] = stateRef.get.map(_.guideConfig)

    override def getWfsGuideStates: F[WfsGuideStates] = stateRef.get.map(_.wfsTrackingConfig)

    override def getTargetAdjustments: F[TargetOffsets] = services.tcsCommon.getTargetAdjustments

    override def getPointingOffset: F[PointingCorrections] =
      services.tcsCommon.getPointingCorrections

    override def getOriginOffset: F[FocalPlaneOffset] = services.tcsCommon.getOriginOffset

    override def targetAdjust(
      target:            VirtualTelescope,
      handsetAdjustment: HandsetAdjustment,
      openLoops:         Boolean
    ): F[CommandResult] =
      simpleCommand(
        engine,
        TargetAdjust(target, handsetAdjustment, openLoops),
        stateRef.get.flatMap(s =>
          services.tcsCommon.targetAdjust(target, handsetAdjustment, openLoops)(s.guideConfig)
        )
      )

    override def originAdjust(
      handsetAdjustment: HandsetAdjustment,
      openLoops:         Boolean
    ): F[CommandResult] =
      simpleCommand(
        engine,
        OriginAdjust(handsetAdjustment, openLoops),
        stateRef.get.flatMap(s =>
          services.tcsCommon.originAdjust(handsetAdjustment, openLoops)(s.guideConfig)
        )
      )

    override def offset(offset: Offset, guiding: Boolean): F[CommandResult] =
      simpleCommand(
        engine,
        TelescopeOffset(offset, guiding),
        stateRef.get.flatMap(s =>
          services.tcsCommon.offset(offset, guiding)(s.guideConfig, s.wfsTrackingConfig)
        )
      )

    override def centralWavelength(wavelength: Wavelength): F[CommandResult] =
      simpleCommand(
        engine,
        CentralWavelength(wavelength),
        services.tcsCommon.centralWavelength(wavelength)
      )

    override def configureStep(
      offset:     Option[Offset],
      wavelength: Option[Wavelength],
      lightPath:  Option[LightPath],
      defocus:    Option[Distance],
      guiding:    Boolean
    ): F[CommandResult] =
      simpleCommand(
        engine,
        ConfigureStep(offset, wavelength, lightPath, defocus, guiding),
        stateRef.get.flatMap(s =>
          services.tcsCommon.configureStep(offset, wavelength, lightPath, defocus, guiding)(
            s.guideConfig,
            s.wfsTrackingConfig
          )
        )
      )

    override def pointingAdjust(handsetAdjustment: HandsetAdjustment): F[CommandResult] =
      simpleCommand(
        engine,
        PointingAdjust(handsetAdjustment),
        services.tcsCommon.pointingAdjust(handsetAdjustment)
      )

    override def targetOffsetAbsorb(target: VirtualTelescope): F[CommandResult] =
      simpleCommand(
        engine,
        TargetOffsetAbsorb(target),
        services.tcsCommon.targetOffsetAbsorb(target)
      )

    override def targetOffsetClear(target: VirtualTelescope, openLoops: Boolean): F[CommandResult] =
      simpleCommand(
        engine,
        TargetOffsetClear(target, openLoops),
        stateRef.get.flatMap(s =>
          services.tcsCommon.targetOffsetClear(target, openLoops)(s.guideConfig)
        )
      )

    override def originOffsetAbsorb: F[CommandResult] =
      simpleCommand(
        engine,
        OriginOffsetAbsorb,
        services.tcsCommon.originOffsetAbsorb
      )

    override def originOffsetClear(openLoops: Boolean): F[CommandResult] =
      simpleCommand(
        engine,
        OriginOffsetClear(openLoops),
        stateRef.get.flatMap(s => services.tcsCommon.originOffsetClear(openLoops)(s.guideConfig))
      )

    override def pointingOffsetClearLocal: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetClearLocal,
        services.tcsCommon.pointingOffsetClearLocal
      )

    override def pointingOffsetAbsorbGuide: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetAbsorbGuide,
        services.tcsCommon.pointingOffsetAbsorbGuide
      )

    override def pointingOffsetClearGuide: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetClearGuide,
        services.tcsCommon.pointingOffsetClearGuide
      )

    override def acLens(l: AcLens): F[CommandResult] = simpleCommand(
      engine,
      AcSetLens(l),
      services.tcsCommon.acCommands.lens(l)
    )

    override def acNdFilter(nd: AcNdFilter): F[CommandResult] = simpleCommand(
      engine,
      AcSetNdFilter(nd),
      services.tcsCommon.acCommands.ndFilter(nd)
    )

    override def acFilter(flt: AcFilter): F[CommandResult] = simpleCommand(
      engine,
      AcSetFilter(flt),
      services.tcsCommon.acCommands.filter(flt)
    )

    override def acWindowSize(wnd: AcWindow): F[CommandResult] = simpleCommand(
      engine,
      AcSetWindowSize(wnd),
      services.tcsCommon.acCommands.windowSize(wnd)
    )

    override def getAcMechsState: F[AcMechsState] = services.tcsCommon.acCommands.getState

    override def pwfs1Filter(filter: PwfsFilter): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Filter(filter),
      services.tcsCommon.pwfs1Mechs.filter(filter)
    )

    override def pwfs1FieldStop(fieldStop: PwfsFieldStop): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1FieldStop(fieldStop),
      services.tcsCommon.pwfs1Mechs.fieldStop(fieldStop)
    )

    override def pwfs2Filter(filter: PwfsFilter): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Filter(filter),
      services.tcsCommon.pwfs2Mechs.filter(filter)
    )

    override def pwfs2FieldStop(fieldStop: PwfsFieldStop): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2FieldStop(fieldStop),
      services.tcsCommon.pwfs2Mechs.fieldStop(fieldStop)
    )

    override def getPwfs1MechsState: F[PwfsMechsState] = services.tcsCommon.getPwfs1Mechs

    override def getPwfs2MechsState: F[PwfsMechsState] = services.tcsCommon.getPwfs2Mechs

    override def getBafflesState: F[BafflesState] = services.tcsCommon.getBaffles

    override def pwfs1CircularBuffer(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1CircularBuffer(enable),
      services.tcsCommon.pwfs1CircularBuffer(enable)
    )

    override def pwfs2CircularBuffer(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2CircularBuffer(enable),
      services.tcsCommon.pwfs2CircularBuffer(enable)
    )

    override def oiwfsCircularBuffer(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      OiwfsCircularBuffer(enable),
      services.tcsCommon.oiwfsCircularBuffer(enable)
    )

    override def getPwfs1Configuration: F[WfsConfiguration] = services.tcsCommon.getPwfs1Config

    override def getPwfs1ConfigurationStream: Resource[F, Stream[F, WfsConfiguration]] =
      services.tcsCommon.pwfs1ConfigStream

    override def getPwfs2Configuration: F[WfsConfiguration] = services.tcsCommon.getPwfs2Config

    override def getPwfs2ConfigurationStream: Resource[F, Stream[F, WfsConfiguration]] =
      services.tcsCommon.pwfs2ConfigStream

    override def getOiwfsConfiguration: F[WfsConfiguration] = services.tcsCommon.getOiwfsConfig

    override def getOiwfsConfigurationStream: Resource[F, Stream[F, WfsConfiguration]] =
      services.tcsCommon.oiwfsConfigStream

    override def refreshEphemerides(date: Option[LocalDate]): F[CommandResult] = for {
      _     <- logEvent(NavigateEvent.CommandStart(NavigateCommand.RefreshEphemerides(date)))
      start <-
        date
          .map(_.pure[F])
          .getOrElse(
            Async[F]
              .delay(Instant.now)
              .map(x =>
                LocalObservingNight.localDate.get(LocalObservingNight.fromSiteAndInstant(site, x))
              )
          )
      x     <- EphemerisUpdater
                 .build(site, conf.ephemerisFolder, services.odb, services.horizonsClient)
                 .refreshEphemerides(DateInterval.between(start, start.plusDays(1)))
                 .attempt
                 .map {
                   case Left(value)  =>
                     NavigateEvent.CommandEnd(NavigateCommand.RefreshEphemerides(date),
                                              CommandResult.CommandFailure(value.getMessage)
                     )
                   case Right(value) =>
                     NavigateEvent.CommandEnd(NavigateCommand.RefreshEphemerides(date),
                                              CommandResult.CommandSuccess
                     )
                 }
      _     <- logEvent(x)
    } yield x.result

    override def ecsEnableDome(mode: DomeMode): F[CommandResult] =
      simpleCommand(
        engine,
        EcsEnableDome(mode),
        services.tcsCommon.ecsEnableDome(mode)
      )

    override def ecsDisableDome: F[CommandResult] =
      simpleCommand(
        engine,
        EcsDisableDome,
        services.tcsCommon.ecsDisableDome
      )

    override def ecsDomePark: F[CommandResult] =
      simpleCommand(engine, EcsDomePark, services.tcsCommon.ecsDomePark)

    override def ecsEnableShutters(mode: ShutterMode): F[CommandResult] =
      simpleCommand(engine, EcsEnableShutters(mode), services.tcsCommon.ecsEnableShutters(mode))

    override def ecsDisableShutters: F[CommandResult] =
      simpleCommand(engine, EcsDisableShutters, services.tcsCommon.ecsDisableShutters)

    override def ecsShuttersPark: F[CommandResult] =
      simpleCommand(engine, EcsShuttersPark, services.tcsCommon.ecsShuttersPark)

    override def ecsMoveEastVentGate(position: IntPercent): F[CommandResult] = simpleCommand(
      engine,
      EcsEastVentGateMove(position),
      services.tcsCommon.ecsMoveEastVentGate(position)
    )

    override def ecsCloseEastVentGate: F[CommandResult] =
      simpleCommand(engine, EcsCloseEastVentGate, services.tcsCommon.ecsCloseEastVentGate)

    override def ecsMoveWestVentGate(position: IntPercent): F[CommandResult] = simpleCommand(
      engine,
      EcsWestVentGateMove(position),
      services.tcsCommon.ecsMoveWestVentGate(position)
    )

    override def ecsCloseWestVentGate: F[CommandResult] =
      simpleCommand(engine, EcsCloseWestVentGate, services.tcsCommon.ecsCloseWestVentGate)

    override def mcsUnwrap: F[CommandResult] =
      simpleCommand(engine, MountUnwrap, services.tcsCommon.azimuthUnwrap)

    override def rotUnwrap: F[CommandResult] =
      simpleCommand(engine, CrcsUnwrap, services.tcsCommon.rotUnwrap)

    override def pwfs1Unwrap: F[CommandResult] =
      simpleCommand(engine, Pwfs1Unwrap, services.tcsCommon.pwfs1Unwrap)

    override def pwfs2Unwrap: F[CommandResult] =
      simpleCommand(engine, Pwfs2Unwrap, services.tcsCommon.pwfs2Unwrap)

    override def agScienceFoldPark: F[CommandResult] = simpleCommand(
      engine,
      AgScienceFoldPark,
      services.tcsCommon.agScienceFoldPark
    )

    override def agPickoffMirrorPark: F[CommandResult] = simpleCommand(
      engine,
      AgPickoffMirrorPark,
      services.tcsCommon.agPickoffMirrorPark
    )

    override def agAoFoldPark: F[CommandResult] = simpleCommand(
      engine,
      AgAoFoldPark,
      services.tcsCommon.agAoFoldPark
    )

    override def agAllPark: F[CommandResult] = simpleCommand(
      engine,
      AgAllPark,
      services.tcsCommon.agAllPark
    )

    override def pwfs1QlMode(mode: QlMode): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1QlMode(mode),
      services.tcsCommon.pwfs1QlMode(mode)
    )

    override def pwfs2QlMode(mode: QlMode): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2QlMode(mode),
      services.tcsCommon.pwfs2QlMode(mode)
    )

    override def oiwfsQlMode(mode: QlMode): F[CommandResult] = simpleCommand(
      engine,
      OiwfsQlMode(mode),
      services.tcsCommon.oiwfsQlMode(mode)
    )
  }

  def build[F[_]: {Temporal, Logger, Async}](
    site:     Site,
    services: Services[F],
    conf:     NavigateEngineConfiguration
  ): F[NavigateEngine[F]] = for {
    eng <- StateEngine.build[F, State, NavigateEvent]
    ref <- Ref.of[F, State](startState)
    top <- Topic[F, NavigateState]
  } yield NavigateEngineImpl[F](site, services, conf, eng, ref, top)

  case class WfsConfigState(
    period:          Option[TimeSpan],
    configuredForQl: Option[Boolean]
  )

  case class State(
    guideConfig:        GuideConfig,
    onSwappedTarget:    Boolean,
    wfsTrackingConfig:  WfsGuideStates,
    commandsInProgress: List[NavigateCommand]
  )

  val startState: State = State(
    guideConfig = GuideConfig.defaultGuideConfig,
    onSwappedTarget = false,
    wfsTrackingConfig = WfsGuideStates(TrackingConfig.noTracking,
                                       TrackingConfig.noTracking,
                                       TrackingConfig.noTracking
    ),
    commandsInProgress = Nil
  )

  /**
   * This is used for simple commands, just an F that executes the command when evaluated, producing
   * a result. It relies on command().
   *
   * @param engine
   *   The state machine.
   * @param cmdType
   *   : The command type, used for logs.
   * @param cmd
   *   : The actual command, wrapped in effect F.
   * @tparam F
   *   : Type of effect that wraps the command execution.
   * @return
   *   Effect that, when evaluated, will schedule the execution of the command in the state machine.
   */
  private def simpleCommand[F[_]: Async](
    engine:  StateEngine[F, State, NavigateEvent],
    cmdType: NavigateCommand,
    cmd:     F[ApplyCommandResult]
  ): F[CommandResult] = command(
    engine,
    cmdType,
    cats.data.State.pure[State, F[ApplyCommandResult]](cmd)
  )

  /**
   * Similar to simple command, but here the command as access to the global state.
   *
   * @param engine
   *   The state machine.
   * @param cmdType
   *   : The command type, used for logs.
   * @param cmd
   *   : The actual command, wrapped in a State.
   * @tparam F
   *   : Type of effect that wraps the command execution.
   * @return
   *   Effect that, when evaluated, will schedule the execution of the command in the state machine.
   */
  private def command[F[_]: Async](
    engine:  StateEngine[F, State, NavigateEvent],
    cmdType: NavigateCommand,
    cmd:     cats.data.State[State, F[ApplyCommandResult]]
  ): F[CommandResult] = Async[F].async { (f: Either[Throwable, CommandResult] => Unit) =>
    // Start the async calculation by putting a message in the input queue
    engine
      .offer(
        // State processing. Several commands can be running at the same time, unless the rules in
        // CommandConflicts say otherwise.
        engine.getState.flatMap { st =>
          st.commandsInProgress.collectFirstSome(running =>
            CommandConflicts.between(running, cmdType).tupleLeft(running)
          ) match {
            case None                 =>
              engine
                // First, register the command as in progress
                .modifyState(_.focus(_.commandsInProgress).modify(cmdType :: _))
                // And produce the start command event
                .as(CommandStart(cmdType).some) <*
                // Flatmap with another Handler. The output is ignored, we only care about the stream that will execute the command later
                Handler(
                  // Still in State world, cmd checks the current state and produces an F that will execute the command
                  cmd.map(x =>
                    Handler.RetVal(
                      NullEvent,
                      Stream
                        .eval(
                          // x is the F that executes the command. It will be evaluated in the main Stream join and produce a result.
                          x.attempt
                            .map(cmdResultToNavigateEvent)
                            .map(z =>
                              // Here we build the State that will be evaluated at the end
                              Event(
                                engine
                                  // Remove one occurrence of the command from the list in progress
                                  .modifyState(
                                    _.focus(_.commandsInProgress).modify(_.diff(List(cmdType)))
                                  )
                                  // And create an output event with the command result
                                  .as(CommandEnd(cmdType, z).some) <*
                                  // Only then pass the result to the Async callback, so a command issued
                                  // as soon as this one returns is not rejected as in progress
                                  Handler.fromStream[F, State, Event[F, State, NavigateEvent]](
                                    Stream.exec(Async[F].delay(f(z.asRight)))
                                  )
                              )
                            )
                        )
                        .some
                    )
                  )
                )
            case Some((running, why)) =>
              // A conflicting command is in progress. We schedule the call to the Async callback and produce the output event.
              val r = CommandResult.CommandFailure(
                s"Cannot execute ${cmdType.show} because ${running.show} is in progress: $why"
              )
              engine.lift(Async[F].delay(f(r.asRight)).as(CommandEnd(cmdType, r)))
          }
        }
      )
      .as(none)
  }

  private def cmdResultToNavigateEvent(
    result: Either[Throwable, ApplyCommandResult]
  ): CommandResult =
    result match {
      case Right(ApplyCommandResult.Paused)                  => CommandResult.CommandPaused
      case Right(ApplyCommandResult.Completed)               => CommandResult.CommandSuccess
      case Left(e: GeminiApplyCommand.ApplyCommandException) =>
        CommandResult.CommandFailure(e.getLocalizedMessage)
      case Left(e: TimeoutException)                         =>
        CommandResult.CommandFailure(s"Command timed out after ${e.getLocalizedMessage}")
      case Left(e)                                           =>
        CommandResult.CommandFailure(
          s"Command failed with error type ${e.getClass.getName}, message: ${e.getLocalizedMessage}"
        )
    }

  private def logEvent[F[_]: {Logger, Applicative}](x: NavigateEvent): F[Unit] =
    x match {
      case ConnectionOpenEvent(userDetails, clientId, serverVersion) =>
        Logger[F].info(s"ConnectionOpenEvent($userDetails, $clientId, $serverVersion)")
      case CommandStart(cmd)                                         => Logger[F].info(s"CommandStart(${cmd.show})")
      case CommandEnd(cmd, CommandResult.CommandSuccess)             =>
        Logger[F].info(s"CommandSuccess(${cmd.name})")
      case CommandEnd(cmd, CommandResult.CommandPaused)              =>
        Logger[F].info(s"CommandPaused(${cmd.name})")
      case CommandEnd(cmd, CommandResult.CommandFailure(msg))        =>
        Logger[F].error(s"CommandFailure(${cmd.name}, $msg)")
      case _                                                         => Applicative[F].unit
    }

  val GuideOff: TelescopeGuideConfig = TelescopeGuideConfig(MountGuideOption.MountGuideOff,
                                                            M1GuideConfig.M1GuideOff,
                                                            M2GuideConfig.M2GuideOff,
                                                            none,
                                                            none
  )

}
