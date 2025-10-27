// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.effect.Async
import cats.effect.Ref
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
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.Site
import lucuma.core.enums.SlewStage
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.GuideConfig
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.Observation
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.TimeSpan
import monocle.Lens
import monocle.syntax.all.focus
import mouse.all.*
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.BafflesState
import navigate.model.CommandResult
import navigate.model.FocalPlaneOffset
import navigate.model.HandsetAdjustment
import navigate.model.InstrumentSpecifics
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
import navigate.model.TcsConfig
import navigate.model.TrackingConfig
import navigate.model.config.ControlStrategy
import navigate.model.config.NavigateEngineConfiguration
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.DomeMode
import navigate.model.enums.LightSource
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.tcs.GuideState
import navigate.server.tcs.GuidersQualityValues
import navigate.server.tcs.TargetOffsets
import navigate.server.tcs.TelescopeState
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

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import NavigateEvent.NullEvent

trait NavigateEngine[F[_]] {
  val systems: Systems[F]
  def eventStream: Stream[F, NavigateEvent]
  def mcsPark: F[CommandResult]
  def mcsFollow(enable:                              Boolean): F[CommandResult]
  def scsFollow(enable:                              Boolean): F[CommandResult]
  def rotStop(useBrakes:                             Boolean): F[CommandResult]
  def rotPark: F[CommandResult]
  def rotFollow(enable:                              Boolean): F[CommandResult]
  def rotMove(angle:                                 RotatorAngle): F[CommandResult]
  def rotTrackingConfig(cfg:                         RotatorTrackConfig): F[CommandResult]
  def ecsCarouselMode(
    domeMode:      DomeMode,
    shutterMode:   ShutterMode,
    slitHeight:    Double,
    domeEnable:    Boolean,
    shutterEnable: Boolean
  ): F[CommandResult]
  def ecsVentGatesMove(gateEast:                     Double, westGate:             Double): F[CommandResult]
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
  def pwfs2Target(target:                            Target): F[CommandResult]
  def pwfs2ProbeTracking(config:                     TrackingConfig): F[CommandResult]
  def pwfs2Park: F[CommandResult]
  def pwfs2Follow(enable:                            Boolean): F[CommandResult]
  def oiwfsTarget(target:                            Target): F[CommandResult]
  def oiwfsProbeTracking(config:                     TrackingConfig): F[CommandResult]
  def oiwfsPark: F[CommandResult]
  def oiwfsFollow(enable:                            Boolean): F[CommandResult]
  def enableGuide(config:                            TelescopeGuideConfig): F[CommandResult]
  def disableGuide: F[CommandResult]
  def pwfs1Observe(period:                           TimeSpan): F[CommandResult]
  def pwfs1StopObserve: F[CommandResult]
  def pwfs2Observe(period:                           TimeSpan): F[CommandResult]
  def pwfs2StopObserve: F[CommandResult]
  def oiwfsObserve(period:                           TimeSpan): F[CommandResult]
  def oiwfsStopObserve: F[CommandResult]
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
  def lightpathConfig(from:                          LightSource, to:              LightSinkName): F[CommandResult]
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

  def getGuideState: F[GuideState]
  def getGuidersQuality: F[GuidersQualityValues]
  def getTelescopeState: F[TelescopeState]
  def getNavigateState: F[NavigateState]
  def getNavigateStateStream: Stream[F, NavigateState]
  def getInstrumentPort(instrument: Instrument): F[Option[Int]]
  def getGuideDemand: F[GuideConfig]
  def getTargetAdjustments: F[TargetOffsets]
  def getPointingOffset: F[PointingCorrections]
  def getOriginOffset: F[FocalPlaneOffset]
  def getAcMechsState: F[AcMechsState]
  def getPwfs1MechsState: F[PwfsMechsState]
  def getPwfs2MechsState: F[PwfsMechsState]
  def getBafflesState: F[BafflesState]
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
    systems:  Systems[F],
    conf:     NavigateEngineConfiguration,
    engine:   StateEngine[F, State, NavigateEvent],
    stateRef: Ref[F, State],
    topic:    Topic[F, NavigateState]
  ) extends NavigateEngine[F]
      with Http4sClientDsl[F] {

    // We want to support some retries to observe
    private val clientWithRetry = {
      val max             = 4
      var attemptsCounter = 1
      val policy          = RetryPolicy[F] { (attempts: Int) =>
        if (attempts >= max) None
        else {
          attemptsCounter = attemptsCounter + 1
          10.milliseconds.some
        }
      }
      Retry[F](policy)(systems.client)
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
        .whenA(conf.systemControl.observe === ControlStrategy.FullControl)
    }

    private def navigateState(s: State): NavigateState =
      NavigateState(onSwappedTarget = s.onSwappedTarget)

    override def eventStream: Stream[F, NavigateEvent] =
      engine.process(startState).evalMap { case (s, o) =>
        logEvent(o) *> stateRef.set(s) *> topic.publish1(navigateState(s)).as(o)
      }

    override def mcsPark: F[CommandResult] =
      simpleCommand(engine, McsPark, systems.tcsCommon.mcsPark)

    override def mcsFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, McsFollow(enable), systems.tcsCommon.mcsFollow(enable))

    override def scsFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, ScsFollow(enable), systems.tcsCommon.scsFollow(enable))

    override def rotStop(useBrakes: Boolean): F[CommandResult] =
      simpleCommand(engine, CrcsStop(useBrakes), systems.tcsCommon.rotStop(useBrakes))

    override def rotPark: F[CommandResult] =
      simpleCommand(engine, CrcsPark, systems.tcsCommon.rotPark)

    override def rotFollow(enable: Boolean): F[CommandResult] =
      simpleCommand(engine, CrcsFollow(enable), systems.tcsCommon.rotFollow(enable))

    override def rotMove(angle: RotatorAngle): F[CommandResult] =
      simpleCommand(engine, CrcsMove(angle), systems.tcsCommon.rotMove(angle))

    override def ecsCarouselMode(
      domeMode:      DomeMode,
      shutterMode:   ShutterMode,
      slitHeight:    Double,
      domeEnable:    Boolean,
      shutterEnable: Boolean
    ): F[CommandResult] = simpleCommand(
      engine,
      EcsCarouselMode(domeMode, shutterMode, slitHeight, domeEnable, shutterEnable),
      systems.tcsCommon.ecsCarouselMode(domeMode,
                                        shutterMode,
                                        slitHeight,
                                        domeEnable,
                                        shutterEnable
      )
    )

    // TODO
    override def ecsVentGatesMove(gateEast: Double, westGate: Double): F[CommandResult] =
      CommandResult.CommandFailure("Command ecsVentGatesMove not yet implemented.").pure[F]

    override def tcsConfig(config: TcsConfig): F[CommandResult] = command(
      engine,
      TcsConfigure(config),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(false))
        .as(systems.tcsCommon.tcsConfig(config))
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
            .flatMap(_ =>
              cats.data.State
                .modify[State](
                  _.focus(_.guideConfig.tcsGuide)
                    .replace(GuideOff)
                )
                .whenA(slewOptions.stopGuide.value)
            )
            .as(
              systems.tcsCommon
                .slew(slewOptions, tcsConfig)
                // if succesful send an event to the odb
                .flatTap(_ => oid.traverse_(systems.odb.addSlewEvent(_, SlewStage.StartSlew)))
            )
        )

    override def swapTarget(swapConfig: SwapConfig): F[CommandResult] = command(
      engine,
      SwapTarget(swapConfig),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(true))
        .as(
          systems.tcsCommon.swapTarget(swapConfig)
        )
    )

    override def restoreTarget(config: TcsConfig): F[CommandResult] = command(
      engine,
      RestoreTarget(config),
      cats.data.State
        .modify[State](_.focus(_.onSwappedTarget).replace(false))
        .as(
          systems.tcsCommon.restoreTarget(config)
        )
    )

    override def instrumentSpecifics(
      instrumentSpecificsParams: InstrumentSpecifics
    ): F[CommandResult] =
      simpleCommand(
        engine,
        InstSpecifics(instrumentSpecificsParams),
        systems.tcsCommon.instrumentSpecifics(instrumentSpecificsParams)
      )

    override def pwfs1Target(target: Target): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Target(target),
      systems.tcsCommon.pwfs1Target(target)
    )

    override def pwfs1ProbeTracking(config: TrackingConfig): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1ProbeTracking(config),
      systems.tcsCommon.pwfs1ProbeTracking(config)
    )

    override def pwfs1Park: F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Park,
      systems.tcsCommon.pwfs1Park
    )

    override def pwfs1Follow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Follow(enable),
      systems.tcsCommon.pwfs1Follow(enable)
    )

    override def pwfs2Target(target: Target): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Target(target),
      systems.tcsCommon.pwfs2Target(target)
    )

    override def pwfs2ProbeTracking(config: TrackingConfig): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2ProbeTracking(config),
      systems.tcsCommon.pwfs2ProbeTracking(config)
    )

    override def pwfs2Park: F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Park,
      systems.tcsCommon.pwfs2Park
    )

    override def pwfs2Follow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Follow(enable),
      systems.tcsCommon.pwfs2Follow(enable)
    )

    override def oiwfsTarget(target: Target): F[CommandResult] = simpleCommand(
      engine,
      OiwfsTarget(target),
      systems.tcsCommon.oiwfsTarget(target)
    )

    override def oiwfsProbeTracking(config: TrackingConfig): F[CommandResult] = simpleCommand(
      engine,
      OiwfsProbeTracking(config),
      systems.tcsCommon.oiwfsProbeTracking(config)
    )

    override def oiwfsPark: F[CommandResult] = simpleCommand(
      engine,
      OiwfsPark,
      systems.tcsCommon.oiwfsPark
    )

    override def oiwfsFollow(enable: Boolean): F[CommandResult] = simpleCommand(
      engine,
      OiwfsFollow(enable),
      systems.tcsCommon.oiwfsFollow(enable)
    )

    override def rotTrackingConfig(config: RotatorTrackConfig): F[CommandResult] =
      simpleCommand(
        engine,
        RotatorTrackingConfig(config),
        systems.tcsCommon.rotTrackingConfig(config)
      )

    override def enableGuide(config: TelescopeGuideConfig): F[CommandResult] =
      command(
        engine,
        EnableGuide(config),
        cats.data.State
          .modify[State](_.focus(_.guideConfig.tcsGuide).replace(config))
          .as(
            systems.tcsCommon.enableGuide(config)
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
          systems.tcsCommon.disableGuide
        )
    ) <* postTelescopeGuideConfig(GuideConfig.defaultGuideConfig)

    override def pwfs1Observe(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Observe(period),
      systems.tcsCommon.pwfs1Observe(period)
    )

    override def pwfs1StopObserve: F[CommandResult] = simpleCommand(
      engine,
      Pwfs1StopObserve,
      systems.tcsCommon.pwfs1StopObserve
    )

    override def pwfs2Observe(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Observe(period),
      systems.tcsCommon.pwfs2Observe(period)
    )

    override def pwfs2StopObserve: F[CommandResult] = simpleCommand(
      engine,
      Pwfs2StopObserve,
      systems.tcsCommon.pwfs2StopObserve
    )

    override def oiwfsObserve(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      OiwfsObserve(period),
      systems.tcsCommon.oiwfsObserve(period)
    )

    override def oiwfsStopObserve: F[CommandResult] = simpleCommand(
      engine,
      OiwfsStopObserve,
      systems.tcsCommon.oiwfsStopObserve
    )

    override def acObserve(period: TimeSpan): F[CommandResult] = simpleCommand(
      engine,
      AcObserve(period),
      systems.tcsCommon.hrwfsObserve(period)
    )

    override def acStopObserve: F[CommandResult] = simpleCommand(
      engine,
      AcStopObserve,
      systems.tcsCommon.hrwfsStopObserve
    )

    override def getGuideState: F[GuideState] = systems.tcsCommon.getGuideState

    override def getGuidersQuality: F[GuidersQualityValues] = systems.tcsCommon.getGuideQuality

    override def getTelescopeState: F[TelescopeState] = systems.tcsCommon.getTelescopeState

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
      systems.tcsCommon.m1Park
    )

    override def m1Unpark: F[CommandResult] = simpleCommand(
      engine,
      M1Unpark,
      systems.tcsCommon.m1Unpark
    )

    override def m1OpenLoopOff: F[CommandResult] = simpleCommand(
      engine,
      M1OpenLoopOff,
      systems.tcsCommon.m1UpdateOff
    )

    override def m1OpenLoopOn: F[CommandResult] = simpleCommand(
      engine,
      M1OpenLoopOn,
      systems.tcsCommon.m1UpdateOn
    )

    override def m1ZeroFigure: F[CommandResult] = simpleCommand(
      engine,
      M1ZeroFigure,
      systems.tcsCommon.m1ZeroFigure
    )

    override def m1LoadAoFigure: F[CommandResult] = simpleCommand(
      engine,
      M1LoadAoFigure,
      systems.tcsCommon.m1LoadAoFigure
    )

    override def m1LoadNonAoFigure: F[CommandResult] = simpleCommand(
      engine,
      M1LoadNonAoFigure,
      systems.tcsCommon.m1LoadNonAoFigure
    )

    override def lightpathConfig(from: LightSource, to: LightSinkName): F[CommandResult] =
      simpleCommand(
        engine,
        LightPathConfig(from, to),
        systems.tcsCommon.lightPath(from, to)
      )

    override def getInstrumentPort(instrument: Instrument): F[Option[Int]] =
      systems.tcsCommon.getInstrumentPorts.map { x =>
        val a = instrument match {
          case Instrument.AcqCam     => 1
          case Instrument.Flamingos2 => x.flamingos2Port
          case Instrument.Ghost      => x.ghostPort
          case Instrument.GmosNorth  => x.gmosPort
          case Instrument.GmosSouth  => x.gmosPort
          case Instrument.Gnirs      => x.gnirsPort
          case Instrument.Gpi        => x.gpiPort
          case Instrument.Gsaoi      => x.gsaoiPort
          case Instrument.Igrins2    => x.igrins2Port
          case Instrument.Niri       => x.niriPort
          case Instrument.Alopeke    => (site === Site.GN).fold(2, 0)
          case Instrument.Zorro      => (site === Site.GS).fold(2, 0)
          case _                     => 0
        }
        (a =!= 0).option(a)
      }

    override def acquisitionAdj(
      offset: Offset,
      iaa:    Option[Angle],
      ipa:    Option[Angle]
    ): F[CommandResult] =
      simpleCommand(
        engine,
        AcquisitionAdjust(offset, ipa, iaa),
        stateRef.get.flatMap(s => systems.tcsCommon.acquisitionAdj(offset, ipa, iaa)(s.guideConfig))
      )

    override def wfsSky(wfs: GuideProbe, period: TimeSpan): F[CommandResult] = wfs match {
      case enums.GuideProbe.PWFS1                                        =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => systems.tcsCommon.pwfs1Sky(period)(s.guideConfig))
        )
      case enums.GuideProbe.PWFS2                                        =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => systems.tcsCommon.pwfs2Sky(period)(s.guideConfig))
        )
      case enums.GuideProbe.GmosOIWFS | enums.GuideProbe.Flamingos2OIWFS =>
        simpleCommand(
          engine,
          WfsSky(wfs, period),
          stateRef.get.flatMap(s => systems.tcsCommon.oiwfsSky(period)(s.guideConfig))
        )
    }

    override def getGuideDemand: F[GuideConfig] = stateRef.get.map(_.guideConfig)

    override def getTargetAdjustments: F[TargetOffsets] = systems.tcsCommon.getTargetAdjustments

    override def getPointingOffset: F[PointingCorrections] =
      systems.tcsCommon.getPointingCorrections

    override def getOriginOffset: F[FocalPlaneOffset] = systems.tcsCommon.getOriginOffset

    override def targetAdjust(
      target:            VirtualTelescope,
      handsetAdjustment: HandsetAdjustment,
      openLoops:         Boolean
    ): F[CommandResult] =
      simpleCommand(
        engine,
        TargetAdjust(target, handsetAdjustment, openLoops),
        stateRef.get.flatMap(s =>
          systems.tcsCommon.targetAdjust(target, handsetAdjustment, openLoops)(s.guideConfig)
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
          systems.tcsCommon.originAdjust(handsetAdjustment, openLoops)(s.guideConfig)
        )
      )

    override def pointingAdjust(handsetAdjustment: HandsetAdjustment): F[CommandResult] =
      simpleCommand(
        engine,
        PointingAdjust(handsetAdjustment),
        systems.tcsCommon.pointingAdjust(handsetAdjustment)
      )

    override def targetOffsetAbsorb(target: VirtualTelescope): F[CommandResult] =
      simpleCommand(
        engine,
        TargetOffsetAbsorb(target),
        systems.tcsCommon.targetOffsetAbsorb(target)
      )

    override def targetOffsetClear(target: VirtualTelescope, openLoops: Boolean): F[CommandResult] =
      simpleCommand(
        engine,
        TargetOffsetClear(target, openLoops),
        stateRef.get.flatMap(s =>
          systems.tcsCommon.targetOffsetClear(target, openLoops)(s.guideConfig)
        )
      )

    override def originOffsetAbsorb: F[CommandResult] =
      simpleCommand(
        engine,
        OriginOffsetAbsorb,
        systems.tcsCommon.originOffsetAbsorb
      )

    override def originOffsetClear(openLoops: Boolean): F[CommandResult] =
      simpleCommand(
        engine,
        OriginOffsetClear(openLoops),
        stateRef.get.flatMap(s => systems.tcsCommon.originOffsetClear(openLoops)(s.guideConfig))
      )

    override def pointingOffsetClearLocal: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetClearLocal,
        systems.tcsCommon.pointingOffsetClearLocal
      )

    override def pointingOffsetAbsorbGuide: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetAbsorbGuide,
        systems.tcsCommon.pointingOffsetAbsorbGuide
      )

    override def pointingOffsetClearGuide: F[CommandResult] =
      simpleCommand(
        engine,
        PointingOffsetClearGuide,
        systems.tcsCommon.pointingOffsetClearGuide
      )

    override def acLens(l: AcLens): F[CommandResult] = simpleCommand(
      engine,
      AcSetLens(l),
      systems.tcsCommon.acCommands.lens(l)
    )

    override def acNdFilter(nd: AcNdFilter): F[CommandResult] = simpleCommand(
      engine,
      AcSetNdFilter(nd),
      systems.tcsCommon.acCommands.ndFilter(nd)
    )

    override def acFilter(flt: AcFilter): F[CommandResult] = simpleCommand(
      engine,
      AcSetFilter(flt),
      systems.tcsCommon.acCommands.filter(flt)
    )

    override def acWindowSize(wnd: AcWindow): F[CommandResult] = simpleCommand(
      engine,
      AcSetWindowSize(wnd),
      systems.tcsCommon.acCommands.windowSize(wnd)
    )

    override def getAcMechsState: F[AcMechsState] = systems.tcsCommon.acCommands.getState

    override def pwfs1Filter(filter: PwfsFilter): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1Filter(filter),
      systems.tcsCommon.pwfs1Mechs.filter(filter)
    )

    override def pwfs1FieldStop(fieldStop: PwfsFieldStop): F[CommandResult] = simpleCommand(
      engine,
      Pwfs1FieldStop(fieldStop),
      systems.tcsCommon.pwfs1Mechs.fieldStop(fieldStop)
    )

    override def pwfs2Filter(filter: PwfsFilter): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2Filter(filter),
      systems.tcsCommon.pwfs2Mechs.filter(filter)
    )

    override def pwfs2FieldStop(fieldStop: PwfsFieldStop): F[CommandResult] = simpleCommand(
      engine,
      Pwfs2FieldStop(fieldStop),
      systems.tcsCommon.pwfs2Mechs.fieldStop(fieldStop)
    )

    override def getPwfs1MechsState: F[PwfsMechsState] = systems.tcsCommon.getPwfs1Mechs

    override def getPwfs2MechsState: F[PwfsMechsState] = systems.tcsCommon.getPwfs2Mechs

    override def getBafflesState: F[BafflesState] = systems.tcsCommon.getBaffles
  }

  def build[F[_]: {Temporal, Logger, Async}](
    site:    Site,
    systems: Systems[F],
    conf:    NavigateEngineConfiguration
  ): F[NavigateEngine[F]] = for {
    eng <- StateEngine.build[F, State, NavigateEvent]
    ref <- Ref.of[F, State](startState)
    top <- Topic[F, NavigateState]
  } yield NavigateEngineImpl[F](site, systems, conf, eng, ref, top)

  case class WfsConfigState(
    period:          Option[TimeSpan],
    configuredForQl: Option[Boolean]
  )

  case class State(
    commandInProgress: Option[NavigateCommand],
    guideConfig:       GuideConfig,
    onSwappedTarget:   Boolean
  ) {
    lazy val tcsActionInProgress: Boolean = commandInProgress.isDefined
  }

  val startState: State = State(
    commandInProgress = None,
    guideConfig = GuideConfig.defaultGuideConfig,
    onSwappedTarget = false
  )

  /**
   * This is used for simple commands, just an F that executes the command when evaluated, producing
   * a result. It relies in command().
   *
   * @param engine
   *   The state machine.
   * @param cmdType
   *   : The command type, used for logs.
   * @param cmd
   *   : The actual command, wrapped in effect F.
   * @param f
   *   : Lens to the command guard flag in the global state.
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
        // State processing. Get the current state to verify that there is no other command in progress
        engine.getState.flatMap { st =>
          if (!st.tcsActionInProgress) {
            // All clear, let's build the message that will execute the command
            engine
              // First, mark that there is a command in progress
              .modifyState(_.focus(_.commandInProgress).replace(cmdType.some))
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
                        // The result is then passed to the Async callback.
                        x.attempt
                          .map(cmdResultToNavigateEvent)
                          .flatTap(y => Async[F].delay(f(y.asRight)))
                          .map(z =>
                            // Here we build the State that will be evaluated at the end
                            Event(
                              engine
                                // Unmark the command in progress flag
                                .modifyState(_.focus(_.commandInProgress).replace(none))
                                // And create an output event with the command result
                                .as(CommandEnd(cmdType, z).some)
                            )
                          )
                      )
                      .some
                  )
                )
              )
          } else {
            // Another command was in progress. We schedule the call to the Async callback and produce the output event.
            val r = CommandResult.CommandFailure(
              s"Cannot execute command because another TCS command is in progress."
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
      case Right(ApplyCommandResult.Paused)    => CommandResult.CommandPaused
      case Right(ApplyCommandResult.Completed) => CommandResult.CommandSuccess
      case Left(e: TimeoutException)           =>
        CommandResult.CommandFailure(s"Command timed out after ${e.getLocalizedMessage}")
      case Left(e)                             =>
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
