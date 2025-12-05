// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Applicative
import cats.Eq
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.MountGuideOption
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.GuideConfig
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Focus.focus
import monocle.Lens
import mouse.boolean.*
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.AllWfsConfiguration
import navigate.model.BafflesState
import navigate.model.FocalPlaneOffset
import navigate.model.HandsetAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.PointingCorrections
import navigate.model.PwfsMechsState
import navigate.model.RotatorAngle
import navigate.model.RotatorTrackConfig
import navigate.model.SlewOptions
import navigate.model.SwapConfig
import navigate.model.Target
import navigate.model.TcsConfig
import navigate.model.TrackingConfig
import navigate.model.WfsConfiguration
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.LightSource
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.ApplyCommandResult
import navigate.server.tcs.FollowStatus.*
import navigate.server.tcs.GuidersQualityValues.GuiderQuality
import navigate.server.tcs.ParkStatus.*
import navigate.server.tcs.TcsBaseController.AcCommands
import navigate.server.tcs.TcsBaseController.PwfsMechanismCommands

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

abstract class TcsBaseControllerSim[F[_]: Async](
  guideRef:      Ref[F, GuideState],
  telStateRef:   Ref[F, TelescopeState],
  acMechRef:     Ref[F, AcMechsState],
  p1MechRef:     Ref[F, PwfsMechsState],
  p2MechRef:     Ref[F, PwfsMechsState],
  wfsConfigsRef: SignallingRef[F, AllWfsConfiguration]
) extends TcsBaseController[F] {

  val acValidNdFilters: List[AcNdFilter] = Enumerated[AcNdFilter].all

  override def mcsPark: F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.mount).replace(MechSystemState(Parked, NotFollowing))
    )
    .as(ApplyCommandResult.Completed)

  override def mcsFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.mount).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def rotStop(useBrakes: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.crcs.following).replace(NotFollowing)
    )
    .as(ApplyCommandResult.Completed)

  override def rotPark: F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.crcs).replace(MechSystemState(Parked, NotFollowing))
    )
    .as(ApplyCommandResult.Completed)

  override def rotFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.crcs).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def rotMove(angle: RotatorAngle): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.crcs.parked).replace(NotParked)
    )
    .as(ApplyCommandResult.Completed)

  override def ecsCarouselMode(
    domeMode:      DomeMode,
    shutterMode:   ShutterMode,
    slitHeight:    Double,
    domeEnable:    Boolean,
    shutterEnable: Boolean
  ): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def ecsVentGatesMove(gateEast: Double, westGate: Double): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def tcsConfig(config: TcsConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def slew(
    slewOptions: SlewOptions,
    tcsConfig:   TcsConfig
  ): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def instrumentSpecifics(config: InstrumentSpecifics): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsTarget(target: Target): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def rotIaa(angle: Angle): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsPark: F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.oiwfs).replace(MechSystemState(Parked, NotFollowing))
    )
    .as(ApplyCommandResult.Completed)

  override def oiwfsFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.oiwfs).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def oiwfsSky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def rotTrackingConfig(cfg: RotatorTrackConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def enableGuide(config: TelescopeGuideConfig): F[ApplyCommandResult] = guideRef
    .update(
      _.copy(mountOffload = config.mountGuide, m1Guide = config.m1Guide, m2Guide = config.m2Guide)
    )
    .as(ApplyCommandResult.Completed)

  override def disableGuide: F[ApplyCommandResult] = guideRef
    .update(
      _.copy(mountOffload = MountGuideOption.MountGuideOff,
             m1Guide = M1GuideConfig.M1GuideOff,
             m2Guide = M2GuideConfig.M2GuideOff
      )
    )
    .as(ApplyCommandResult.Completed)

  override def oiwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] = guideRef
    .update(_.copy(oiIntegrating = true))
    .as(ApplyCommandResult.Completed)

  override def oiwfsStopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(oiIntegrating = false))
    .as(ApplyCommandResult.Completed)

  override def getGuideState: F[GuideState] = guideRef.get

  override def getGuideQuality: F[GuidersQualityValues] =
    for {
      p1Cnts <- Async[F].delay(1000 + scala.util.Random.between(-100, 100))
      p2Cnts <- Async[F].delay(1000 + scala.util.Random.between(-100, 100))
      oiCnts <- Async[F].delay(1000 + scala.util.Random.between(-100, 100))
    } yield GuidersQualityValues(
      pwfs1 = GuiderQuality(p1Cnts, false),
      pwfs2 = GuiderQuality(p2Cnts, false),
      oiwfs = GuiderQuality(oiCnts, false)
    )

  override def baffles(
    central:    CentralBafflePosition,
    deployable: DeployableBafflePosition
  ): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def getTelescopeState: F[TelescopeState] = telStateRef.get

  override def scsFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.scs).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def swapTarget(swapConfig: SwapConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def getInstrumentPorts: F[InstrumentPorts] =
    InstrumentPorts(
      flamingos2Port = 1,
      ghostPort = 0,
      gmosPort = 3,
      gnirsPort = 0,
      gpiPort = 0,
      gsaoiPort = 5,
      igrins2Port = 0,
      nifsPort = 0,
      niriPort = 0
    ).pure[F]

  override def lightPath(from: LightSource, to: LightSinkName): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def restoreTarget(config: TcsConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def hrwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def hrwfsStopObserve: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1Park: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1Unpark: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1UpdateOn: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1UpdateOff: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1ZeroFigure: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1LoadAoFigure: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def m1LoadNonAoFigure: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def acquisitionAdj(offset: Offset, ipa: Option[Angle], iaa: Option[Angle])(
    guide: GuideConfig
  ): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def getTargetAdjustments: F[TargetOffsets] = TargetOffsets.default.pure[F]

  override def getPointingCorrections: F[PointingCorrections] = PointingCorrections.default.pure[F]

  override def getOriginOffset: F[FocalPlaneOffset] = FocalPlaneOffset.Zero.pure[F]

  override def targetAdjust(
    target:            VirtualTelescope,
    handsetAdjustment: HandsetAdjustment,
    openLoops:         Boolean
  )(guide: GuideConfig): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def originAdjust(handsetAdjustment: HandsetAdjustment, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def pointingAdjust(handsetAdjustment: HandsetAdjustment): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def targetOffsetAbsorb(target: VirtualTelescope): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def targetOffsetClear(target: VirtualTelescope, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def originOffsetAbsorb: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def originOffsetClear(openLoops: Boolean)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pointingOffsetClearLocal: F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pointingOffsetAbsorbGuide: F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pointingOffsetClearGuide: F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs1Target(target: Target): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2Target(target: Target): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs1ProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs1Park: F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs1).replace(MechSystemState(Parked, NotFollowing))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs1Follow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs1).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs2ProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2Park: F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs2).replace(MechSystemState(Parked, NotFollowing))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs2Follow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs2).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs1Observe(exposureTime: TimeSpan): F[ApplyCommandResult] = guideRef
    .update(_.copy(p1Integrating = true))
    .as(ApplyCommandResult.Completed)

  override def pwfs1StopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(p1Integrating = false))
    .as(ApplyCommandResult.Completed)

  override def pwfs1Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2Observe(exposureTime: TimeSpan): F[ApplyCommandResult] = guideRef
    .update(_.copy(p2Integrating = true))
    .as(ApplyCommandResult.Completed)

  override def pwfs2StopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(p2Integrating = false))
    .as(ApplyCommandResult.Completed)

  override def pwfs2Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override val pwfs1Mechs: PwfsMechanismCommands[F] = new PwfsMechanismCommands[F] {
    override def filter(f: PwfsFilter): F[ApplyCommandResult] =
      simulateMechanism(p1MechRef, Focus[PwfsMechsState](_.filter), Enumerated[PwfsFilter].all)(f)

    override def fieldStop(fs: PwfsFieldStop): F[ApplyCommandResult] =
      simulateMechanism(p1MechRef,
                        Focus[PwfsMechsState](_.fieldStop),
                        Enumerated[PwfsFieldStop].all
      )(fs)
  }
  override val pwfs2Mechs: PwfsMechanismCommands[F] = new PwfsMechanismCommands[F] {
    override def filter(f: PwfsFilter): F[ApplyCommandResult] =
      simulateMechanism(p2MechRef, Focus[PwfsMechsState](_.filter), Enumerated[PwfsFilter].all)(f)

    override def fieldStop(fs: PwfsFieldStop): F[ApplyCommandResult] =
      simulateMechanism(p2MechRef,
                        Focus[PwfsMechsState](_.fieldStop),
                        Enumerated[PwfsFieldStop].all
      )(fs)
  }

  override def getPwfs1Mechs: F[PwfsMechsState] = for {
    flt <- p1MechRef.get.map(_.filter)
    fld <- p1MechRef.get.map(_.fieldStop)
  } yield PwfsMechsState(flt, fld)

  override def getPwfs2Mechs: F[PwfsMechsState] = for {
    flt <- p2MechRef.get.map(_.filter)
    fld <- p2MechRef.get.map(_.fieldStop)
  } yield PwfsMechsState(flt, fld)

  override def getBaffles: F[BafflesState] =
    BafflesState(CentralBafflePosition.Open, DeployableBafflePosition.Visible).pure[F]

  override val acCommands: AcCommands[F] = new AcCommands[F] {
    override def lens(l: AcLens): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.lens), Enumerated[AcLens].all)(l)

    override def ndFilter(ndFilter: AcNdFilter): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.ndFilter), acValidNdFilters)(ndFilter)

    override def filter(filter: AcFilter): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.filter), Enumerated[AcFilter].all)(filter)

    override def windowSize(size: AcWindow): F[ApplyCommandResult] =
      ApplyCommandResult.Completed.pure[F]

    override def getState: F[AcMechsState] =
      for {
        lns <- acMechRef.get.map(_.lens)
        flt <- acMechRef.get.map(_.filter)
        ndf <- acMechRef.get.map(_.ndFilter)
      } yield AcMechsState(lns, ndf, flt)
  }

  private val mechanismStepPeriod: FiniteDuration = 1.seconds
  protected def simulateMechanism[S, A: Eq](ref: Ref[F, S], l: Lens[S, Option[A]], seq: List[A])(
    pos: A
  ): F[ApplyCommandResult] =
    ref.get
      .flatMap(x =>
        l.get(x)
          .map { i =>
            val straight  = (seq ++ seq).dropWhile(_ =!= i).takeWhile(_ =!= pos).tail :+ pos
            val backwards = (seq ++ seq).reverse.dropWhile(_ =!= i).takeWhile(_ =!= pos).tail :+ pos
            val finalSeq  = if (straight.length <= backwards.length) straight else backwards

            finalSeq
              .flatMap(a => List(none, a.some))
              .map(v => Temporal[F].delayBy(ref.update(l.replace(v)), mechanismStepPeriod))
              .sequence
              .whenA(i =!= pos)
          }
          .getOrElse(Applicative[F].unit)
      )
      .as(ApplyCommandResult.Completed)

  private val pwfs1ConfigsRef = SignallingRef.lens(wfsConfigsRef)(
    AllWfsConfiguration.pwfs1.get,
    w => p1 => AllWfsConfiguration.pwfs1.replace(p1)(w)
  )
  private val pwfs2ConfigsRef = SignallingRef.lens(wfsConfigsRef)(
    AllWfsConfiguration.pwfs2.get,
    w => p2 => AllWfsConfiguration.pwfs2.replace(p2)(w)
  )
  private val oiwfsConfigsRef = SignallingRef.lens(wfsConfigsRef)(
    AllWfsConfiguration.oiwfs.get,
    w => oi => AllWfsConfiguration.oiwfs.replace(oi)(w)
  )

  override def pwfs1CircularBuffer(enable: Boolean): F[ApplyCommandResult] = pwfs1ConfigsRef
    .update(WfsConfiguration.saving.replace(enable))
    .as(ApplyCommandResult.Completed)

  override def pwfs2CircularBuffer(enable: Boolean): F[ApplyCommandResult] = pwfs2ConfigsRef
    .update(WfsConfiguration.saving.replace(enable))
    .as(ApplyCommandResult.Completed)

  override def oiwfsCircularBuffer(enable: Boolean): F[ApplyCommandResult] = oiwfsConfigsRef
    .update(WfsConfiguration.saving.replace(enable))
    .as(ApplyCommandResult.Completed)

  override def getPwfs1Config: F[WfsConfiguration] = pwfs1ConfigsRef.get

  override def getPwfs2Config: F[WfsConfiguration] = pwfs2ConfigsRef.get

  override def getOiwfsConfig: F[WfsConfiguration] = pwfs2ConfigsRef.get

  override def pwfs1ConfigStream: F[Stream[F, WfsConfiguration]] =
    pwfs1ConfigsRef.changes.discrete.zipLeft(Stream.fixedDelay(mechanismStepPeriod)).pure[F]

  override def pwfs2ConfigStream: F[Stream[F, WfsConfiguration]] =
    pwfs2ConfigsRef.changes.discrete.zipLeft(Stream.fixedDelay(mechanismStepPeriod)).pure[F]

  override def oiwfsConfigStream: F[Stream[F, WfsConfiguration]] =
    oiwfsConfigsRef.changes.discrete.zipLeft(Stream.fixedDelay(mechanismStepPeriod)).pure[F]

}
