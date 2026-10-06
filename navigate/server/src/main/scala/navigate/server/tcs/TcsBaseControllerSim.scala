// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Eq
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import lucuma.core.enums
import lucuma.core.enums.Instrument
import lucuma.core.enums.MountGuideOption
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.GuideConfig
import lucuma.core.model.IntPercent
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.navigate.LightSource
import monocle.Focus
import monocle.Focus.focus
import monocle.Lens
import mouse.boolean.*
import navigate.model.AcMechsState
import navigate.model.AcWindow
import navigate.model.AllWfsConfiguration
import navigate.model.BafflesState
import navigate.model.Distance
import navigate.model.FocalPlaneOffset
import navigate.model.GuideState
import navigate.model.GuidersQualityValues
import navigate.model.GuidersQualityValues.GuiderQuality
import navigate.model.HandsetAdjustment
import navigate.model.InstrumentSpecifics
import navigate.model.LightPath
import navigate.model.MechSystemState
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
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.DomeMode
import navigate.model.enums.FollowStatus.*
import navigate.model.enums.LightSink
import navigate.model.enums.ParkStatus.*
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.QlMode
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope
import navigate.server.ApplyCommandResult
import navigate.server.tcs.TcsBaseController.AcCommands
import navigate.server.tcs.TcsBaseController.PwfsMechanismCommands
import navigate.server.tcs.TcsBaseControllerEpics.WfsGuideStates

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

abstract class TcsBaseControllerSim[F[_]: Async](stateRef: SignallingRef[F, TcsSimState])
    extends TcsBaseController[F] {

  val acValidNdFilters: List[AcNdFilter] = Enumerated[AcNdFilter].all

  private def lensRef[A](l: Lens[TcsSimState, A]): SignallingRef[F, A] =
    SignallingRef.lens(stateRef)(l.get, s => a => l.replace(a)(s))

  private val guideRef        = lensRef(TcsSimState.guide)
  private val telStateRef     = lensRef(TcsSimState.telescope)
  private val acMechRef       = lensRef(TcsSimState.acMechs)
  private val p1MechRef       = lensRef(TcsSimState.pwfs1Mechs)
  private val p2MechRef       = lensRef(TcsSimState.pwfs2Mechs)
  private val bafflesRef      = lensRef(TcsSimState.baffles)
  private val pwfs1ConfigsRef = lensRef(TcsSimState.wfsConfigs.andThen(AllWfsConfiguration.pwfs1))
  private val pwfs2ConfigsRef = lensRef(TcsSimState.wfsConfigs.andThen(AllWfsConfiguration.pwfs2))
  private val oiwfsConfigsRef = lensRef(TcsSimState.wfsConfigs.andThen(AllWfsConfiguration.oiwfs))

  private val random: Random[F] = Random.javaUtilConcurrentThreadLocalRandom[F]

  private val parkedState: MechSystemState = MechSystemState(Parked, NotFollowing)

  private val guideOff: GuideState => GuideState =
    _.copy(mountOffload = MountGuideOption.MountGuideOff,
           m1Guide = M1GuideConfig.M1GuideOff,
           m2Guide = M2GuideConfig.M2GuideOff,
           probeGuide = none
    )

  private val wfsStopped: GuideState => GuideState =
    _.copy(p1Integrating = false, p2Integrating = false, oiIntegrating = false)

  /**
   * Mirrors the autopark slew options: a probe with no guider configured in the TcsConfig is parked
   * when its autopark flag is set.
   */
  private def autoparkProbes(slewOptions: SlewOptions, config: TcsConfig)(
    t: TelescopeState
  ): TelescopeState = t.copy(
    pwfs1 = if (slewOptions.autoparkPwfs1.value && config.pwfs1.isEmpty) parkedState else t.pwfs1,
    pwfs2 = if (slewOptions.autoparkPwfs2.value && config.pwfs2.isEmpty) parkedState else t.pwfs2,
    oiwfs = if (slewOptions.autoparkOiwfs.value && config.oiwfs.isEmpty) parkedState else t.oiwfs
  )

  private def withBaffles(config: TcsConfig): TcsSimState => TcsSimState =
    s => config.bafflesState.fold(s)(TcsSimState.baffles.replace(_)(s))

  private def observe(
    integrating:  Lens[GuideState, Boolean],
    wfsConfig:    Lens[AllWfsConfiguration, WfsConfiguration],
    exposureTime: TimeSpan
  ): F[ApplyCommandResult] =
    stateRef
      .update(
        TcsSimState.guide
          .andThen(integrating)
          .replace(true)
          .andThen(
            TcsSimState.wfsConfigs
              .andThen(wfsConfig)
              .andThen(WfsConfiguration.exposureTime)
              .replace(exposureTime)
          )
      )
      .as(ApplyCommandResult.Completed)

  override def mcsPark: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.mount).replace(parkedState))
    .as(ApplyCommandResult.Completed)

  override def mcsFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.mount).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def rotStop(useBrakes: Boolean): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.crcs.following).replace(NotFollowing))
    .as(ApplyCommandResult.Completed)

  override def rotPark: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.crcs).replace(parkedState))
    .as(ApplyCommandResult.Completed)

  override def rotFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.crcs).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def rotMove(angle: RotatorAngle): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.crcs.parked).replace(NotParked))
    .as(ApplyCommandResult.Completed)

  override def tcsConfig(config: TcsConfig)(guide: GuideConfig): F[ApplyCommandResult] =
    stateRef.update(withBaffles(config)).as(ApplyCommandResult.Completed)

  override def slew(
    slewOptions: SlewOptions,
    tcsConfig:   TcsConfig
  ): F[ApplyCommandResult] =
    val stopGuide: TcsSimState => TcsSimState =
      if (slewOptions.stopGuide.value) TcsSimState.guide.modify(wfsStopped.andThen(guideOff))
      else identity

    stateRef
      .update(
        stopGuide
          .andThen(TcsSimState.telescope.modify(autoparkProbes(slewOptions, tcsConfig)))
          .andThen(withBaffles(tcsConfig))
      )
      .as(ApplyCommandResult.Completed)

  override def instrumentSpecifics(config: InstrumentSpecifics): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsTarget(target: Target): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def rotIaa(angle: Angle): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsPark: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.oiwfs).replace(parkedState))
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
      _.copy(mountOffload = config.mountGuide,
             m1Guide = config.m1Guide,
             m2Guide = config.m2Guide,
             probeGuide = config.probeGuide
      )
    )
    .as(ApplyCommandResult.Completed)

  override def disableGuide: F[ApplyCommandResult] =
    guideRef.update(guideOff).as(ApplyCommandResult.Completed)

  override def oiwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] =
    observe(Focus[GuideState](_.oiIntegrating), AllWfsConfiguration.oiwfs, exposureTime)

  override def oiwfsStopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(oiIntegrating = false))
    .as(ApplyCommandResult.Completed)

  override def getGuideState: F[GuideState] = guideRef.get

  override def getGuideQuality: F[GuidersQualityValues] =
    def quality(integrating: Boolean): F[GuiderQuality] =
      random.betweenInt(900, 1100).map(GuiderQuality(_, integrating))

    guideRef.get.flatMap: g =>
      (quality(g.p1Integrating), quality(g.p2Integrating), quality(g.oiIntegrating))
        .mapN(GuidersQualityValues.apply)

  override def baffles(
    central:    CentralBafflePosition,
    deployable: DeployableBafflePosition
  ): F[ApplyCommandResult] =
    bafflesRef.set(BafflesState(central, deployable)).as(ApplyCommandResult.Completed)

  override def getTelescopeState: F[TelescopeState] = telStateRef.get

  override def scsFollow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.scs).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def swapTarget(swapConfig: SwapConfig): F[ApplyCommandResult] = disableGuide

  override def getInstrumentPort(instrument: Instrument): F[Option[Int]] = (instrument match {
    case enums.Instrument.AcqCamNorth  => 1
    case enums.Instrument.AcqCamSouth  => 1
    case enums.Instrument.Alopeke      => 2
    case enums.Instrument.Flamingos2   => 1
    case enums.Instrument.Ghost        => 0
    case enums.Instrument.GmosNorth    => 5
    case enums.Instrument.GmosSouth    => 3
    case enums.Instrument.Gnirs        => 0
    case enums.Instrument.Gpi          => 0
    case enums.Instrument.Gsaoi        => 0
    case enums.Instrument.Igrins2      => 0
    case enums.Instrument.MaroonX      => 0
    case enums.Instrument.Niri         => 0
    case enums.Instrument.Scorpio      => 0
    case enums.Instrument.VisitorNorth => 0
    case enums.Instrument.VisitorSouth => 0
    case enums.Instrument.Zorro        => 2
  }).some.filter(_ =!= 0).pure[F]

  override def lightPath(from: LightSource, to: LightSink): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def restoreTarget(config: TcsConfig): F[ApplyCommandResult] =
    stateRef
      .update(TcsSimState.guide.modify(guideOff).andThen(withBaffles(config)))
      .as(ApplyCommandResult.Completed)

  override def hrwfsObserve(exposureTime: TimeSpan): F[ApplyCommandResult] = guideRef
    .update(_.copy(acIntegrating = true))
    .as(ApplyCommandResult.Completed)

  override def hrwfsStopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(acIntegrating = false))
    .as(ApplyCommandResult.Completed)

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

  override def offset(offset: Offset, guiding: Boolean)(
    guide:       GuideConfig,
    wfsTracking: WfsGuideStates
  ): F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def centralWavelength(wavelength: Wavelength): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def configureStep(
    offset:      Option[Offset],
    wavelength:  Option[Wavelength],
    lightPath:   Option[LightPath],
    defocus:     Option[Distance],
    guiding:     Boolean
  )(
    guide:       GuideConfig,
    wfsTracking: WfsGuideStates
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
    .update(_.focus(_.pwfs1).replace(parkedState))
    .as(ApplyCommandResult.Completed)

  override def pwfs1Follow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs1).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs2ProbeTracking(config: TrackingConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2Park: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.pwfs2).replace(parkedState))
    .as(ApplyCommandResult.Completed)

  override def pwfs2Follow(enable: Boolean): F[ApplyCommandResult] = telStateRef
    .update(
      _.focus(_.pwfs2).replace(MechSystemState(NotParked, enable.fold(Following, NotFollowing)))
    )
    .as(ApplyCommandResult.Completed)

  override def pwfs1Observe(exposureTime: TimeSpan): F[ApplyCommandResult] =
    observe(Focus[GuideState](_.p1Integrating), AllWfsConfiguration.pwfs1, exposureTime)

  override def pwfs1StopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(p1Integrating = false))
    .as(ApplyCommandResult.Completed)

  override def pwfs1Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2Observe(exposureTime: TimeSpan): F[ApplyCommandResult] =
    observe(Focus[GuideState](_.p2Integrating), AllWfsConfiguration.pwfs2, exposureTime)

  override def pwfs2StopObserve: F[ApplyCommandResult] = guideRef
    .update(_.copy(p2Integrating = false))
    .as(ApplyCommandResult.Completed)

  override def pwfs2Sky(exposureTime: TimeSpan)(guide: GuideConfig): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override val pwfs1Mechs: PwfsMechanismCommands[F] = new PwfsMechanismCommandsImpl(p1MechRef)

  override val pwfs2Mechs: PwfsMechanismCommands[F] = new PwfsMechanismCommandsImpl(p2MechRef)

  private class PwfsMechanismCommandsImpl(ref: Ref[F, PwfsMechsState])
      extends PwfsMechanismCommands[F] {
    override def filter(f: PwfsFilter): F[ApplyCommandResult] =
      simulateMechanism(ref, Focus[PwfsMechsState](_.filter), Enumerated[PwfsFilter].all)(f)

    override def fieldStop(fs: PwfsFieldStop): F[ApplyCommandResult] =
      simulateMechanism(ref, Focus[PwfsMechsState](_.fieldStop), Enumerated[PwfsFieldStop].all)(fs)
  }

  override def getPwfs1Mechs: F[PwfsMechsState] = p1MechRef.get

  override def getPwfs2Mechs: F[PwfsMechsState] = p2MechRef.get

  override def getBaffles: F[BafflesState] = bafflesRef.get

  override val acCommands: AcCommands[F] = new AcCommands[F] {
    override def lens(l: AcLens): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.lens), Enumerated[AcLens].all)(l)

    override def ndFilter(ndFilter: AcNdFilter): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.ndFilter), acValidNdFilters)(ndFilter)

    override def filter(filter: AcFilter): F[ApplyCommandResult] =
      simulateMechanism(acMechRef, Focus[AcMechsState](_.filter), Enumerated[AcFilter].all)(filter)

    override def windowSize(size: AcWindow): F[ApplyCommandResult] =
      ApplyCommandResult.Completed.pure[F]

    override def getState: F[AcMechsState] = acMechRef.get
  }

  private val mechanismStepPeriod: FiniteDuration = 1.seconds

  protected def simulateMechanism[S, A: Eq](ref: Ref[F, S], l: Lens[S, Option[A]], seq: List[A])(
    pos: A
  ): F[ApplyCommandResult] =
    val target = seq.indexWhere(_ === pos)
    if target < 0 then
      Async[F].raiseError(
        new IllegalArgumentException(s"Mechanism position $pos is not one of $seq")
      )
    else
      ref.get
        .flatMap: x =>
          l.get(x)
            .traverse_(i =>
              mechanismPath(seq, seq.indexWhere(_ === i), target)
                .flatMap(a => List(none, a.some))
                .traverse_(v => Temporal[F].delayBy(ref.update(l.replace(v)), mechanismStepPeriod))
            )
        .as(ApplyCommandResult.Completed)

  private def mechanismPath[A](seq: List[A], from: Int, to: Int): List[A] =
    if from < 0 then List(seq(to))
    else
      val n        = seq.length
      val forward  = (to - from + n) % n
      val backward = (from - to + n) % n
      if forward <= backward then (1 to forward).toList.map(k => seq((from + k) % n))
      else (1 to backward).toList.map(k => seq((from - k + n) % n))

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

  override def getOiwfsConfig: F[WfsConfiguration] = oiwfsConfigsRef.get

  override def pwfs1ConfigStream: Resource[F, Stream[F, WfsConfiguration]] =
    Resource.pure(pwfs1ConfigsRef.discrete.changes.zipLeft(Stream.fixedDelay(mechanismStepPeriod)))

  override def pwfs2ConfigStream: Resource[F, Stream[F, WfsConfiguration]] =
    Resource.pure(pwfs2ConfigsRef.discrete.changes.zipLeft(Stream.fixedDelay(mechanismStepPeriod)))

  override def oiwfsConfigStream: Resource[F, Stream[F, WfsConfiguration]] =
    Resource.pure(oiwfsConfigsRef.discrete.changes.zipLeft(Stream.fixedDelay(mechanismStepPeriod)))

  override def pwfs1QlMode(mode: QlMode): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def pwfs2QlMode(mode: QlMode): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def oiwfsQlMode(mode: QlMode): F[ApplyCommandResult] =
    ApplyCommandResult.Completed.pure[F]

  override def agScienceFoldPark: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def agPickoffMirrorPark: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def agAoFoldPark: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def agAllPark: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def ecsEnableDome(mode: DomeMode): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.dome).replace(mode.some))
    .as(ApplyCommandResult.Completed)

  override def ecsDisableDome: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.dome).replace(none))
    .as(ApplyCommandResult.Completed)

  override def ecsEnableShutters(mode: ShutterMode): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.shutters).replace(mode.some))
    .as(ApplyCommandResult.Completed)

  override def ecsDisableShutters: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.shutters).replace(none))
    .as(ApplyCommandResult.Completed)

  override def ecsMoveEastVentGate(position: IntPercent): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.eastVentGateOpen).replace(position))
    .as(ApplyCommandResult.Completed)

  override def ecsCloseEastVentGate: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.eastVentGateOpen).replace(EcsEpicsSystem.ventGateClosePos))
    .as(ApplyCommandResult.Completed)

  override def ecsMoveWestVentGate(position: IntPercent): F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.westVentGateOpen).replace(position))
    .as(ApplyCommandResult.Completed)

  override def ecsCloseWestVentGate: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.westVentGateOpen).replace(EcsEpicsSystem.ventGateClosePos))
    .as(ApplyCommandResult.Completed)

  override def ecsDomePark: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.dome).replace(none))
    .as(ApplyCommandResult.Completed)

  override def ecsShuttersPark: F[ApplyCommandResult] = telStateRef
    .update(_.focus(_.enclosure.shutters).replace(none))
    .as(ApplyCommandResult.Completed)

  override def azimuthUnwrap: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def rotUnwrap: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def pwfs1Unwrap: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]

  override def pwfs2Unwrap: F[ApplyCommandResult] = ApplyCommandResult.Completed.pure[F]
}
