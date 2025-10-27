// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import lucuma.core.enums.LightSinkName
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.TimeSpan
import navigate.model.AcMechsState
import navigate.model.AcWindow
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
import navigate.server.tcs.TcsBaseController.AcCommands
import navigate.server.tcs.TcsBaseController.PwfsMechanismCommands

trait TcsBaseController[F[_]] {
  def mcsPark: F[ApplyCommandResult]
  def mcsFollow(enable:                 Boolean): F[ApplyCommandResult]
  def rotStop(useBrakes:                Boolean): F[ApplyCommandResult]
  def rotPark: F[ApplyCommandResult]
  def rotFollow(enable:                 Boolean): F[ApplyCommandResult]
  def rotMove(angle:                    RotatorAngle): F[ApplyCommandResult]
  def ecsCarouselMode(
    domeMode:      DomeMode,
    shutterMode:   ShutterMode,
    slitHeight:    Double,
    domeEnable:    Boolean,
    shutterEnable: Boolean
  ): F[ApplyCommandResult]
  def ecsVentGatesMove(gateEast:        Double, westGate:       Double): F[ApplyCommandResult]
  def tcsConfig(config:                 TcsConfig): F[ApplyCommandResult]
  def slew(slewOptions:                 SlewOptions, tcsConfig: TcsConfig): F[ApplyCommandResult]
  def swapTarget(swapConfig:            SwapConfig): F[ApplyCommandResult]
  def restoreTarget(config:             TcsConfig): F[ApplyCommandResult]
  def instrumentSpecifics(config:       InstrumentSpecifics): F[ApplyCommandResult]
  def pwfs1Target(target:               Target): F[ApplyCommandResult]
  def pwfs2Target(target:               Target): F[ApplyCommandResult]
  def oiwfsTarget(target:               Target): F[ApplyCommandResult]
  def rotIaa(angle:                     Angle): F[ApplyCommandResult]
  def rotTrackingConfig(cfg:            RotatorTrackConfig): F[ApplyCommandResult]
  def pwfs1ProbeTracking(config:        TrackingConfig): F[ApplyCommandResult]
  def pwfs1Park: F[ApplyCommandResult]
  def pwfs1Follow(enable:               Boolean): F[ApplyCommandResult]
  def pwfs2ProbeTracking(config:        TrackingConfig): F[ApplyCommandResult]
  def pwfs2Park: F[ApplyCommandResult]
  def pwfs2Follow(enable:               Boolean): F[ApplyCommandResult]
  def oiwfsProbeTracking(config:        TrackingConfig): F[ApplyCommandResult]
  def oiwfsPark: F[ApplyCommandResult]
  def oiwfsFollow(enable:               Boolean): F[ApplyCommandResult]
  def enableGuide(config:               TelescopeGuideConfig): F[ApplyCommandResult]
  def disableGuide: F[ApplyCommandResult]
  def pwfs1Observe(exposureTime:        TimeSpan): F[ApplyCommandResult]
  def pwfs1StopObserve: F[ApplyCommandResult]
  def pwfs1Sky(exposureTime:            TimeSpan)(guide:        GuideConfig): F[ApplyCommandResult]
  def pwfs2Observe(exposureTime:        TimeSpan): F[ApplyCommandResult]
  def pwfs2StopObserve: F[ApplyCommandResult]
  def pwfs2Sky(exposureTime:            TimeSpan)(guide:        GuideConfig): F[ApplyCommandResult]
  def oiwfsObserve(exposureTime:        TimeSpan): F[ApplyCommandResult]
  def oiwfsStopObserve: F[ApplyCommandResult]
  def oiwfsSky(exposureTime:            TimeSpan)(guide:        GuideConfig): F[ApplyCommandResult]
  def hrwfsObserve(exposureTime:        TimeSpan): F[ApplyCommandResult]
  def hrwfsStopObserve: F[ApplyCommandResult]
  def baffles(
    central:    CentralBafflePosition,
    deployable: DeployableBafflePosition
  ): F[ApplyCommandResult]
  def scsFollow(enable:                 Boolean): F[ApplyCommandResult]
  def lightPath(from:                   LightSource, to:        LightSinkName): F[ApplyCommandResult]
  def m1Park: F[ApplyCommandResult]
  def m1Unpark: F[ApplyCommandResult]
  def m1UpdateOn: F[ApplyCommandResult]
  def m1UpdateOff: F[ApplyCommandResult]
  def m1ZeroFigure: F[ApplyCommandResult]
  def m1LoadAoFigure: F[ApplyCommandResult]
  def m1LoadNonAoFigure: F[ApplyCommandResult]
  def acquisitionAdj(offset: Offset, ipa: Option[Angle], iaa: Option[Angle])(
    guide: GuideConfig
  ): F[ApplyCommandResult]
  def targetAdjust(
    target:            VirtualTelescope,
    handsetAdjustment: HandsetAdjustment,
    openLoops:         Boolean
  )(guide: GuideConfig): F[ApplyCommandResult]
  def targetOffsetAbsorb(target:        VirtualTelescope): F[ApplyCommandResult]
  def targetOffsetClear(target: VirtualTelescope, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult]
  def originAdjust(handsetAdjustment: HandsetAdjustment, openLoops: Boolean)(
    guide: GuideConfig
  ): F[ApplyCommandResult]
  def originOffsetAbsorb: F[ApplyCommandResult]
  def originOffsetClear(openLoops:      Boolean)(guide:         GuideConfig): F[ApplyCommandResult]
  def pointingAdjust(handsetAdjustment: HandsetAdjustment): F[ApplyCommandResult]
  def pointingOffsetClearLocal: F[ApplyCommandResult]
  def pointingOffsetAbsorbGuide: F[ApplyCommandResult]
  def pointingOffsetClearGuide: F[ApplyCommandResult]

  val acCommands: AcCommands[F]

  val pwfs1Mechs: PwfsMechanismCommands[F]
  val pwfs2Mechs: PwfsMechanismCommands[F]

  // Queries
  def getGuideState: F[GuideState]
  def getGuideQuality: F[GuidersQualityValues]
  def getTelescopeState: F[TelescopeState]
  def getInstrumentPorts: F[InstrumentPorts]
  def getTargetAdjustments: F[TargetOffsets]
  def getPointingCorrections: F[PointingCorrections]
  def getOriginOffset: F[FocalPlaneOffset]
  def getPwfs1Mechs: F[PwfsMechsState]
  def getPwfs2Mechs: F[PwfsMechsState]
  def getBaffles: F[BafflesState]
}

object TcsBaseController {

  val SystemDefault: String  = "FK5"
  val EquinoxDefault: String = "J2000"
  val FixedSystem: String    = "Fixed"

  trait AcCommands[F[_]] {
    def lens(l:            AcLens): F[ApplyCommandResult]
    def ndFilter(ndFilter: AcNdFilter): F[ApplyCommandResult]
    def filter(filter:     AcFilter): F[ApplyCommandResult]
    def windowSize(size:   AcWindow): F[ApplyCommandResult]

    def getState: F[AcMechsState]
  }

  trait PwfsMechanismCommands[F[_]] {
    def filter(f:     PwfsFilter): F[ApplyCommandResult]
    def fieldStop(fs: PwfsFieldStop): F[ApplyCommandResult]
  }

}
