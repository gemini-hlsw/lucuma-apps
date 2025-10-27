// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.kernel.Eq
import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.LightSinkName
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.TimeSpan
import navigate.model.HandsetAdjustment.given
import navigate.model.RotatorAngle.*
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.DomeMode
import navigate.model.enums.LightSource
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.model.enums.ShutterMode
import navigate.model.enums.VirtualTelescope

sealed trait NavigateCommand extends Product with Serializable

object NavigateCommand {
  case class AcObserve(period: TimeSpan)                                     extends NavigateCommand
  case class AcSetLens(l: AcLens)                                            extends NavigateCommand
  case class AcSetFilter(flt: AcFilter)                                      extends NavigateCommand
  case class AcSetNdFilter(nd: AcNdFilter)                                   extends NavigateCommand
  case class AcSetWindowSize(wnd: AcWindow)                                  extends NavigateCommand
  case class AcquisitionAdjust(offset: Offset, ipa: Option[Angle], iaa: Option[Angle])
      extends NavigateCommand
  case class AowfsFollow(enable: Boolean)                                    extends NavigateCommand
  case class CrcsFollow(enable: Boolean)                                     extends NavigateCommand
  case class CrcsMove(angle: RotatorAngle)                                   extends NavigateCommand
  case class CrcsStop(brakes: Boolean)                                       extends NavigateCommand
  case class Cwfs1Follow(enable: Boolean)                                    extends NavigateCommand
  case class Cwfs2Follow(enable: Boolean)                                    extends NavigateCommand
  case class Cwfs3Follow(enable: Boolean)                                    extends NavigateCommand
  case class EcsCarouselMode(
    domeMode:      DomeMode,
    shutterMode:   ShutterMode,
    slitHeight:    Double,
    domeEnable:    Boolean,
    shutterEnable: Boolean
  ) extends NavigateCommand
  case class EcsVentGatesMove(gateEast: Double, gateWest: Double)            extends NavigateCommand
  case class EnableGuide(config: TelescopeGuideConfig)                       extends NavigateCommand
  case class InstSpecifics(instrumentSpecificsParams: InstrumentSpecifics)   extends NavigateCommand
  case class LightPathConfig(from: LightSource, to: LightSinkName)           extends NavigateCommand
  case class McsFollow(enable: Boolean)                                      extends NavigateCommand
  case class Odgw1Follow(enable: Boolean)                                    extends NavigateCommand
  case class Odgw2Follow(enable: Boolean)                                    extends NavigateCommand
  case class Odgw3Follow(enable: Boolean)                                    extends NavigateCommand
  case class Odgw4Follow(enable: Boolean)                                    extends NavigateCommand
  case class OiwfsFollow(enable: Boolean)                                    extends NavigateCommand
  case class OiwfsObserve(period: TimeSpan)                                  extends NavigateCommand
  case class OiwfsProbeTracking(config: TrackingConfig)                      extends NavigateCommand
  case class OiwfsTarget(target: Target)                                     extends NavigateCommand
  case class OriginAdjust(handsetAdjustment: HandsetAdjustment, openLoops: Boolean)
      extends NavigateCommand
  case class OriginOffsetClear(openLoops: Boolean)                           extends NavigateCommand
  case class PointingAdjust(handsetAdjustment: HandsetAdjustment)            extends NavigateCommand
  case class Pwfs1FieldStop(fieldStop: PwfsFieldStop)                        extends NavigateCommand
  case class Pwfs1Filter(filter: PwfsFilter)                                 extends NavigateCommand
  case class Pwfs1Follow(enable: Boolean)                                    extends NavigateCommand
  case class Pwfs1Observe(period: TimeSpan)                                  extends NavigateCommand
  case class Pwfs1ProbeTracking(config: TrackingConfig)                      extends NavigateCommand
  case class Pwfs1Target(target: Target)                                     extends NavigateCommand
  case class Pwfs2FieldStop(fieldStop: PwfsFieldStop)                        extends NavigateCommand
  case class Pwfs2Filter(filter: PwfsFilter)                                 extends NavigateCommand
  case class Pwfs2Follow(enable: Boolean)                                    extends NavigateCommand
  case class Pwfs2Observe(period: TimeSpan)                                  extends NavigateCommand
  case class Pwfs2ProbeTracking(config: TrackingConfig)                      extends NavigateCommand
  case class Pwfs2Target(target: Target)                                     extends NavigateCommand
  case class RestoreTarget(config: TcsConfig)                                extends NavigateCommand
  case class RotatorTrackingConfig(config: RotatorTrackConfig)               extends NavigateCommand
  case class ScsFollow(enable: Boolean)                                      extends NavigateCommand
  case class Slew(slewOptions: SlewOptions, tcsConfig: TcsConfig, oid: Option[Observation.Id])
      extends NavigateCommand
  case class SwapTarget(swapConfig: SwapConfig)                              extends NavigateCommand
  case class TargetAdjust(
    target:            VirtualTelescope,
    handsetAdjustment: HandsetAdjustment,
    openLoops:         Boolean
  ) extends NavigateCommand
  case class TargetOffsetAbsorb(target: VirtualTelescope)                    extends NavigateCommand
  case class TargetOffsetClear(target: VirtualTelescope, openLoops: Boolean) extends NavigateCommand
  case class TcsConfigure(config: TcsConfig)                                 extends NavigateCommand
  case class WfsSky(wfs: GuideProbe, period: TimeSpan)                       extends NavigateCommand
  case object AcStopObserve                                                  extends NavigateCommand
  case object AowfsPark                                                      extends NavigateCommand
  case object CrcsPark                                                       extends NavigateCommand
  case object Cwfs1Park                                                      extends NavigateCommand
  case object Cwfs2Park                                                      extends NavigateCommand
  case object Cwfs3Park                                                      extends NavigateCommand
  case object DisableGuide                                                   extends NavigateCommand
  case object M1LoadAoFigure                                                 extends NavigateCommand
  case object M1LoadNonAoFigure                                              extends NavigateCommand
  case object M1OpenLoopOff                                                  extends NavigateCommand
  case object M1OpenLoopOn                                                   extends NavigateCommand
  case object M1Park                                                         extends NavigateCommand
  case object M1Unpark                                                       extends NavigateCommand
  case object M1ZeroFigure                                                   extends NavigateCommand
  case object McsPark                                                        extends NavigateCommand
  case object Odgw1Park                                                      extends NavigateCommand
  case object Odgw2Park                                                      extends NavigateCommand
  case object Odgw3Park                                                      extends NavigateCommand
  case object Odgw4Park                                                      extends NavigateCommand
  case object OiwfsPark                                                      extends NavigateCommand
  case object OiwfsStopObserve                                               extends NavigateCommand
  case object OriginOffsetAbsorb                                             extends NavigateCommand
  case object PointingOffsetAbsorbGuide                                      extends NavigateCommand
  case object PointingOffsetClearGuide                                       extends NavigateCommand
  case object PointingOffsetClearLocal                                       extends NavigateCommand
  case object Pwfs1Park                                                      extends NavigateCommand
  case object Pwfs1StopObserve                                               extends NavigateCommand
  case object Pwfs2Park                                                      extends NavigateCommand
  case object Pwfs2StopObserve                                               extends NavigateCommand
  case object ScsPark                                                        extends NavigateCommand

  given Eq[NavigateCommand] = Eq.fromUniversalEquals

  extension (self: NavigateCommand) {
    def name: String = self.getClass.getSimpleName.takeWhile(_ =!= '$')
  }

  given Show[NavigateCommand] = Show.show { self =>
    self match {
      case AcObserve(period)                                                             => f"${self.name}(period = ${period.toSeconds.toDouble}%.3f)"
      case AcSetLens(l)                                                                  => s"${self.name}(lens = $l)"
      case AcSetFilter(flt)                                                              => s"${self.name}(filter = $flt)"
      case AcSetNdFilter(nd)                                                             => s"${self.name}(ndFilter = $nd)"
      case AcSetWindowSize(wnd)                                                          => s"${self.name}(window = ${wnd.tag})"
      case AcquisitionAdjust(offset, ipa, iaa)                                           =>
        s"${self.name}(offset = $offset, ipa = $ipa, iaa = $iaa)"
      case AowfsFollow(enable)                                                           => s"${self.name}(enable = $enable)"
      case CrcsFollow(enable)                                                            => s"${self.name}(enable = $enable)"
      case CrcsMove(angle)                                                               => f"${self.name}(angle = ${angle.toDoubleDegrees}%.2fº)"
      case CrcsStop(brakes)                                                              => s"${self.name}(brakes = $brakes)"
      case Cwfs1Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Cwfs2Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Cwfs3Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case EcsCarouselMode(domeMode, shutterMode, slitHeight, domeEnable, shutterEnable) =>
        s"${self.name}(domeMode = $domeMode, shutterMode = $shutterMode, slitHeight = $slitHeight, domeEnable = $domeEnable, shutterEnable = $shutterEnable)"
      case EcsVentGatesMove(gateEast, gateWest)                                          =>
        s"${self.name}(gateEast = $gateEast, gateWest = $gateWest)"
      case EnableGuide(config)                                                           => s"${self.name}(config = $config)"
      case InstSpecifics(instrumentSpecificsParams)                                      =>
        s"${self.name}(instrumentSpecificsParams = ${instrumentSpecificsParams.show})"
      case LightPathConfig(from, to)                                                     => s"${self.name}(from = $from, to = $to)"
      case McsFollow(enable)                                                             => s"${self.name}(enable = $enable)"
      case Odgw1Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Odgw2Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Odgw3Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Odgw4Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case OiwfsFollow(enable)                                                           => s"${self.name}(enable = $enable)"
      case OiwfsObserve(period)                                                          => f"${self.name}(period = ${period.toSeconds.toDouble}%.3f)"
      case OiwfsProbeTracking(config)                                                    => s"${self.name}(config = $config)"
      case OiwfsTarget(target)                                                           => s"${self.name}(target = $target.show)"
      case OriginAdjust(handsetAdjustment, openLoops)                                    =>
        s"${self.name}(handsetAdjustment = ${handsetAdjustment.show}, openLoops = $openLoops)"
      case OriginOffsetClear(openLoops)                                                  => s"${self.name}(openLoops = $openLoops)"
      case PointingAdjust(handsetAdjustment)                                             =>
        s"${self.name}(handsetAdjustment = ${handsetAdjustment.show})"
      case Pwfs1FieldStop(fieldStop)                                                     => s"${self.name}(fieldStop = $fieldStop)"
      case Pwfs1Filter(filter)                                                           => s"${self.name}(filter = $filter)"
      case Pwfs1Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Pwfs1Observe(period)                                                          => f"${self.name}(period = ${period.toSeconds.toDouble}%.3f)"
      case Pwfs1ProbeTracking(config)                                                    => s"${self.name}(config = $config)"
      case Pwfs1Target(target: Target)                                                   => s"${self.name}(target = $target.show)"
      case Pwfs2FieldStop(fieldStop)                                                     => s"${self.name}(fieldStop = $fieldStop)"
      case Pwfs2Filter(filter)                                                           => s"${self.name}(filter = $filter)"
      case Pwfs2Follow(enable)                                                           => s"${self.name}(enable = $enable)"
      case Pwfs2Observe(period)                                                          => f"${self.name}(period = ${period.toSeconds.toDouble}%.3f)"
      case Pwfs2ProbeTracking(config)                                                    => s"${self.name}(config = $config)"
      case Pwfs2Target(target)                                                           => s"${self.name}(target = $target.show)"
      case RestoreTarget(config)                                                         => s"${self.name}(config = $config)"
      case RotatorTrackingConfig(config)                                                 => s"${self.name}(config = $config)"
      case ScsFollow(enable)                                                             => s"${self.name}(enable = $enable)"
      case Slew(slewOptions, tcsConfig, oid)                                             =>
        s"${self.name}(slewOptions = $slewOptions, tcsConfig = ${tcsConfig.show}, oid = $oid)"
      case SwapTarget(swapConfig)                                                        => s"${self.name}(swapConfig = ${swapConfig.show})"
      case TargetAdjust(target, handsetAdjustment, openLoops)                            =>
        s"${self.name}(target = $target, handsetAdjustment = ${handsetAdjustment.show}, openLoops = $openLoops)"
      case TargetOffsetAbsorb(target)                                                    => s"${self.name}(target = $target)"
      case TargetOffsetClear(target, openLoops)                                          =>
        s"${self.name}(target = $target, openLoops = $openLoops)"
      case TcsConfigure(config)                                                          => s"${self.name}(config = ${config.show})"
      case WfsSky(wfs, period)                                                           =>
        f"${self.name}(wfs = $wfs, period = ${period.toSeconds.toDouble}%.3f)"
      case _                                                                             => self.name
    }
  }

}
