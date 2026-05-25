// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.Eq
import cats.implicits.*
import giapi.client.GiapiConfig
import giapi.enums.GiapiStatusApply
import giapi.enums.GiapiStatusApply.*
import lucuma.core.enums
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.StepType
import lucuma.core.model.Target as GemTarget
import lucuma.core.model.Target.given
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.Enumerated

enum FiberAgitator(val tag: String) derives Enumerated {
  case On extends FiberAgitator("On")

  case Off extends FiberAgitator("Off")

  case None extends FiberAgitator("None")
}

object FiberAgitator {

  given GiapiConfig[FiberAgitator] =
    GiapiConfig.instance {
      case On   => "FA_DEMAND_ON"
      case Off  => "FA_DEMAND_OFF"
      case None => "FA_DEMAND_NONE"
    }

  def fromBoolean(b: Boolean): FiberAgitator =
    if (b) On else Off

  def fromIfu1FiberAgitator(v: GhostIfu1FiberAgitator) = v match {
    case enums.GhostIfu1FiberAgitator.Disabled => FiberAgitator.Off
    case enums.GhostIfu1FiberAgitator.Enabled  => FiberAgitator.On
  }

  def fromIfu2FiberAgitator(v: GhostIfu2FiberAgitator) = v match {
    case enums.GhostIfu2FiberAgitator.Disabled => FiberAgitator.Off
    case enums.GhostIfu2FiberAgitator.Enabled  => FiberAgitator.On
  }

}

enum DemandType(val tag: String, val demandType: String) derives Enumerated {
  case DemandRADec extends DemandType("DemandRADec", "IFU_DEMAND_RADEC")
  case DemandXY    extends DemandType("DemandXY", "IFU_DEMAND_XY")
  case DemandPark  extends DemandType("DemandPark", "IFU_DEMAND_PARK")
  case DemandNone  extends DemandType("DemandNone", "IFU_DEMAND_NONE")
}

object DemandType {
  given GiapiConfig[DemandType] = _.demandType
}

enum IFUNum(val tag: String, val ifuNum: Int) derives Enumerated {

  case IFU1 extends IFUNum(tag = "IFU1", ifuNum = 1)
  case IFU2 extends IFUNum(tag = "IFU2", ifuNum = 2)

  val ifuStr: String = s"ghost:cc:cu:ifu$ifuNum"

  def demandItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Type
    case IFUNum.IFU2 => GhostIFU2Type
  }

  def bundleItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Bundle
    case IFUNum.IFU2 => GhostIFU2Bundle
  }

  def targetItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Target
    case IFUNum.IFU2 => GhostIFU2Target
  }

}

object IFUNum {
  given GiapiConfig[IFUNum] = _.ifuStr
}

enum BundleConfig(val tag: String, val configName: String) derives Enumerated {
  case Standard extends BundleConfig(tag = "Standard", configName = "IFU_STDRES")
  case HighRes  extends BundleConfig(tag = "HighRes", configName = "IFU_HIRES")
  case Sky      extends BundleConfig(tag = "Sky", configName = "IFU_SKY")

  def determineType(t: IFUTargetType): BundleConfig =
    t match {
      case IFUTargetType.SkyPosition(_) => BundleConfig.Sky
      case _                            => this
    }
}

object BundleConfig {
  given GiapiConfig[BundleConfig] = _.configName
}

sealed abstract class IFUTargetType(val targetType: String) extends Product with Serializable {
  def name: Option[String] = this match {
    case IFUTargetType.NoTarget                  => none
    case IFUTargetType.TargetXY                  => none
    case IFUTargetType.SkyPosition(sky)          => sky.name.value.some
    case IFUTargetType.SiderealTarget(target)    => target.name.value.some
    case IFUTargetType.NonsiderealTarget(target) => target.name.value.some
  }
}

object IFUTargetType {

  case object NoTarget extends IFUTargetType(targetType = "IFU_TARGET_NONE")

  case object TargetXY extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

  final case class SkyPosition(sky: GemTarget) extends IFUTargetType(targetType = "IFU_TARGET_SKY")

  final case class SiderealTarget(target: GemTarget.Sidereal)
      extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

  final case class NonsiderealTarget(target: GemTarget.Nonsidereal)
      extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

  def determineType(target: Option[GemTarget], sky: Option[GemTarget]): IFUTargetType =
    target
      .flatMap {
        case t: GemTarget.Sidereal    => SiderealTarget(t).some
        case t: GemTarget.Nonsidereal => NonsiderealTarget(t).some
        case _                        => none
      }
      .orElse(sky.map(SkyPosition.apply))
      .getOrElse(NoTarget)

  given GiapiConfig[IFUTargetType] = _.targetType

  given Eq[IFUTargetType.SiderealTarget] = Eq.by(_.target)

  given Eq[IFUTargetType.NonsiderealTarget] = Eq.by(_.target)

  given Eq[IFUTargetType.SkyPosition] = Eq.by(_.sky)

  given Eq[IFUTargetType] = Eq.instance {
    case (NoTarget, NoTarget)                           => true
    case (TargetXY, TargetXY)                           => true
    case (s1: SkyPosition, s2: SkyPosition)             => s1 === s2
    case (t1: SiderealTarget, t2: SiderealTarget)       => t1 === t2
    case (t1: NonsiderealTarget, t2: NonsiderealTarget) => t1 === t2
    case _                                              => false
  }
}

given GiapiConfig[GhostReadMode] = {
  case GhostReadMode.Slow   => "CHIP_READOUT_SLOW"
  case GhostReadMode.Medium => "CHIP_READOUT_MEDIUM"
  case GhostReadMode.Fast   => "CHIP_READOUT_FAST"
}

extension (b: GhostBinning) {
  def displayValue: String = s"${b.spectralBinning} x ${b.spatialBinning}"
}

extension (sc: StepConfig) {
  def isScience: Boolean = sc.stepType === StepType.Science
}
