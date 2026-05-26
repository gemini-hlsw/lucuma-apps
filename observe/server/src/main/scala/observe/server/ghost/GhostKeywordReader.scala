// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.Applicative
import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.all.*
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.util.TimeSpan
import observe.model.CurrentConditions
import observe.server.keywords.*

sealed trait GhostKeywordsReader[F[_]] {
  def basePos: F[Boolean]
  def srifu1: F[String]
  def srifu2: F[String]
  def hrifu1: F[String]
  def hrifu2: F[String]
  def fiberAgitator1Enabled: F[Boolean]
  def fiberAgitator2Enabled: F[Boolean]
  def redCount: F[Option[Int]]
  def redDuration: F[Option[Double]]
  def redCcds: F[Option[String]]
  def redReadMode: F[Option[String]]
  def blueCount: F[Option[Int]]
  def blueDuration: F[Option[Double]]
  def blueCcds: F[Option[String]]
  def blueReadMode: F[Option[String]]
  def exposureDuration: F[Option[Double]]
  def resolutionMode: F[String]
  def targetMode: F[Option[String]]
  val targetName: F[Option[String]]
  def slitCount: F[Option[Int]]
  def slitDuration: F[Option[Double]]
}

final class DefaultGhostKeywordsReader[F[_]: Applicative]
    extends GhostKeywordsReader[F]
    with GhostTargetName {

  val basePos: F[Boolean]                 = true.pure[F]
  val srifu1: F[String]                   = "".pure[F]
  val srifu2: F[String]                   = "".pure[F]
  val hrifu1: F[String]                   = "".pure[F]
  val hrifu2: F[String]                   = "".pure[F]
  val fiberAgitator1Enabled: F[Boolean]   = false.pure[F]
  val fiberAgitator2Enabled: F[Boolean]   = false.pure[F]
  val redCount: F[Option[Int]]            = intDefault[F].map(_.some)
  val redDuration: F[Option[Double]]      = doubleDefault[F].map(_.some)
  val redCcds: F[Option[String]]          = strDefault[F].map(_.some)
  val redReadMode: F[Option[String]]      = strDefault[F].map(_.some)
  val blueCount: F[Option[Int]]           = intDefault[F].map(_.some)
  val blueDuration: F[Option[Double]]     = doubleDefault[F].map(_.some)
  val blueCcds: F[Option[String]]         = strDefault[F].map(_.some)
  val blueReadMode: F[Option[String]]     = strDefault[F].map(_.some)
  val resolutionMode: F[String]           = strDefault[F]
  val targetMode: F[Option[String]]       = strDefault[F].map(_.some)
  val targetName: F[Option[String]]       = strDefault[F].map(_.some)
  val slitCount: F[Option[Int]]           = intDefault[F].map(_.some)
  val slitDuration: F[Option[Double]]     = doubleDefault[F].map(_.some)
  val exposureDuration: F[Option[Double]] = doubleDefault[F].map(_.some)
}

object GhostKeywordsReader extends GhostLUT with GhostTargetName {
  val readMode2String: GhostReadMode => String = {
    case GhostReadMode.Slow   => "Slow"
    case GhostReadMode.Medium => "Medium"
    case GhostReadMode.Fast   => "Rapid"
  }

  val resolutionMode2String: GhostResolutionMode => String = {
    case GhostResolutionMode.Standard => "Standard"
    case GhostResolutionMode.High     => "High"
  }

  def exposureTime(
    red:  GhostDetector.Red,
    blue: GhostDetector.Blue
  ): TimeSpan = (red.value.exposureTime *| red.value.exposureCount.value)
    .max(blue.value.exposureTime *| blue.value.exposureCount.value)

  def extractTargetName(t: IFUTargetType): Option[String] = t match {
    case IFUTargetType.SkyPosition(sky)          => sky.name.toString.some
    case IFUTargetType.SiderealTarget(target)    => target.name.toString.some
    case IFUTargetType.NonsiderealTarget(target) => target.name.toString.some
    case _                                       => none
  }

  def apply[F[_]: Sync](
    config:     GhostConfig,
    conditions: Ref[F, CurrentConditions]
  ): GhostKeywordsReader[F] =
    new GhostKeywordsReader[F] {
      val basePos: F[Boolean]                 = config.baseCoords.isEmpty.pure[F]
      val srifu1: F[String]                   = config.ifu1TargetType.name
        .filter(_ => config.resolutionMode === GhostResolutionMode.Standard)
        .getOrElse("    ")
        .pure[F]
      val srifu2: F[String]                   = config.ifu2TargetType.name
        .filter(_ => config.resolutionMode === GhostResolutionMode.Standard)
        .getOrElse("    ")
        .pure[F]
      val hrifu1: F[String]                   = config.ifu1TargetType.name
        .filter(_ => config.resolutionMode === GhostResolutionMode.High)
        .getOrElse("    ")
        .pure[F]
      val hrifu2: F[String]                   = config.ifu2TargetType.name
        .filter(_ => config.resolutionMode === GhostResolutionMode.High)
        .getOrElse("    ")
        .pure[F]
      val fiberAgitator1Enabled: F[Boolean]   =
        (config.fiberAgitator1 === GhostIfu1FiberAgitator.Enabled).pure[F]
      val fiberAgitator2Enabled: F[Boolean]   =
        (config.fiberAgitator2 === GhostIfu2FiberAgitator.Enabled).pure[F]
      val redCount: F[Option[Int]]            =
        calcRedCount(config.obsType, config.coAdds, config.redConfig).value.some.pure[F]
      val redDuration: F[Option[Double]]      =
        config.redConfig.value.exposureTime.toSeconds.toDouble.some.pure[F]
      val redCcds: F[Option[String]]          = config.redConfig.value.binning.displayValue.some.pure[F]
      val redReadMode: F[Option[String]]      =
        readMode2String(config.redConfig.value.readMode).some.pure[F]
      val blueCount: F[Option[Int]]           =
        calcBlueCount(config.obsType, config.coAdds, config.blueConfig).value.some.pure[F]
      val blueDuration: F[Option[Double]]     =
        config.blueConfig.value.exposureTime.toSeconds.toDouble.some.pure[F]
      val blueCcds: F[Option[String]]         = config.blueConfig.value.binning.displayValue.some.pure[F]
      val blueReadMode: F[Option[String]]     =
        readMode2String(config.blueConfig.value.readMode).some.pure[F]
      val resolutionMode: F[String]           = resolutionMode2String(config.resolutionMode).pure[F]
      val targetMode: F[Option[String]]       = targetModeFromConfig(config).pure[F]
      val targetName: F[Option[String]]       = targetNameFromConfig(config).pure[F]
      val slitCount: F[Option[Int]]           =
        if (isScience(config.obsType)) {
          conditions.get.map { c =>
            config.svCameraOverride
              .map(t => svOverrideCameraRepeats(t, config.blueConfig, config.redConfig))
              .getOrElse(
                svCameraRepeats(c, config.scienceMagnitude, config.blueConfig, config.redConfig)
              )
              .some
          }
        } else
          svCalibSVRepeats(config.obsType, config.blueConfig, config.redConfig, config.coAdds).some
            .pure[F]
      val slitDuration: F[Option[Double]]     =
        if (isScience(config.obsType))
          conditions.get.map { c =>
            config.svCameraOverride
              .getOrElse(svCameraTime(c, config.scienceMagnitude))
              .toSeconds
              .toDouble
              .some
          }
        else
          svCalibExposureTime(config.obsType).toSeconds.toDouble.some.pure[F]
      val exposureDuration: F[Option[Double]] =
        exposureTime(config.redConfig, config.blueConfig).toSeconds.toDouble.some.pure[F]
    }

}
