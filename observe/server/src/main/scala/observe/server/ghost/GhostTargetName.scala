// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.syntax.all.*

trait GhostTargetName {
//  def targetModeFromNames(
//                           srifu1: Option[String],
//                           srifu2: Option[String],
//                           hrifu1: Option[String],
//                           hrifu2: Option[String]
//                         ): Option[String] = (srifu1, srifu2, hrifu1, hrifu2) match {
//    case (Some(_), Some("Sky"), None, None) => "SRIFU1 Target, SRIFU2 Sky".some
//    case (Some("Sky"), Some(_), None, None) => "SRIFU1 Sky, SRIFU2 Target".some
//    case (Some(_), Some(_), None, None) => "Dual Target".some
//    case (Some(_), None, None, None) => "Single Target".some
//    case (None, None, Some(_), Some("Sky")) => "HRIFU Target, Sky".some
//    case (None, None, Some(_), Some("Sky (PRV)")) => "HRIFU Target, Sky (PRV)".some
//    case _ => None
//  }

  def targetModeFromConfig(ghostConfig: GhostConfig): Option[String] = ghostConfig match {
    case _: GhostCalibration                                                                  => none
    case _: StandardResolutionMode.SingleTarget | _: StandardResolutionMode.NonSiderealTarget =>
      "Single Target".some
    case _: StandardResolutionMode.DualTarget                                                 => "Dual Target".some
    case _: StandardResolutionMode.TargetPlusSky                                              => "SRIFU1 Target, SRIFU2 Sky".some
    case _: StandardResolutionMode.SkyPlusTarget                                              => "SRIFU1 Sky, SRIFU2 Target".some
    case _: HighResolutionMode.TargetPlusSky                                                  => "HRIFU Target, Sky".some
    case _: HighResolutionMode.NonSidereal                                                    => none
  }

  private def ellipsis(text: String, max: Int): String =
    if (text.length <= max) {
      text
    } else {
      text.substring(0, max - 3) + "..."
    }

//  def targetNameFromNames(
//                           srifu1: Option[String],
//                           srifu2: Option[String],
//                           hrifu1: Option[String],
//                           hrifu2: Option[String]
//                         ): Option[String] = (srifu1, srifu2, hrifu1, hrifu2) match {
//    case (Some(t), Some("Sky"), None, None) => t.some
//    case (Some("Sky"), Some(t), None, None) => t.some
//    // max value length is 70 so we have to subtract 3 for the ellipsis
//    case (Some(t1), Some(t2), None, None) =>
//      s"${ellipsis(t1, 35 - 3)}, ${ellipsis(t2, 35 - 3)}".some
//    case (Some(t), None, None, None) => t.some
//    case (None, None, Some(t), Some("Sky")) => t.some
//    case (None, None, Some(t), Some("Sky (PRV)")) => t.some
//    case _ => None
//  }

  def targetNameFromConfig(ghostConfig: GhostConfig): Option[String] = ghostConfig match {
    case _: GhostCalibration                     => none
    case _: StandardResolutionMode.SingleTarget | _: StandardResolutionMode.NonSiderealTarget |
        _: StandardResolutionMode.TargetPlusSky | _: HighResolutionMode =>
      ghostConfig.ifu1TargetType.name
    case _: StandardResolutionMode.DualTarget    =>
      (ghostConfig.ifu1TargetType.name, ghostConfig.ifu2TargetType.name).mapN { case (n1, n2) =>
        s"${ellipsis(n1, 35 - 3)}, ${ellipsis(n2, 35 - 3)}"
      }
    case _: StandardResolutionMode.SkyPlusTarget => ghostConfig.ifu2TargetType.name
  }
}
