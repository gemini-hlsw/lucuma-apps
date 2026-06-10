// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.syntax.all.*

trait GhostTargetName {
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

  def targetNameFromConfig(ghostConfig: GhostConfig): Option[String] = ghostConfig match {
    case _: GhostCalibration                     => none
    case _: StandardResolutionMode.SingleTarget | _: StandardResolutionMode.NonSiderealTarget |
        _: StandardResolutionMode.TargetPlusSky | _: HighResolutionMode =>
      ghostConfig.ifu1TargetType.getNameOption
    case _: StandardResolutionMode.DualTarget    =>
      (ghostConfig.ifu1TargetType.getNameOption, ghostConfig.ifu2TargetType.getNameOption).mapN {
        case (n1, n2) =>
          s"${ellipsis(n1, 35 - 3)}, ${ellipsis(n2, 35 - 3)}"
      }
    case _: StandardResolutionMode.SkyPlusTarget => ghostConfig.ifu2TargetType.getNameOption
  }
}
