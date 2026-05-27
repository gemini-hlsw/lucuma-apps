// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

private[config] enum GnirsFocusMode(val tag: String, val shortName: String, val longName: String)
    derives Enumerated,
      Display:
  case Best   extends GnirsFocusMode("best", "Best", "Best Focus")
  case Custom extends GnirsFocusMode("custom", "Custom", "Custom Focus")

  def toMotorSteps: Option[GnirsFocusMotorStepsValue] = this match
    case Best   => None
    case Custom => Some(GnirsFocusMotorStepsValue.unsafeFrom(0)) // .withUnit[GnirsFocusMotorStep])

private[config] object GnirsFocusMode:
  def fromMotorSteps(steps: Option[GnirsFocusMotorStepsValue]): GnirsFocusMode =
    steps match
      case Some(_) => Custom
      case None    => Best
