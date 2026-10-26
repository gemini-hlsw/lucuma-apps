// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.enums

import cats.Order
import cats.derived.*
import lucuma.schemas.model.BasicConfiguration

enum ObsClass(val label: String) derives Order:
  case Daytime   extends ObsClass("Day")
  case Nighttime extends ObsClass("Night")

object ObsClass:
  def fromString(s: String): ObsClass = s match
    case "dayCal" => ObsClass.Daytime
    case _        => ObsClass.Nighttime

  def fromObservingMode(observingMode: Option[BasicConfiguration]): ObsClass =
    observingMode match
      case Some(BasicConfiguration.DaytimeCalibration(_)) => ObsClass.Daytime
      case _                                              => ObsClass.Nighttime
