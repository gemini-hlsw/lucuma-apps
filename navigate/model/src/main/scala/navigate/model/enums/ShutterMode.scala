// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import navigate.model.Distance

enum ShutterMode(val tag: String) derives Eq {
  case FullyOpen                    extends ShutterMode("FullyOpen")
  case Tracking(aperture: Distance) extends ShutterMode(ShutterMode.TrackingTag)
}

object ShutterMode {
  val default: ShutterMode = FullyOpen
  val TrackingTag: String  = "Tracking"

  def fromTag(tag: String, height: Option[Distance]): Option[ShutterMode] =
    tag match {
      case FullyOpen.tag => FullyOpen.some
      case TrackingTag   => height.map(Tracking(_))
      case _             => none
    }
}
