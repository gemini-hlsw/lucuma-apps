// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.util.TimeSpan

object formatting:
  // def offsetAxis[A](using show: OffsetFormat[A]): String =
  //   s"${show.format}:"

  // def offsetNSNod[T](using show: OffsetFormat[T]): String =
  //   s"${show.format}"

  def offsetAngle(off: Angle): String =
    f" ${Angle.signedDecimalArcseconds.get(off).toDouble}%03.2f″"

  // def axisLabelWidth[A](implicit show: OffsetFormat[A]): Double =
  //   tableTextWidth(offsetAxis[A])

  // def nsNodLabelWidth[A](implicit show: OffsetFormat[A]): Double =
  //   tableTextWidth(offsetNSNod[A])

  def formatExposureTime(i: Instrument)(time: TimeSpan): String =
    val e = time.toSeconds
    i match
      case Instrument.GmosNorth | Instrument.GmosSouth => f"$e%.0f"
      case _                                           => f"$e%.2f"
