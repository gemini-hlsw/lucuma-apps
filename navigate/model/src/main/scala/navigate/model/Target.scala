// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import lucuma.core.math.*
import lucuma.core.math.Coordinates.given
import lucuma.core.math.Epoch.given
import lucuma.core.math.RadialVelocity.given
import lucuma.core.math.Wavelength.given
import monocle.Lens

sealed trait Target extends Product with Serializable derives Show {
  val objectName: String
  val wavelength: Option[Wavelength]
}

object Target {

  given Show[ProperMotion] = Show.fromToString

  given Show[Parallax] = Show.fromToString

  case class SiderealTarget(
    override val objectName: String,
    override val wavelength: Option[Wavelength],
    coordinates:             Coordinates,
    epoch:                   Epoch,
    properMotion:            Option[ProperMotion],
    radialVelocity:          Option[RadialVelocity],
    parallax:                Option[Parallax]
  ) extends Target derives Show

  case class Azimuth(toAngle: Angle)

  given Show[Azimuth] = Show.show(a => f"Azimuth(${a.toAngle.toDoubleDegrees}%.2f)")

  case class Elevation(toAngle: Angle)

  given Show[Elevation] = Show.show(a => f"Elevation(${a.toAngle.toDoubleDegrees}%.2f)")

  case class AzElCoordinates(azimuth: Azimuth, elevation: Elevation) derives Show

  case class AzElTarget(
    override val objectName: String,
    override val wavelength: Option[Wavelength],
    coordinates:             AzElCoordinates
  ) extends Target derives Show

  case class EphemerisTarget(
    override val objectName: String,
    override val wavelength: Option[Wavelength],
    ephemerisFile:           String
  ) extends Target derives Show

  val objectName: Lens[Target, String] = Lens.apply[Target, String](_.objectName) { s =>
    {
      case a: SiderealTarget  => a.copy(objectName = s)
      case a: AzElTarget      => a.copy(objectName = s)
      case a: EphemerisTarget => a.copy(objectName = s)
    }
  }

  val wavelength: Lens[Target, Option[Wavelength]] =
    Lens.apply[Target, Option[Wavelength]](_.wavelength) { s =>
      {
        case a: SiderealTarget  => a.copy(wavelength = s)
        case a: AzElTarget      => a.copy(wavelength = s)
        case a: EphemerisTarget => a.copy(wavelength = s)
      }
    }

}
