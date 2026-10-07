// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.Ephemeris
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.NonsiderealInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.WavelengthInput
import lucuma.odb.graphql.input.oneOrFail
import navigate.model.Target

object AzElTargetInput:

  val Binding: Matcher[Target.AzElCoordinates] =
    anglePairBinding("azimuth", "elevation"): (az, el) =>
      Target.AzElCoordinates(Target.Azimuth(az), Target.Elevation(el))

/**
 * Reads the ephemeris key from an ODB `NonsiderealInput`. Unlike the ODB, Navigate accepts any key
 * and ignores user-supplied ephemerides.
 */
object EphemerisKeyInput:

  val Binding: Matcher[Ephemeris.Key] =
    ObjectFieldsBinding.rmap:
      case List(
            NonsiderealInput.EphemerisKeyTypeBinding.Option("keyType", rKeyType),
            NonEmptyStringBinding.Option("des", rDes),
            NonsiderealInput.EphemerisKeyBinding.Option("key", rKey),
            _
          ) =>
        (rKeyType, rDes, rKey).parTupled
          .flatMap(NonsiderealInput.resolveKey)
          .flatMap(_.toResult("Must specify either (type and designation) or key."))

object TargetPropertiesInput:

  // Navigate does not use the target id.
  val Binding: Matcher[Target] =
    ObjectFieldsBinding.rmap:
      case List(
            _,
            NonEmptyStringBinding("name", rName),
            SiderealInput.CreateBinding.Option("sidereal", rSidereal),
            EphemerisKeyInput.Binding.Option("nonsidereal", rNonsidereal),
            AzElTargetInput.Binding.Option("azel", rAzel),
            WavelengthInput.Binding.Option("wavelength", rWavelength)
          ) =>
        (rName, rSidereal, rNonsidereal, rAzel, rWavelength).parTupled.flatMap:
          (name, sidereal, nonsidereal, azel, wavelength) =>
            target(name.value, wavelength, sidereal, nonsidereal, azel)

  private[input] def target(
    name:        String,
    wavelength:  Option[Wavelength],
    sidereal:    Option[SiderealInput.Create],
    nonsidereal: Option[Ephemeris.Key],
    azel:        Option[Target.AzElCoordinates]
  ): Result[Target] =
    val siderealT       =
      sidereal.map(s =>
        Target.SiderealTarget(
          name,
          wavelength,
          Coordinates(s.ra, s.dec),
          s.epoch,
          s.properMotion,
          s.radialVelocity,
          s.parallax
        )
      )
    val nonSiderealT    =
      nonsidereal.map(Target.EphemerisTarget(name, wavelength, _))
    val azelT           =
      azel.map(Target.AzElTarget(name, wavelength, _))

    oneOrFail[Target](
      siderealT    -> "sidereal",
      nonSiderealT -> "nonsidereal",
      azelT        -> "azel"
    )

object GuideTargetPropertiesInput:

  val Binding: Matcher[Target] =
    ObjectFieldsBinding.rmap:
      case List(
            NonEmptyStringBinding("name", rName),
            SiderealInput.CreateBinding.Option("sidereal", rSidereal),
            EphemerisKeyInput.Binding.Option("nonsidereal", rNonsidereal)
          ) =>
        (rName, rSidereal, rNonsidereal).parTupled.flatMap: (name, sidereal, nonsidereal) =>
          TargetPropertiesInput.target(name.value, none, sidereal, nonsidereal, none)
