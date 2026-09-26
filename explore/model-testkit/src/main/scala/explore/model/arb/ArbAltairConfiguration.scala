// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.itc.AltairParameters
import lucuma.odb.data.AltairConfiguration
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAltairConfiguration:
  given Arbitrary[AltairConfiguration] =
    Arbitrary:
      for
        mode              <- arbitrary[AltairMode]
        explicitFieldLens <- arbitrary[Option[FieldLens]]
        cassRotator       <- arbitrary[CassRotator]
        ndFilter          <- arbitrary[AltairNdFilter]
      yield AltairConfiguration(mode, explicitFieldLens, cassRotator, ndFilter)

  given Cogen[AltairConfiguration] =
    Cogen[(AltairMode, Option[FieldLens], CassRotator, AltairNdFilter)].contramap(altair =>
      (altair.mode, altair.explicitFieldLens, altair.cassRotator, altair.ndFilter)
    )

  given Arbitrary[AltairParameters] =
    Arbitrary:
      Gen.oneOf(
        for
          separation <- arbitrary[Angle]
          brightness <- arbitrary[BrightnessValue]
          fieldLens  <- arbitrary[FieldLens]
        yield AltairParameters.Ngs(separation, brightness, fieldLens),
        for
          separation <- arbitrary[Angle]
          brightness <- arbitrary[BrightnessValue]
        yield AltairParameters.Lgs(separation, brightness),
        Gen.const(AltairParameters.LgsP1)
      )

object ArbAltairConfiguration extends ArbAltairConfiguration
