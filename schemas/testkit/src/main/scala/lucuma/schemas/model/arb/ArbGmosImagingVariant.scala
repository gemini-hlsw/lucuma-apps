// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import eu.timepit.refined.scalacheck.all.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.schemas.model.GmosImagingVariant
import lucuma.schemas.model.TelescopeConfigGenerator
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import ArbTelescopeConfigGenerator.given

trait ArbGmosImagingVariant:
  val genGrouped: Gen[GmosImagingVariant.Grouped] =
    for
      order      <- arbitrary[WavelengthOrder]
      offsets    <- Gen.option(arbitrary[TelescopeConfigGenerator])
      skyCount   <- arbitrary[NonNegInt]
      skyOffsets <- Gen.option(arbitrary[TelescopeConfigGenerator])
    yield GmosImagingVariant.Grouped(order, offsets, skyCount, skyOffsets)

  val genInterleaved: Gen[GmosImagingVariant.Interleaved] =
    for
      offsets    <- Gen.option(arbitrary[TelescopeConfigGenerator])
      skyCount   <- arbitrary[NonNegInt]
      skyOffsets <- Gen.option(arbitrary[TelescopeConfigGenerator])
    yield GmosImagingVariant.Interleaved(offsets, skyCount, skyOffsets)

  val genPreImaging: Gen[GmosImagingVariant.PreImaging] =
    for
      offset1 <- arbitrary[Offset]
      offset2 <- arbitrary[Offset]
      offset3 <- arbitrary[Offset]
      offset4 <- arbitrary[Offset]
    yield GmosImagingVariant.PreImaging(offset1, offset2, offset3, offset4)

  given Arbitrary[GmosImagingVariant] =
    Arbitrary[GmosImagingVariant]:
      Gen.oneOf(genGrouped, genInterleaved, genPreImaging)

  given Cogen[GmosImagingVariant.Grouped] =
    Cogen[
      (WavelengthOrder,
       Option[TelescopeConfigGenerator],
       NonNegInt,
       Option[TelescopeConfigGenerator]
      )
    ].contramap(o => (o.order, o.offsets, o.skyCount, o.skyOffsets))

  given Cogen[GmosImagingVariant.Interleaved] =
    Cogen[
      (Option[TelescopeConfigGenerator], NonNegInt, Option[TelescopeConfigGenerator])
    ].contramap(o => (o.offsets, o.skyCount, o.skyOffsets))

  given Cogen[GmosImagingVariant.PreImaging] =
    Cogen[(Offset, Offset, Offset, Offset)].contramap(o =>
      (o.offset1, o.offset2, o.offset3, o.offset4)
    )

  given Cogen[GmosImagingVariant] =
    Cogen[
      Either[
        GmosImagingVariant.Grouped,
        Either[
          GmosImagingVariant.Interleaved,
          GmosImagingVariant.PreImaging
        ]
      ]
    ].contramap:
      case g: GmosImagingVariant.Grouped     => Left(g)
      case i: GmosImagingVariant.Interleaved => Right(Left(i))
      case p: GmosImagingVariant.PreImaging  => Right(Right(p))

object ArbGmosImagingVariant extends ArbGmosImagingVariant
