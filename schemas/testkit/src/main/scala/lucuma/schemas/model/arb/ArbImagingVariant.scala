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
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.TelescopeConfigGenerator
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import ArbTelescopeConfigGenerator.given

trait ArbImagingVariant:
  val genGrouped: Gen[ImagingVariant.Grouped] =
    for
      order      <- arbitrary[WavelengthOrder]
      offsets    <- Gen.option(arbitrary[TelescopeConfigGenerator])
      skyCount   <- arbitrary[NonNegInt]
      skyOffsets <- Gen.option(arbitrary[TelescopeConfigGenerator])
    yield ImagingVariant.Grouped(order, offsets, skyCount, skyOffsets)

  val genInterleaved: Gen[ImagingVariant.Interleaved] =
    for
      offsets    <- Gen.option(arbitrary[TelescopeConfigGenerator])
      skyCount   <- arbitrary[NonNegInt]
      skyOffsets <- Gen.option(arbitrary[TelescopeConfigGenerator])
    yield ImagingVariant.Interleaved(offsets, skyCount, skyOffsets)

  val genPreImaging: Gen[ImagingVariant.PreImaging] =
    for
      offset1 <- arbitrary[Offset]
      offset2 <- arbitrary[Offset]
      offset3 <- arbitrary[Offset]
      offset4 <- arbitrary[Offset]
    yield ImagingVariant.PreImaging(offset1, offset2, offset3, offset4)

  given Arbitrary[ImagingVariant] =
    Arbitrary[ImagingVariant]:
      Gen.oneOf(genGrouped, genInterleaved, genPreImaging)

  given Cogen[ImagingVariant.Grouped] =
    Cogen[
      (WavelengthOrder,
       Option[TelescopeConfigGenerator],
       NonNegInt,
       Option[TelescopeConfigGenerator]
      )
    ].contramap(o => (o.order, o.offsets, o.skyCount, o.skyOffsets))

  given Cogen[ImagingVariant.Interleaved] =
    Cogen[
      (Option[TelescopeConfigGenerator], NonNegInt, Option[TelescopeConfigGenerator])
    ].contramap(o => (o.offsets, o.skyCount, o.skyOffsets))

  given Cogen[ImagingVariant.PreImaging] =
    Cogen[(Offset, Offset, Offset, Offset)].contramap(o =>
      (o.offset1, o.offset2, o.offset3, o.offset4)
    )

  given Cogen[ImagingVariant] =
    Cogen[
      Either[
        ImagingVariant.Grouped,
        Either[
          ImagingVariant.Interleaved,
          ImagingVariant.PreImaging
        ]
      ]
    ].contramap:
      case g: ImagingVariant.Grouped     => Left(g)
      case i: ImagingVariant.Interleaved => Right(Left(i))
      case p: ImagingVariant.PreImaging  => Right(Right(p))

object ArbImagingVariant extends ArbImagingVariant
