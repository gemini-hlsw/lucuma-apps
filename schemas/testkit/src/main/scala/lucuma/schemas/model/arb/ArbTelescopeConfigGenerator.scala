// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import lucuma.core.geom.arb.ArbOffsetGenerator.given
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.schemas.model.TelescopeConfigGenerator
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbTelescopeConfigGenerator:

  val genEnumerated: Gen[TelescopeConfigGenerator.Enumerated] =
    for {
      values <- arbitrary[NonEmptyList[lucuma.core.model.sequence.TelescopeConfig]]
    } yield TelescopeConfigGenerator.Enumerated(values)

  val genFromOffsetGenerator: Gen[TelescopeConfigGenerator.FromOffsetGenerator] =
    for {
      offsetGenerator <- arbitrary[lucuma.core.geom.OffsetGenerator]
    } yield TelescopeConfigGenerator.FromOffsetGenerator(offsetGenerator)

  given Arbitrary[TelescopeConfigGenerator] =
    Arbitrary[TelescopeConfigGenerator]:
      Gen.oneOf(genEnumerated, genFromOffsetGenerator)

  given Cogen[TelescopeConfigGenerator] =
    Cogen[
      Either[
        NonEmptyList[lucuma.core.model.sequence.TelescopeConfig],
        lucuma.core.geom.OffsetGenerator
      ]
    ].contramap:
      case TelescopeConfigGenerator.Enumerated(values)             => Left(values)
      case TelescopeConfigGenerator.FromOffsetGenerator(offsetGen) => Right(offsetGen)

object ArbTelescopeConfigGenerator extends ArbTelescopeConfigGenerator
