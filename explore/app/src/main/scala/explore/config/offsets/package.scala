// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.model.TelescopeConfigGenerator

enum TelescopeConfigGeneratorType(
  val tag:       String,
  val shortName: String,
  val init:      Option[TelescopeConfigGenerator]
) derives Enumerated:
  case NoOffsets extends TelescopeConfigGeneratorType("none", "No Offsets", none)
  case Explicit
      extends TelescopeConfigGeneratorType(
        "explicit",
        "Explicit",
        TelescopeConfigGenerator.Enumerated(NonEmptyList.one(TelescopeConfig.Default)).some
      )
  case Uniform
      extends TelescopeConfigGeneratorType(
        "uniform",
        "Uniform",
        TelescopeConfigGenerator
          .FromOffsetGenerator:
            OffsetGenerator.Uniform(Offset.Zero, Offset.Zero)
          .some
      )
  case Spiral
      extends TelescopeConfigGeneratorType(
        "spiral",
        "Spiral",
        TelescopeConfigGenerator
          .FromOffsetGenerator:
            OffsetGenerator.Spiral(Angle.Angle0, Offset.Zero)
          .some
      )
  case Random
      extends TelescopeConfigGeneratorType(
        "random",
        "Random",
        TelescopeConfigGenerator
          .FromOffsetGenerator:
            OffsetGenerator.Random(Angle.Angle0, Offset.Zero)
          .some
      )

object TelescopeConfigGeneratorType:
  def fromTelescopeConfigGenerator(
    og: Option[TelescopeConfigGenerator]
  ): TelescopeConfigGeneratorType =
    og match
      case None                                                                              =>
        TelescopeConfigGeneratorType.NoOffsets
      case Some(TelescopeConfigGenerator.Enumerated(_))                                      =>
        TelescopeConfigGeneratorType.Explicit
      case Some(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Uniform(_, _))) =>
        TelescopeConfigGeneratorType.Uniform
      case Some(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Spiral(_, _)))  =>
        TelescopeConfigGeneratorType.Spiral
      case Some(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Random(_, _)))  =>
        TelescopeConfigGeneratorType.Random

  given Display[TelescopeConfigGeneratorType] = Display.byShortName(_.shortName)
