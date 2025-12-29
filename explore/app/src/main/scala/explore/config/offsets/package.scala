// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum GridType(val tag: String, val shortName: String, val init: Option[OffsetGenerator])
    derives Enumerated:
  case NoGrid extends GridType("none", "None", none)
  case Uniform
      extends GridType("uniform", "Uniform", OffsetGenerator.Uniform(Offset.Zero, Offset.Zero).some)
  case Spiral
      extends GridType("spiral", "Spiral", OffsetGenerator.Spiral(Angle.Angle0, Offset.Zero).some)
  case Random
      extends GridType("random", "Random", OffsetGenerator.Random(Angle.Angle0, Offset.Zero).some)

object GridType:
  def fromOffsetGenerator(og: Option[OffsetGenerator]): GridType =
    og match
      case None                                => GridType.NoGrid
      case Some(OffsetGenerator.Uniform(_, _)) => GridType.Uniform
      case Some(OffsetGenerator.Spiral(_, _))  => GridType.Spiral
      case Some(OffsetGenerator.Random(_, _))  => GridType.Random

  given Display[GridType] = Display.byShortName(_.shortName)
