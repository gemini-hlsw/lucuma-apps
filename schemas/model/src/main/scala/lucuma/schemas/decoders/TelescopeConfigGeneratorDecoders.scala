// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import lucuma.core.geom.OffsetGenerator
import lucuma.schemas.model.TelescopeConfigGenerator
import io.circe.Decoder
import lucuma.odb.json.stepconfig.given
import lucuma.core.enums.TelescopeConfigGeneratorType
import cats.syntax.option.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import cats.data.NonEmptyList
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.offset.decoder.given
import scala.annotation.targetName

trait TelescopeConfigGeneratorDecoders:
  @targetName("TelescopeConfigGeneratorDecoder")
  given Decoder[Option[TelescopeConfigGenerator]] = Decoder.instance: c =>
    c.downField("generatorType")
      .as[TelescopeConfigGeneratorType]
      .flatMap:
        case TelescopeConfigGeneratorType.NoGenerator => Right(none)
        case TelescopeConfigGeneratorType.Enumerated  =>
          for configs <- c.downField("enumerated")
                           .downField("values")
                           .as[NonEmptyList[lucuma.core.model.sequence.TelescopeConfig]]
          yield TelescopeConfigGenerator.Enumerated(configs).some
        case TelescopeConfigGeneratorType.Random      =>
          val random = c.downField("random")
          for
            size   <- random.downField("size").as[Angle]
            center <- random.downField("center").as[Offset]
          yield TelescopeConfigGenerator
            .FromOffsetGenerator(OffsetGenerator.Random(size, center))
            .some
        case TelescopeConfigGeneratorType.Spiral      =>
          val spiral = c.downField("spiral")
          for
            size   <- spiral.downField("size").as[Angle]
            center <- spiral.downField("center").as[Offset]
          yield TelescopeConfigGenerator
            .FromOffsetGenerator(OffsetGenerator.Spiral(size, center))
            .some
        case TelescopeConfigGeneratorType.Uniform     =>
          val uniform = c.downField("uniform")
          for
            cornerA <- uniform.downField("cornerA").as[Offset]
            cornerB <- uniform.downField("cornerB").as[Offset]
          yield TelescopeConfigGenerator
            .FromOffsetGenerator(OffsetGenerator.Uniform(cornerA, cornerB))
            .some
