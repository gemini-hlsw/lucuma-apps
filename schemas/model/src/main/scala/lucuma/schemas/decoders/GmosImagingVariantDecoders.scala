// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.GmosImagingVariantType
import lucuma.core.enums.TelescopeConfigGeneratorType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.offset.decoder.given
import lucuma.odb.json.stepconfig.given
import lucuma.schemas.model.GmosImagingVariant
import lucuma.schemas.model.TelescopeConfigGenerator

import scala.annotation.targetName

trait GmosImagingVariantDecoders:
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

  given Decoder[GmosImagingVariant] = Decoder.instance: c =>
    c.downField("variantType")
      .as[GmosImagingVariantType]
      .flatMap:
        case GmosImagingVariantType.Grouped     =>
          val grouped = c.downField("grouped")
          for
            order      <- grouped.downField("order").as[WavelengthOrder]
            offsets    <- grouped.downField("offsets").as[Option[TelescopeConfigGenerator]]
            skyCount   <- grouped.downField("skyCount").as[NonNegInt]
            skyOffsets <- grouped.downField("skyOffsets").as[Option[TelescopeConfigGenerator]]
          yield GmosImagingVariant.Grouped(order, offsets, skyCount, skyOffsets)
        case GmosImagingVariantType.Interleaved =>
          val interleaved = c.downField("interleaved")
          for
            offsets    <- interleaved.downField("offsets").as[Option[TelescopeConfigGenerator]]
            skyCount   <- interleaved.downField("skyCount").as[NonNegInt]
            skyOffsets <- interleaved.downField("skyOffsets").as[Option[TelescopeConfigGenerator]]
          yield GmosImagingVariant.Interleaved(offsets, skyCount, skyOffsets)
        case GmosImagingVariantType.PreImaging  =>
          val preimaging = c.downField("preimaging")
          for
            offset1 <- preimaging.downField("offset1").as[Offset]
            offset2 <- preimaging.downField("offset2").as[Offset]
            offset3 <- preimaging.downField("offset3").as[Offset]
            offset4 <- preimaging.downField("offset4").as[Offset]
          yield GmosImagingVariant.PreImaging(offset1, offset2, offset3, offset4)
