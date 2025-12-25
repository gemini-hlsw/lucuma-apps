// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.GmosImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.refined.*

enum GmosImagingVariant(val variantType: GmosImagingVariantType) derives Eq:
  case Grouped(
    order:      WavelengthOrder,
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends GmosImagingVariant(GmosImagingVariantType.Grouped)
  case Interleaved(
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends GmosImagingVariant(GmosImagingVariantType.Interleaved)
  case PreImaging(offset1: Offset, offset2: Offset, offset3: Offset, offset4: Offset)
      extends GmosImagingVariant(GmosImagingVariantType.PreImaging)

  def toVariantType(newVariantType: GmosImagingVariantType): GmosImagingVariant =
    this match
      case Grouped(order, offsets, skyCount, skyOffsets)  =>
        newVariantType match
          case GmosImagingVariantType.Grouped     => this
          case GmosImagingVariantType.Interleaved => Interleaved(offsets, skyCount, skyOffsets)
          case GmosImagingVariantType.PreImaging  =>
            PreImaging(Offset.Zero, Offset.Zero, Offset.Zero, Offset.Zero)
      case Interleaved(offsets, skyCount, skyOffsets)     =>
        newVariantType match
          case GmosImagingVariantType.Grouped     =>
            Grouped(WavelengthOrder.Increasing, offsets, skyCount, skyOffsets)
          case GmosImagingVariantType.Interleaved => this
          case GmosImagingVariantType.PreImaging  =>
            PreImaging(Offset.Zero, Offset.Zero, Offset.Zero, Offset.Zero)
      case PreImaging(offset1, offset2, offset3, offset4) =>
        newVariantType match
          case GmosImagingVariantType.Grouped     =>
            Grouped(WavelengthOrder.Increasing, None, 0.refined, None)
          case GmosImagingVariantType.Interleaved => Interleaved(None, 0.refined, None)
          case GmosImagingVariantType.PreImaging  => this
