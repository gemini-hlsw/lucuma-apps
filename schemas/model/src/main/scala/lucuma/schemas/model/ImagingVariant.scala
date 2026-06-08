// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

enum ImagingVariant(val variantType: ImagingVariantType) derives Eq:
  case Grouped(
    order:      WavelengthOrder,
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends ImagingVariant(ImagingVariantType.Grouped)
  case Interleaved(
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends ImagingVariant(ImagingVariantType.Interleaved)
  case PreImaging(offset1: Offset, offset2: Offset, offset3: Offset, offset4: Offset)
      extends ImagingVariant(ImagingVariantType.PreImaging)

  def toVariantType(newVariantType: ImagingVariantType): ImagingVariant =
    this match
      case Grouped(order, offsets, skyCount, skyOffsets)  =>
        newVariantType match
          case ImagingVariantType.Grouped     => this
          case ImagingVariantType.Interleaved => Interleaved(offsets, skyCount, skyOffsets)
          case ImagingVariantType.PreImaging  => PreImaging.Default
      case Interleaved(offsets, skyCount, skyOffsets)     =>
        newVariantType match
          case ImagingVariantType.Grouped     =>
            Grouped(WavelengthOrder.Increasing, offsets, skyCount, skyOffsets)
          case ImagingVariantType.Interleaved => this
          case ImagingVariantType.PreImaging  => PreImaging.Default
      case PreImaging(offset1, offset2, offset3, offset4) =>
        newVariantType match
          case ImagingVariantType.Grouped     =>
            Grouped(WavelengthOrder.Increasing, None, 0.refined, None)
          case ImagingVariantType.Interleaved => Interleaved(None, 0.refined, None)
          case ImagingVariantType.PreImaging  => this

object ImagingVariant:
  val grouped: Prism[ImagingVariant, ImagingVariant.Grouped]         =
    GenPrism[ImagingVariant, ImagingVariant.Grouped]
  val interleaved: Prism[ImagingVariant, ImagingVariant.Interleaved] =
    GenPrism[ImagingVariant, ImagingVariant.Interleaved]
  val preImaging: Prism[ImagingVariant, ImagingVariant.PreImaging]   =
    GenPrism[ImagingVariant, ImagingVariant.PreImaging]

  object Grouped:
    val order: Lens[Grouped, WavelengthOrder]                       = Focus[Grouped](_.order)
    val offsets: Lens[Grouped, Option[TelescopeConfigGenerator]]    =
      Focus[Grouped](_.offsets)
    val skyCount: Lens[Grouped, NonNegInt]                          = Focus[Grouped](_.skyCount)
    val skyOffsets: Lens[Grouped, Option[TelescopeConfigGenerator]] =
      Focus[Grouped](_.skyOffsets)

  object Interleaved:
    val offsets: Lens[Interleaved, Option[TelescopeConfigGenerator]]    =
      Focus[Interleaved](_.offsets)
    val skyCount: Lens[Interleaved, NonNegInt]                          = Focus[Interleaved](_.skyCount)
    val skyOffsets: Lens[Interleaved, Option[TelescopeConfigGenerator]] =
      Focus[Interleaved](_.skyOffsets)

  object PreImaging:
    val Default: PreImaging =
      PreImaging(
        Offset.signedDecimalArcseconds.reverseGet(0, 0),
        Offset.signedDecimalArcseconds.reverseGet(-4, -6),
        Offset.signedDecimalArcseconds.reverseGet(4, -6),
        Offset.signedDecimalArcseconds.reverseGet(8, 0)
      )

    val offset1: Lens[PreImaging, Offset] = Focus[PreImaging](_.offset1)
    val offset2: Lens[PreImaging, Offset] = Focus[PreImaging](_.offset2)
    val offset3: Lens[PreImaging, Offset] = Focus[PreImaging](_.offset3)
    val offset4: Lens[PreImaging, Offset] = Focus[PreImaging](_.offset4)
