// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Enums.GmosImagingVariantType
import lucuma.schemas.model.GmosImagingVariant
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

final case class GmosImagingVariantEditor(variant: View[GmosImagingVariant], readonly: Boolean)
    extends ReactFnProps(GmosImagingVariantEditor):
  val variantType: GmosImagingVariantType = variant.get match
    case GmosImagingVariant.Grouped(_, _, _, _)    => GmosImagingVariantType.Grouped
    case GmosImagingVariant.Interleaved(_, _, _)   => GmosImagingVariantType.Interleaved
    case GmosImagingVariant.PreImaging(_, _, _, _) => GmosImagingVariantType.PreImaging

object GmosImagingVariantEditor
    extends ReactFnComponent[GmosImagingVariantEditor]({ props =>
      val groupedView: Option[View[GmosImagingVariant.Grouped]]         =
        props.variant.zoom(GmosImagingVariant.grouped).toOptionView
      val interleavedView: Option[View[GmosImagingVariant.Interleaved]] =
        props.variant.zoom(GmosImagingVariant.interleaved).toOptionView
      val preImagingView: Option[View[GmosImagingVariant.PreImaging]]   =
        props.variant.zoom(GmosImagingVariant.preImaging).toOptionView

      React.Fragment(
        // TODO Customized version
        // TODO FormEnumDropdown
        EnumDropdown[GmosImagingVariantType](
          id = "variant-type".refined,
          value = props.variantType,
          onChange = vt => props.variant.mod(_.toVariantType(vt)),
          // label = "Variant".some,
          // helpId = Some("configuration/imaging/variant-type.md".refined),
          clazz = LucumaPrimeStyles.FormField,
          disabled = props.readonly
        ),
        groupedView.map[VdomNode]: grouped =>
          React.Fragment(
            EnumDropdownView(
              id = "wavelength-order".refined,
              value = grouped.zoom(GmosImagingVariant.Grouped.order),
              //   label = "Wavelength Order".some,
              //   helpId = Some("configuration/imaging/wavelength-order.md".refined),
              clazz = LucumaPrimeStyles.FormField,
              disabled = props.readonly
            ),
            <.label(^.htmlFor := "sky-count", "SkyCount:"),
            FormInputTextView(
              id = "sky-count".refined,
              value = grouped.zoom(GmosImagingVariant.Grouped.skyCount),
              validFormat = InputValidSplitEpi.nonNegInt,
              placeholder = "0",
              disabled = props.readonly
            )
          ),
        interleavedView.map[VdomNode]: interleaved =>
          React.Fragment(
            FormInputTextView(
              id = "sky-count".refined,
              value = interleaved.zoom(GmosImagingVariant.Interleaved.skyCount),
              validFormat = InputValidSplitEpi.nonNegInt,
              placeholder = "0",
              disabled = props.readonly
            )
          ),
        preImagingView.map[VdomNode]: preImaging =>
          React.Fragment(
            <.div(preImaging.get.toString)
          )
      )
    })
