// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.HelpIcon
import explore.config.offsets.OffsetGeneratorEditorStyles
import explore.config.offsets.OffsetInput
import explore.config.offsets.TelescopeConfigGeneratorEditor
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
import lucuma.schemas.model.TelescopeConfigGenerator
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

      def commonInputs(
        offsets:    View[Option[TelescopeConfigGenerator]],
        skyCount:   View[NonNegInt],
        skyOffsets: View[Option[TelescopeConfigGenerator]]
      ): VdomElement =
        React.Fragment(
          TelescopeConfigGeneratorEditor(
            id = "grouped-offsets".refined,
            label = "Offsets",
            value = offsets,
            showCenter = false,
            readonly = props.readonly
          ),
          <.hr(OffsetGeneratorEditorStyles.Separator),
          FormInputTextView(
            id = "grouped-sky-count".refined,
            label = React.Fragment(
              "Sky Offset Count",
              HelpIcon("configuration/imaging/sky-offset.md".refined)
            ),
            value = skyCount.withOnMod: count =>
              if count.value === 0 then skyOffsets.set(none) else Callback.empty,
            validFormat = InputValidSplitEpi.nonNegInt,
            placeholder = "0",
            disabled = props.readonly
          ),
          TelescopeConfigGeneratorEditor(
            id = "grouped-sky-offsets".refined,
            label = "Sky Offsets",
            value = skyOffsets,
            showCenter = true,
            readonly = props.readonly || skyCount.get.value === 0,
            maxExplicit = skyCount.get.value.toInt * 2
          )
        )

      React.Fragment(
        FormEnumDropdown[GmosImagingVariantType](
          id = "variant-type".refined,
          value = props.variantType,
          onChange = vt => props.variant.mod(_.toVariantType(vt)),
          label = React.Fragment(
            "Offset Variant",
            HelpIcon("configuration/imaging/variant-type.md".refined)
          ),
          clazz = LucumaPrimeStyles.FormField,
          disabled = props.readonly
        ),
        groupedView.map[VdomNode]: grouped =>
          React.Fragment(
            FormEnumDropdownView(
              id = "wavelength-order".refined,
              value = grouped.zoom(GmosImagingVariant.Grouped.order),
              label = "Wavelength Order".some,
              clazz = LucumaPrimeStyles.FormField,
              disabled = props.readonly
            ),
            commonInputs(
              grouped.zoom(GmosImagingVariant.Grouped.offsets),
              grouped.zoom(GmosImagingVariant.Grouped.skyCount),
              grouped.zoom(GmosImagingVariant.Grouped.skyOffsets)
            )
          ),
        interleavedView.map[VdomNode]: interleaved =>
          commonInputs(
            interleaved.zoom(GmosImagingVariant.Interleaved.offsets),
            interleaved.zoom(GmosImagingVariant.Interleaved.skyCount),
            interleaved.zoom(GmosImagingVariant.Interleaved.skyOffsets)
          ),
        preImagingView.map[VdomNode]: preImaging =>
          React.Fragment(
            <.label(^.htmlFor := "preImaging-offset-1", "Offset 1 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-1".refined,
              offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset1),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-2", "Offset 2 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-2".refined,
              offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset2),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-3", "Offset 3 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-3".refined,
              offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset3),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-4", "Offset 4 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-4".refined,
              offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset4),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            )
          )
      )
    })
