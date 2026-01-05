// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Endo
import cats.syntax.option.*
import crystal.react.View
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import explore.config.offsets.OffsetGeneratorEditor
import explore.config.offsets.OffsetInput
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.geom.OffsetGenerator
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
import explore.config.offsets.OffsetEditorStyles

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

      val offsetGeneratorGetter: Option[TelescopeConfigGenerator] => Option[OffsetGenerator] =
        _.flatMap(TelescopeConfigGenerator.fromOffsetGenerator.getOption).map(_.offsetGenerator)

      val offsetGeneratorModder
        : Endo[Option[OffsetGenerator]] => Endo[Option[TelescopeConfigGenerator]] =
        mod =>
          tcOpt =>
            mod(offsetGeneratorGetter(tcOpt)).map(TelescopeConfigGenerator.FromOffsetGenerator(_))

      React.Fragment(
        // TODO Customized version
        // TODO FormEnumDropdown
        FormEnumDropdown[GmosImagingVariantType](
          id = "variant-type".refined,
          value = props.variantType,
          onChange = vt => props.variant.mod(_.toVariantType(vt)),
          label = "Variant".some,
          //   helpId = Some("configuration/imaging/variant-type.md".refined),
          clazz = LucumaPrimeStyles.FormField,
          disabled = props.readonly
        ),
        groupedView.map[VdomNode]: grouped =>
          React.Fragment(
            FormEnumDropdownView(
              id = "wavelength-order".refined,
              value = grouped.zoom(GmosImagingVariant.Grouped.order),
              label = "Wavelength Order".some,
              //   helpId = Some("configuration/imaging/wavelength-order.md".refined),
              clazz = LucumaPrimeStyles.FormField,
              disabled = props.readonly
            ),
            OffsetGeneratorEditor(
              id = "grouped-offsets".refined,
              label = "Offsets",
              value = grouped
                .zoom(GmosImagingVariant.Grouped.offsets)
                .zoom(offsetGeneratorGetter)(offsetGeneratorModder),
              readonly = props.readonly
            ),
            <.label(^.htmlFor := "grouped-sky-count", "Sky Count"),
            FormInputTextView(
              id = "grouped-sky-count".refined,
              value = grouped.zoom(GmosImagingVariant.Grouped.skyCount),
              validFormat = InputValidSplitEpi.nonNegInt,
              placeholder = "0",
              disabled = props.readonly
            ),
            OffsetGeneratorEditor(
              id = "grouped-sky-offsets".refined,
              label = "Sky Offsets",
              value = grouped
                .zoom(GmosImagingVariant.Grouped.skyOffsets)
                .zoom(offsetGeneratorGetter)(offsetGeneratorModder),
              readonly = props.readonly
            )
          ),
        interleavedView.map[VdomNode]: interleaved =>
          React.Fragment(
            OffsetGeneratorEditor(
              id = "interleaved-offsets".refined,
              label = "Offsets",
              value = interleaved
                .zoom(GmosImagingVariant.Interleaved.offsets)
                .zoom(offsetGeneratorGetter)(offsetGeneratorModder),
              readonly = props.readonly
            ),
            <.label(^.htmlFor := "interleaved-sky-count", "Sky Count"),
            FormInputTextView(
              id = "interleaved-sky-count".refined,
              value = interleaved.zoom(GmosImagingVariant.Interleaved.skyCount),
              validFormat = InputValidSplitEpi.nonNegInt,
              placeholder = "0",
              disabled = props.readonly
            ),
            OffsetGeneratorEditor(
              id = "interleaved-sky-offsets".refined,
              label = "Sky Offsets",
              value = interleaved
                .zoom(GmosImagingVariant.Interleaved.skyOffsets)
                .zoom(offsetGeneratorGetter)(offsetGeneratorModder),
              readonly = props.readonly
            )
          ),
        preImagingView.map[VdomNode]: preImaging =>
          // TODO: Should this be shown in a grid?
          //   React.Fragment(
          <.div(OffsetEditorStyles.Content)(
            <.div(OffsetEditorStyles.OffsetsFormRow)(
              <.label(^.htmlFor := "preImaging-offset-1", "Offset 1 (arcsec):"),
              OffsetInput(
                id = "preImaging-offset-1".refined,
                offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset1),
                readonly = props.readonly,
                inputClass = LucumaPrimeStyles.FormField,
                labelClass = OffsetEditorStyles.OffsetLabel
              )
            ),
            <.div(OffsetEditorStyles.OffsetsFormRow)(
              <.label(^.htmlFor := "preImaging-offset-2", "Offset 2 (arcsec):"),
              OffsetInput(
                id = "preImaging-offset-2".refined,
                offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset2),
                readonly = props.readonly,
                inputClass = LucumaPrimeStyles.FormField,
                labelClass = OffsetEditorStyles.OffsetLabel
              )
            ),
            <.div(OffsetEditorStyles.OffsetsFormRow)(
              <.label(^.htmlFor := "preImaging-offset-3", "Offset 3 (arcsec):"),
              OffsetInput(
                id = "preImaging-offset-3".refined,
                offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset3),
                readonly = props.readonly,
                inputClass = LucumaPrimeStyles.FormField,
                labelClass = OffsetEditorStyles.OffsetLabel
              )
            ),
            <.div(OffsetEditorStyles.OffsetsFormRow)(
              <.label(^.htmlFor := "preImaging-offset-4", "Offset 4 (arcsec):"),
              OffsetInput(
                id = "preImaging-offset-4".refined,
                offset = preImaging.zoom(GmosImagingVariant.PreImaging.offset4),
                readonly = props.readonly,
                inputClass = LucumaPrimeStyles.FormField,
                labelClass = OffsetEditorStyles.OffsetLabel
              )
            )
          )
      )
    })
