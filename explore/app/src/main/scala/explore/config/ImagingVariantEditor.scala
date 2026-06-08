// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.HelpIcon
import explore.config.offsets.OffsetInput
import explore.config.offsets.TelescopeConfigEditorStyles
import explore.config.offsets.TelescopeConfigGeneratorEditor
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Lens
import monocle.Prism

final case class ImagingVariantEditor(variant: View[ImagingVariant], readonly: Boolean)
    extends ReactFnProps(ImagingVariantEditor)

object ImagingVariantEditor
    extends ReactFnComponent[ImagingVariantEditor](props =>
      val enumeratedValues: Prism[Option[TelescopeConfigGenerator], NonEmptyList[TelescopeConfig]] =
        Iso.id.some
          .andThen(TelescopeConfigGenerator.enumerated)
          .andThen(TelescopeConfigGenerator.Enumerated.values)

      // For Grouped and Interleaved, this sets the skyCount and skyOffsets as needed
      def skyOffsetModifier[A <: ImagingVariant](
        skyCountLens:  Lens[A, NonNegInt],
        skyOffsetLens: Lens[A, Option[TelescopeConfigGenerator]]
      ): (A, A) => A = (oldVariant, newVariant) =>
        val oldCount                                 = skyCountLens.get(oldVariant)
        val newCount                                 = skyCountLens.get(newVariant)
        val newTcg: Option[TelescopeConfigGenerator] = skyOffsetLens.get(newVariant)
        val oldTcg: Option[TelescopeConfigGenerator] = skyOffsetLens.get(oldVariant)

        val updatedVariant: A =
          if newTcg.isEmpty && oldTcg.isDefined && newCount.value =!= 0 then
            // has been changed to No Offsets
            skyCountLens.replace(0.refined)(newVariant)
          else if newCount === oldCount then newVariant
          else if newCount.value === 0 then skyOffsetLens.replace(none)(newVariant)
          else if oldCount.value === 0 then
            val updatedTcg =
              TelescopeConfigGenerator
                .Enumerated(NonEmptyList.one(TelescopeConfig.Default))
            skyOffsetLens.replace(updatedTcg.some)(newVariant)
          else // truncate the list of explicit values if needed
            val updatedTcg = enumeratedValues.modify(nel =>
              NonEmptyList
                .fromList:
                  nel.take(newCount.value.toInt * 2)
                .getOrElse(NonEmptyList.one(TelescopeConfig.Default))
            )(newTcg)
            skyOffsetLens.replace(updatedTcg)(newVariant)
        updatedVariant

      val groupedView: Option[View[ImagingVariant.Grouped]]         =
        props.variant
          .zoom(ImagingVariant.grouped)
          .toOptionView
          .map:
            _.withModPatch(
              skyOffsetModifier(
                ImagingVariant.Grouped.skyCount,
                ImagingVariant.Grouped.skyOffsets
              )
            )
      val interleavedView: Option[View[ImagingVariant.Interleaved]] =
        props.variant
          .zoom(ImagingVariant.interleaved)
          .toOptionView
          .map:
            _.withModPatch(
              skyOffsetModifier(
                ImagingVariant.Interleaved.skyCount,
                ImagingVariant.Interleaved.skyOffsets
              )
            )
      val preImagingView: Option[View[ImagingVariant.PreImaging]]   =
        props.variant
          .zoom(ImagingVariant.preImaging)
          .toOptionView
          .map:
            _.withOnMod(props.variant.set) // no extra work to do here.

      val variantType: ImagingVariantType = props.variant.get match
        case ImagingVariant.Grouped(_, _, _, _)    => ImagingVariantType.Grouped
        case ImagingVariant.Interleaved(_, _, _)   => ImagingVariantType.Interleaved
        case ImagingVariant.PreImaging(_, _, _, _) => ImagingVariantType.PreImaging

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
          <.hr(TelescopeConfigEditorStyles.Separator),
          FormInputTextView(
            id = "grouped-sky-count".refined,
            label = React.Fragment(
              "Sky Offset Count",
              HelpIcon("configuration/imaging/sky-offset.md".refined)
            ),
            value = skyCount,
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
        FormEnumDropdown[ImagingVariantType](
          id = "variant-type".refined,
          value = variantType,
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
              value = grouped.zoom(ImagingVariant.Grouped.order),
              label = "Wavelength Order".some,
              clazz = LucumaPrimeStyles.FormField,
              disabled = props.readonly
            ),
            commonInputs(
              grouped.zoom(ImagingVariant.Grouped.offsets),
              grouped.zoom(ImagingVariant.Grouped.skyCount),
              grouped.zoom(ImagingVariant.Grouped.skyOffsets)
            )
          ),
        interleavedView.map[VdomNode]: interleaved =>
          commonInputs(
            interleaved.zoom(ImagingVariant.Interleaved.offsets),
            interleaved.zoom(ImagingVariant.Interleaved.skyCount),
            interleaved.zoom(ImagingVariant.Interleaved.skyOffsets)
          ),
        preImagingView.map[VdomNode]: preImaging =>
          React.Fragment(
            <.label(^.htmlFor := "preImaging-offset-1", "Offset 1 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-1".refined,
              offset = preImaging.zoom(ImagingVariant.PreImaging.offset1),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-2", "Offset 2 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-2".refined,
              offset = preImaging.zoom(ImagingVariant.PreImaging.offset2),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-3", "Offset 3 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-3".refined,
              offset = preImaging.zoom(ImagingVariant.PreImaging.offset3),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "preImaging-offset-4", "Offset 4 (arcsec):"),
            OffsetInput(
              id = "preImaging-offset-4".refined,
              offset = preImaging.zoom(ImagingVariant.PreImaging.offset4),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            )
          )
      )
    )
