// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.HelpIcon
import explore.config.offsets.OffsetGeneratorEditorStyles
import explore.config.offsets.OffsetInput
import explore.config.offsets.TelescopeConfigGeneratorEditor
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.sequence.TelescopeConfig
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
import monocle.Iso
import monocle.Lens
import monocle.Prism

final case class GmosImagingVariantEditor(variant: View[GmosImagingVariant], readonly: Boolean)
    extends ReactFnProps(GmosImagingVariantEditor)

object GmosImagingVariantEditor
    extends ReactFnComponent[GmosImagingVariantEditor](props =>
      val enumeratedValues: Prism[Option[TelescopeConfigGenerator], NonEmptyList[TelescopeConfig]] =
        Iso.id.some
          .andThen(TelescopeConfigGenerator.enumerated)
          .andThen(TelescopeConfigGenerator.Enumerated.values)

      // Using a local view so that we can "bulk update" sky count and sky offsets,
      // see sc-7740
      for
        localVariant <- useStateView(props.variant.get)
        _            <- useEffectWithDeps(props.variant.get): newVariant =>
                          if newVariant != localVariant.get then localVariant.set(newVariant)
                          else Callback.empty
      yield
        // For Grouped and Interleaved, this sets the skyCount and skyOffsets as needed
        // when the local view changes, then updates props.variant.
        def skyOffsetModifier[A <: GmosImagingVariant](
          skyCountLens:  Lens[A, NonNegInt],
          skyOffsetLens: Lens[A, Option[TelescopeConfigGenerator]]
        ): (A, A) => Callback = (oldVariant, newVariant) =>
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
          props.variant.set(updatedVariant)

        val groupedView: Option[View[GmosImagingVariant.Grouped]]         =
          localVariant
            .zoom(GmosImagingVariant.grouped)
            .toOptionView
            .map:
              _.withOnMod(
                skyOffsetModifier(
                  GmosImagingVariant.Grouped.skyCount,
                  GmosImagingVariant.Grouped.skyOffsets
                )
              )
        val interleavedView: Option[View[GmosImagingVariant.Interleaved]] =
          localVariant
            .zoom(GmosImagingVariant.interleaved)
            .toOptionView
            .map:
              _.withOnMod(
                skyOffsetModifier(
                  GmosImagingVariant.Interleaved.skyCount,
                  GmosImagingVariant.Interleaved.skyOffsets
                )
              )
        val preImagingView: Option[View[GmosImagingVariant.PreImaging]]   =
          localVariant
            .zoom(GmosImagingVariant.preImaging)
            .toOptionView
            .map:
              _.withOnMod(props.variant.set) // no extra work to do here.

        val variantType: GmosImagingVariantType = localVariant.get match
          case GmosImagingVariant.Grouped(_, _, _, _)    => GmosImagingVariantType.Grouped
          case GmosImagingVariant.Interleaved(_, _, _)   => GmosImagingVariantType.Interleaved
          case GmosImagingVariant.PreImaging(_, _, _, _) => GmosImagingVariantType.PreImaging

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
          FormEnumDropdown[GmosImagingVariantType](
            id = "variant-type".refined,
            value = variantType,
            onChange = vt => localVariant.mod(_.toVariantType(vt)),
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
    )
