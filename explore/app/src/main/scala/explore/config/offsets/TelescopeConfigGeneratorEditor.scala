// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.eq.*
import crystal.react.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.ToggleButton
import lucuma.refined.*
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sequence.SequenceIcons
import lucuma.ui.utils.*

case class TelescopeConfigGeneratorEditor(
  id:       NonEmptyString,
  label:    String,
  value:    View[Option[TelescopeConfigGenerator]],
  readonly: Boolean
) extends ReactFnProps(TelescopeConfigGeneratorEditor)

object TelescopeConfigGeneratorEditor
    extends ReactFnComponent[TelescopeConfigGeneratorEditor]({ props =>
      val valueOpt: Option[View[TelescopeConfigGenerator]] = props.value.toOptionView

      val explicitOpt: Option[View[NonEmptyList[TelescopeConfig]]] =
        valueOpt.flatMap:
          _.zoom:
            TelescopeConfigGenerator.enumerated.andThen(TelescopeConfigGenerator.Enumerated.values)
          .toOptionView
      val offsetGeneratorOpt: Option[View[OffsetGenerator]]        =
        valueOpt.flatMap:
          _.zoom:
            TelescopeConfigGenerator.fromOffsetGenerator.andThen:
              TelescopeConfigGenerator.FromOffsetGenerator.offsetGenerator
          .toOptionView

      val randomOpt: Option[View[OffsetGenerator.Random]]   =
        offsetGeneratorOpt.flatMap(_.zoom(OffsetGenerator.random).toOptionView)
      val spiralOpt: Option[View[OffsetGenerator.Spiral]]   =
        offsetGeneratorOpt.flatMap(_.zoom(OffsetGenerator.spiral).toOptionView)
      val uniformOpt: Option[View[OffsetGenerator.Uniform]] =
        offsetGeneratorOpt.flatMap(_.zoom(OffsetGenerator.uniform).toOptionView)

      extension (sgs: StepGuideState.type)
        def fromBoolean: Boolean => StepGuideState =
          case false => StepGuideState.Disabled
          case true  => StepGuideState.Enabled

      React.Fragment(
        FormEnumDropdown[TelescopeConfigGeneratorType](
          id = "grid-type".refined,
          label = props.label,
          value = TelescopeConfigGeneratorType.fromTelescopeConfigGenerator(props.value.get),
          onChange = gt => props.value.set(gt.init),
          placeholder = "Select grid type"
        ),
        explicitOpt.map { explicit =>
          React.Fragment(
            explicit.toNelOfViews.zipWithIndex
              .map: (telescopeConfig, idx) =>
                val offset: View[Offset]          = telescopeConfig.zoom(TelescopeConfig.offset)
                val guiding: View[StepGuideState] = telescopeConfig.zoom(TelescopeConfig.guiding)
                ReactFragment(
                  <.label(^.htmlFor := s"explicit-offsets-$idx", s"Offset ${idx + 1} (arcsec):"),
                  <.div(
                    OffsetInput(
                      id = NonEmptyString.unsafeFrom(s"explicit-offsets-$idx"),
                      offset = offset,
                      readonly = props.readonly,
                      inputClass = LucumaPrimeStyles.FormField
                    ),
                    ToggleButton(
                      onIcon = SequenceIcons.Crosshairs, // TODO
                      offIcon = SequenceIcons.Crosshairs,
                      // disabled = props.readonly,
                      clazz = LucumaPrimeStyles.FormField,
                      checked = guiding.get === StepGuideState.Enabled,
                      onChange = b => guiding.set(StepGuideState.fromBoolean(b))
                    ), // .tiny.compact
                    Button(
                      icon = Icons.Trash,
                      disabled = props.readonly,
                      clazz = LucumaPrimeStyles.FormField,
                      onClick = explicit.mod: offsets =>
                        NonEmptyList
                          .fromList:
                            offsets.take(idx) ++ offsets.toList.drop(idx + 1)
                          .getOrElse(NonEmptyList.one(TelescopeConfig.Default))
                    ).tiny.compact
                  )
                )
              .toList
              .toVdomArray,
            Button(
              icon = Icons.New,
              disabled = props.readonly,
              clazz = LucumaPrimeStyles.FormField,
              onClick = explicit.mod: offsets =>
                offsets.append(TelescopeConfig.Default)
            ).tiny.compact
          )
        },
        randomOpt.map { random =>
          ReactFragment(
            FormInputTextView(
              id = "random-size".refined,
              label = "Size (arcsec):",
              value = random.zoom(OffsetGenerator.Random.size),
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              placeholder = "0.0"
            ),
            <.label(^.htmlFor := "random-size", "Center (arcsec):"),
            OffsetInput(
              id = "random-center".refined,
              offset = random.zoom(OffsetGenerator.Random.center),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            )
          )
        },
        spiralOpt.map { spiral =>
          ReactFragment(
            FormInputTextView(
              id = "spiral-size".refined,
              label = "Size (arcsec):",
              value = spiral.zoom(OffsetGenerator.Spiral.size),
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              placeholder = "0.0"
            ),
            <.label(^.htmlFor := "spiral-center", "Center (arcsec):"),
            OffsetInput(
              id = "spiral-center".refined,
              offset = spiral.zoom(OffsetGenerator.Spiral.center),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            )
          )
        },
        uniformOpt.map { uniform =>
          ReactFragment(
            <.label(^.htmlFor := "uniform-corner-a", "Corner A (arcsec):"),
            OffsetInput(
              id = "uniform-corner-a".refined,
              offset = uniform.zoom(OffsetGenerator.Uniform.cornerA),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            ),
            <.label(^.htmlFor := "uniform-corner-b", "Corner B (arcsec):"),
            OffsetInput(
              id = "uniform-corner-b".refined,
              offset = uniform.zoom(OffsetGenerator.Uniform.cornerB),
              readonly = props.readonly,
              inputClass = LucumaPrimeStyles.FormField
            )
          )
        }
      )
    })
