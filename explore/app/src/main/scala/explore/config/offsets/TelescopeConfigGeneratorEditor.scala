// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.ToggleButton
import lucuma.refined.*
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sequence.SequenceIcons
import lucuma.ui.sequence.SequenceStyles
import lucuma.ui.utils.*
import monocle.Iso

final case class TelescopeConfigGeneratorEditor(
  id:          NonEmptyString,
  label:       String,
  value:       View[Option[TelescopeConfigGenerator]],
  showCenter:  Boolean,
  readonly:    Boolean,
  maxExplicit: Int = Int.MaxValue
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

      for _ <- useEffectWithDeps(props.maxExplicit): maxExplicit =>
                 props.value // If maxExplicit changes, ensure we trim the list if needed
                   .zoom:
                     Iso.id.some
                       .andThen(TelescopeConfigGenerator.enumerated)
                       .andThen(TelescopeConfigGenerator.Enumerated.values)
                   .mod: oldList =>
                     NonEmptyList
                       .fromList:
                         oldList.take(maxExplicit)
                       .getOrElse(NonEmptyList.one(TelescopeConfig.Default))
      yield React.Fragment(
        FormEnumDropdown[TelescopeConfigGeneratorType](
          id = "grid-type".refined,
          label = props.label,
          value = TelescopeConfigGeneratorType.fromTelescopeConfigGenerator(props.value.get),
          onChange = gt => props.value.set(gt.init),
          disabled = props.readonly
        ),
        explicitOpt.map { explicit =>
          React.Fragment(
            explicit.toNelOfViews.zipWithIndex
              .map: (telescopeConfig, idx) =>
                val offset: View[Offset]          = telescopeConfig.zoom(TelescopeConfig.offset)
                val guiding: View[StepGuideState] = telescopeConfig.zoom(TelescopeConfig.guiding)
                React.Fragment.withKey(s"explicit-offsets-row-$idx")(
                  <.label(^.htmlFor := s"explicit-offsets-$idx", s"Offset ${idx + 1} (arcsec):"),
                  <.div(OffsetGeneratorEditorStyles.ExplicitRow)(
                    OffsetInput(
                      id = NonEmptyString.unsafeFrom(s"explicit-offsets-$idx"),
                      offset = offset,
                      readonly = props.readonly,
                      inputClass = LucumaPrimeStyles.FormField
                    ),
                    ToggleButton(
                      onIcon = SequenceIcons.Crosshairs.addClass(SequenceStyles.StepGuided),
                      offIcon = SequenceIcons.Crosshairs.addClass(
                        OffsetGeneratorEditorStyles.ExplicitUnguided
                      ),
                      onLabel = "",
                      offLabel = "",
                      tooltip = "Toggle Guiding",
                      disabled = props.readonly,
                      text = true,
                      clazz = LucumaPrimeStyles.FormField |+| PrimeStyles.ButtonIconOnly |+|
                        OffsetGeneratorEditorStyles.ToggleButton,
                      checked = guiding.get === StepGuideState.Enabled,
                      onChange = b => guiding.set(StepGuideState.fromBoolean(b))
                    ).mini.compact,
                    Button(
                      icon = Icons.Trash.addClass(
                        OffsetGeneratorEditorStyles.RemoveOffsetFirstIcon.when_(idx === 0)
                      ),
                      tooltip = "Remove Offset",
                      disabled = props.readonly || idx === 0,
                      text = true,
                      clazz = LucumaPrimeStyles.FormField,
                      onClick = explicit.mod: offsets =>
                        NonEmptyList
                          .fromList:
                            offsets.take(idx) ++ offsets.toList.drop(idx + 1)
                          .getOrElse(NonEmptyList.one(TelescopeConfig.Default))
                    ).mini.compact
                  )
                )
              .toList
              .toVdomArray,
            Button(
              icon = Icons.ThinPlus,
              severity = Button.Severity.Success,
              disabled = props.readonly || explicit.get.length >= props.maxExplicit,
              tooltip = "Add Offset",
              text = true,
              clazz = LucumaPrimeStyles.FormField |+| OffsetGeneratorEditorStyles.AddOffset,
              onClick = explicit.mod: offsets =>
                offsets.append(TelescopeConfig.Default)
            ).mini.compact
          )
        },
        randomOpt.map { random =>
          React.Fragment(
            FormInputTextView(
              id = "random-size".refined,
              label = "Size (arcsec):",
              value = random.zoom(OffsetGenerator.Random.size),
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              disabled = props.readonly,
              placeholder = "0.0"
            ),
            if props.showCenter then
              React.Fragment(
                <.label(^.htmlFor := "random-size", "Center (arcsec):"),
                OffsetInput(
                  id = "random-center".refined,
                  offset = random.zoom(OffsetGenerator.Random.center),
                  readonly = props.readonly,
                  inputClass = LucumaPrimeStyles.FormField
                )
              )
            else EmptyVdom
          )
        },
        spiralOpt.map { spiral =>
          React.Fragment(
            FormInputTextView(
              id = "spiral-size".refined,
              label = "Size (arcsec):",
              value = spiral.zoom(OffsetGenerator.Spiral.size),
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              disabled = props.readonly,
              placeholder = "0.0"
            ),
            if props.showCenter then
              React.Fragment(
                <.label(^.htmlFor := "spiral-center", "Center (arcsec):"),
                OffsetInput(
                  id = "spiral-center".refined,
                  offset = spiral.zoom(OffsetGenerator.Spiral.center),
                  readonly = props.readonly,
                  inputClass = LucumaPrimeStyles.FormField
                )
              )
            else EmptyVdom
          )
        },
        uniformOpt.map { uniform =>
          React.Fragment(
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
