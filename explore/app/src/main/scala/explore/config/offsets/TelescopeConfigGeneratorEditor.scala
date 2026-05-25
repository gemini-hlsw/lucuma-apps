// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

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

      React.Fragment(
        FormEnumDropdown[TelescopeConfigGeneratorType](
          id = "grid-type".refined,
          label = props.label,
          value = TelescopeConfigGeneratorType.fromTelescopeConfigGenerator(props.value.get),
          onChange = gt => props.value.set(gt.init),
          disabled = props.readonly
        ),
        explicitOpt.map: explicit =>
          TelescopeConfigsEditor(
            telescopeConfigs = explicit,
            maxOffsets = props.maxExplicit,
            readonly = props.readonly
          ),
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
