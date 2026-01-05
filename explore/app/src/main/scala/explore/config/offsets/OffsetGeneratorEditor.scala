// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import crystal.react.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.*
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

case class OffsetGeneratorEditor(
  id:       NonEmptyString,
  label:    String,
  value:    View[Option[OffsetGenerator]],
  readonly: Boolean
) extends ReactFnProps(OffsetGeneratorEditor)

object OffsetGeneratorEditor
    extends ReactFnComponent[OffsetGeneratorEditor]({ props =>
      val valueOpt: Option[View[OffsetGenerator]]           = props.value.toOptionView
      val randomOpt: Option[View[OffsetGenerator.Random]]   =
        valueOpt.flatMap(_.zoom(OffsetGenerator.random).toOptionView)
      val spiralOpt: Option[View[OffsetGenerator.Spiral]]   =
        valueOpt.flatMap(_.zoom(OffsetGenerator.spiral).toOptionView)
      val uniformOpt: Option[View[OffsetGenerator.Uniform]] =
        valueOpt.flatMap(_.zoom(OffsetGenerator.uniform).toOptionView)

      React.Fragment(
        FormEnumDropdown[GridType](
          id = "grid-type".refined,
          label = props.label,
          value = GridType.fromOffsetGenerator(props.value.get),
          onChange = gt => props.value.set(gt.init),
          placeholder = "Select grid type"
        ),
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
