// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import japgolly.scalajs.react.*
import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.validation.MathValidators
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.model.SlotId
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.*
import lucuma.ui.syntax.all.given

// Coordinate editor position used for sky and base positions.
case class PositionCoordinatesEditor(
  slot:        SlotId,
  coordinates: View[Coordinates],
  readonly:    Boolean
) extends ReactFnProps[PositionCoordinatesEditor](PositionCoordinatesEditor)

object PositionCoordinatesEditor
    extends ReactFnComponent[PositionCoordinatesEditor](props =>
      val coordsRAView: View[RightAscension] =
        props.coordinates.zoom(Coordinates.rightAscension)

      val coordsDecView: View[Declination] =
        props.coordinates.zoom(Coordinates.declination)

      def inputId(field: String): NonEmptyString =
        NonEmptyString.unsafeFrom(s"${props.slot.tag}-$field")

      React.Fragment(
        FormInputTextView(
          id = inputId("ra"),
          value = coordsRAView,
          label = React.Fragment("RA", HelpIcon("target/main/coordinates.md".refined)),
          validFormat = MathValidators.truncatedRA,
          changeAuditor = ChangeAuditor.accept,
          validateOnPaste = false,
          disabled = props.readonly
        ),
        FormInputTextView(
          id = inputId("dec"),
          value = coordsDecView,
          label = React.Fragment("Dec", HelpIcon("target/main/coordinates.md".refined)),
          validFormat = MathValidators.truncatedDec,
          changeAuditor = ChangeAuditor.accept,
          validateOnPaste = false,
          disabled = props.readonly
        )
      )
    )
