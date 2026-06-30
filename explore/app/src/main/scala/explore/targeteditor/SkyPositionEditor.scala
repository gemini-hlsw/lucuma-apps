// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import lucuma.core.math.Coordinates
import lucuma.ui.syntax.*
import crystal.react.*
import japgolly.scalajs.react.React
import japgolly.scalajs.react.*
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import explore.components.HelpIcon
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SkyPositionEditor(
  coordinates: View[Coordinates],
  readonly:    Boolean
) extends ReactFnProps[SkyPositionEditor](SkyPositionEditor)

object SkyPositionEditor
    extends ReactFnComponent[SkyPositionEditor](props =>
      val coordsRAView: View[RightAscension] =
        props.coordinates.zoom(Coordinates.rightAscension)

      val coordsDecView: View[Declination] =
        props.coordinates.zoom(Coordinates.declination)

      React.Fragment(
        FormInputTextView(
          id = "sky-ra".refined,
          value = coordsRAView,
          label = React.Fragment("RA", HelpIcon("target/main/coordinates.md".refined)),
          validFormat = MathValidators.truncatedRA,
          changeAuditor = ChangeAuditor.accept,
          validateOnPaste = false,
          disabled = props.readonly
        ),
        FormInputTextView(
          id = "sky-dec".refined,
          value = coordsDecView,
          label = React.Fragment("Dec", HelpIcon("target/main/coordinates.md".refined)),
          validFormat = MathValidators.truncatedDec,
          changeAuditor = ChangeAuditor.accept,
          validateOnPaste = false,
          disabled = props.readonly
        )
      )
    )
