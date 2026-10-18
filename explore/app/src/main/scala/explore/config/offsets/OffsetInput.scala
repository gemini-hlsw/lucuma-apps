// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.inputCodecs
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Offset
import lucuma.react.common.*
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

final case class OffsetInput(
  id:         NonEmptyString,
  offset:     View[Offset],
  readonly:   Boolean,
  pEnabled:   Boolean = true,
  qEnabled:   Boolean = true,
  inputClass: Css = Css.Empty,
  labelClass: Css = Css.Empty,
  clazz:      Css = Css.Empty // for the outter div
) extends ReactFnProps(OffsetInput):
  val pId: NonEmptyString = NonEmptyString.unsafeFrom(s"${id.value}-p")
  val qId: NonEmptyString = NonEmptyString.unsafeFrom(s"${id.value}-q")

object OffsetInput
    extends ReactFnComponent[OffsetInput](props =>
      <.div(TelescopeConfigEditorStyles.OffsetsInput |+| props.clazz)(
        FormInputTextView(
          id = props.pId,
          label = "p:",
          units = "\"",
          value = props.offset.zoom(Offset.pAngle),
          codec = inputCodecs.OffsetArcseconds,
          placeholder = "0.0",
          disabled = props.readonly || !props.pEnabled,
          inputClass = props.inputClass,
          labelClass = props.labelClass
        ),
        FormInputTextView(
          id = props.qId,
          label = "q:",
          units = "\"",
          value = props.offset.zoom(Offset.qAngle),
          codec = inputCodecs.OffsetArcseconds,
          placeholder = "0.0",
          disabled = props.readonly || !props.qEnabled,
          inputClass = props.inputClass,
          labelClass = props.labelClass
        )
      )
    )
