// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.syntax.semigroup.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.Undoer

case class UndoButtons(
  undoer:   Undoer,
  size:     PlSize = PlSize.Tiny,
  disabled: Boolean = false,
  loading:  Boolean = false
) extends ReactFnProps[UndoButtons](UndoButtons.component)

object UndoButtons:
  private type Props = UndoButtons

  private val ButtonsUndoCss: Css = Css("lucuma-ui-buttons-undo") |+| Css("p-button-group")

  private val component =
    ScalaFnComponent[Props](props =>
      <.span(ButtonsUndoCss)(
        Button(
          severity = Button.Severity.Secondary,
          outlined = true,
          onClickE = _.stopPropagationCB >> props.undoer.undo,
          disabled = props.undoer.isUndoEmpty || props.disabled || props.undoer.working,
          loading = props.undoer.working || props.loading,
          clazz = props.size.cls,
          icon = UndoIcons.Undo,
          tooltip = "Undo",
          tooltipOptions = DefaultTooltipOptions
        ).compact,
        Button(
          severity = Button.Severity.Secondary,
          outlined = true,
          onClickE = _.stopPropagationCB >> props.undoer.redo,
          disabled = props.undoer.isRedoEmpty || props.disabled || props.undoer.working,
          loading = props.undoer.working || props.loading,
          clazz = props.size.cls,
          icon = UndoIcons.Redo,
          tooltip = "Redo",
          tooltipOptions = DefaultTooltipOptions
        ).compact
      )
    )
