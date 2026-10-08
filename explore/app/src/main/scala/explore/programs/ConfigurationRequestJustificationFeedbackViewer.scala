// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ConfigurationRequest
import explore.model.PopupState
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.ui.primereact.*

case class ConfigurationRequestJustificationFeedbackViewer(
  trigger:  Button,
  requests: List[ConfigurationRequest]
) extends ReactFnProps(ConfigurationRequestJustificationFeedbackViewer)

object ConfigurationRequestJustificationFeedbackViewer
    extends ReactFnComponent[ConfigurationRequestJustificationFeedbackViewer](props =>
      for {
        popupState <- useStateView(PopupState.Closed)
        md         <- useMemo(props.requests): requests =>
                        // Requests with the same justification and feedback share a section.
                        val map  = requests
                          .groupMap(r => (r.justification, r.feedback))(_.id)
                          .view
                          .mapValues(_.sorted)
                        val list = map.toList.sortBy(_._2.headOption)
                        if (list.length === 0) "## Nothing selected"
                        else
                          list
                            .map { case ((ojust, ofeedback), ids) =>
                              val just     = ojust.fold("_<No Justification Provided>_")(_.value)
                              val feedback = ofeedback.fold("_<No Feedback Provided>_")(_.value)
                              val idString = ids.mkString("### ", ", ", "")
                              s"$idString\n#### Justification\n$just\n#### Feedback\n$feedback"
                            }
                            .mkString("\n\n")
      } yield
        val close = popupState.set(PopupState.Closed)

        val footer =
          Button(
            label = "Close",
            icon = Icons.Close,
            severity = Button.Severity.Danger,
            onClick = close
          ).small

        React.Fragment(
          props.trigger.copy(onClick = props.trigger.onClick >> popupState.set(PopupState.Open)),
          Dialog(
            visible = popupState.get.value,
            onHide = close,
            closable = true,
            closeOnEscape = true,
            dismissableMask = true,
            resizable = true,
            clazz = LucumaPrimeStyles.Dialog.Small,
            header = "Justification and Feedback Viewer",
            footer = footer
          )(
            <.div(ExploreStyles.HelpMarkdownBody)(ReactMarkdown(content = md))
          )
        )
    )
