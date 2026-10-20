// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import cats.syntax.all.*
import crystal.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.ui.primereact.*
import observe.model.UserPrompt
import observe.model.enums.RunOverride
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.model.LoadedObservation
import observe.ui.model.LoadedObservations
import observe.ui.services.SequenceApi

/**
 * Warns the user that the start checks failed and lets them stop or continue anyway. Continuing
 * repeats the action that was refused, this time overriding the checks. Prompts are shown one at a
 * time, oldest first, so none is lost if several observations fail their checks close together.
 */
case class ChecksOverrideDialog(
  prompts:            View[List[UserPrompt.ChecksOverride]],
  loadedObservations: View[LoadedObservations]
) extends ReactFnProps(ChecksOverrideDialog)

private val TargetWarning: String =
  "Target in TCS does not match target in Explore. Verify the telescope is pointing at the correct target and the correct sequence is loaded in Observe."

private def discrepancyLine(
  label:       String,
  discrepancy: Option[UserPrompt.Discrepancy[String]]
): Option[VdomNode] =
  discrepancy.map: d =>
    <.p(s"$label: required ${d.required}, actual ${d.actual}")

private def renderCheck(check: UserPrompt.SeqCheck): VdomNode =
  check match
    case UserPrompt.TargetCheckOverride(d)                     =>
      <.div(
        <.p(TargetWarning),
        <.p(s"Target in sequence: ${d.required}. Target in TCS: ${d.actual}.")
      )
    case UserPrompt.ObsConditionsCheckOverride(cc, iq, sb, wv) =>
      <.div(
        <.p("Observing conditions do not match the observation's constraints."),
        List(
          discrepancyLine("Cloud Extinction", cc),
          discrepancyLine("Image Quality", iq),
          discrepancyLine("Sky Background", sb),
          discrepancyLine("Water Vapor", wv)
        ).flattenOption.toTagMod
      )

object ChecksOverrideDialog
    extends ReactFnComponent[ChecksOverrideDialog](props =>
      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        val current: Option[UserPrompt.ChecksOverride] = props.prompts.get.headOption

        val dismiss: Callback = props.prompts.mod(_.drop(1))

        // Mirrors what the Play button does, so it shows busy until the next state arrives.
        def setRefreshing(obsId: Observation.Id): Callback =
          props.loadedObservations.mod:
            LoadedObservations.Value
              .index(obsId)
              .andThen(Pot.readyPrism)
              .andThen(LoadedObservation.isRefreshing)
              .replace(true)

        Dialog(
          header = <.span(Icons.CircleExclamation.withFixedWidth(), " Warning"),
          visible = current.isDefined,
          closable = false,
          modal = true,
          onHide = Callback.empty,
          clazz = ObserveStyles.ChecksOverrideDialog,
          footer = <.div(
            Button(
              label = "Stop",
              icon = Icons.Stop,
              severity = Button.Severity.Primary,
              onClick = dismiss
            ).compact,
            Button(
              label = "Continue anyway",
              icon = Icons.Play,
              severity = Button.Severity.Danger,
              onClick = dismiss >>
                current.foldMap: p =>
                  p.action match
                    case UserPrompt.ChecksOverride.Action.StartSequence    =>
                      setRefreshing(p.obsId) >>
                        sequenceApi.startSequence(p.obsId, RunOverride.Override).runAsync
                    case UserPrompt.ChecksOverride.Action.ProceedToScience =>
                      sequenceApi
                        .proceedAfterPrompt(p.obsId, SequenceType.Science, RunOverride.Override)
                        .runAsync
            ).compact
          )
        )(
          <.div(current.map(_.checks.toList.map(renderCheck).toTagMod).getOrElse(TagMod.empty))
        )
    )
