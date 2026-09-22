// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.Icons
import explore.cache.LoadProgressRef
import explore.cache.LoadStep
import explore.cache.LoadStepState
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.SolarProgress

case class LoadProgressIndicator(progress: LoadProgressRef[IO])
    extends ReactFnProps(LoadProgressIndicator.component)

object LoadProgressIndicator:
  private def stepItem(step: LoadStep, state: LoadStepState): VdomNode =
    val (indicator: VdomNode, stateCss: Css) = state match
      case LoadStepState.InFlight =>
        (Icons.Spinner.withSpin(true).withFixedWidth(), Css.Empty)
      case LoadStepState.Done     =>
        (Icons.Checkmark.withFixedWidth(), ExploreStyles.LoadProgressDone)

    <.li(ExploreStyles.LoadProgressStep |+| stateCss, ^.key := step.toString)(
      indicator,
      <.span(step.label)
    )

  private val component = ScalaFnComponent[LoadProgressIndicator]: props =>
    useStreamOnMount(props.progress.discrete).map: progress =>
      val checklist: Option[VdomNode] =
        progress.toOption
          .filter(_.nonEmpty)
          .map: states =>
            <.ul(ExploreStyles.LoadProgressList)(
              states.toList.sortBy(_._1.ordinal).map(stepItem).toTagMod
            )

      SolarProgress(message = checklist)
