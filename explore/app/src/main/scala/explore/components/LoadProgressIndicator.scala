// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.Icons
import explore.cache.LoadProgressRef
import explore.cache.LoadStage
import explore.cache.LoadStageState
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.components.SolarProgress

case class LoadProgressIndicator(progress: LoadProgressRef[IO])
    extends ReactFnProps(LoadProgressIndicator.component)

object LoadProgressIndicator:
  private def stageItem(stage: LoadStage, state: LoadStageState): VdomNode =
    <.li(
      ExploreStyles.LoadProgressStage,
      ExploreStyles.LoadProgressDone.when(state === LoadStageState.Done),
      ^.key := stage.toString
    )(
      <.span(ExploreStyles.LoadProgressSpinner)(Icons.Spinner.withFixedWidth()),
      <.span(stage.label)
    )

  private val component = ScalaFnComponent[LoadProgressIndicator]: props =>
    useStreamOnMount(props.progress.discrete).map: progress =>
      val stages: Option[VdomNode] =
        progress.toOption
          .filter(_.nonEmpty)
          .map: states =>
            <.ul(ExploreStyles.LoadProgressList)(
              states.toList.sortBy(_._1.ordinal).map(stageItem).toTagMod
            )

      SolarProgress(message = stages)
