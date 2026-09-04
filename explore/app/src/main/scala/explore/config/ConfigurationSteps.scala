// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

// ConfigurationSteps, not by the declaration order here.
enum ConfigurationStep(val label: String) derives Eq:
  case Requirements extends ConfigurationStep("Set the science requirements")
  case SelectRow    extends ConfigurationStep("Select a configuration from the table")
  case Details      extends ConfigurationStep("Fill in the instrument details")
  case Accept       extends ConfigurationStep("Accept the configuration")

// Shows the whole configuration workflow at once. Each step is paired with whether it is already
// satisfied
case class ConfigurationSteps(
  steps:   NonEmptyList[(ConfigurationStep, Boolean)],
  blocker: Option[String]
) extends ReactFnProps(ConfigurationSteps.component)

object ConfigurationSteps:
  private type Props = ConfigurationSteps

  private val component =
    ScalaFnComponent[Props]: props =>
      val current: Option[ConfigurationStep] =
        props.steps.find(!_._2).map(_._1)

      <.ol(ExploreStyles.ConfigurationSteps)(
        props.steps.toList.zipWithIndex.map { case ((step, done), index) =>
          val isCurrent        = current.contains_(step)
          val blocker          = props.blocker.filter(_ => isCurrent)
          val marker: VdomNode =
            if done then Icons.Checkmark else (index + 1).toString
          <.li(
            ^.key := index,
            ExploreStyles.ConfigurationStepDone.when(done),
            ExploreStyles.ConfigurationStepCurrent.when(isCurrent && blocker.isEmpty),
            ExploreStyles.ConfigurationStepBlocked.when(blocker.isDefined)
          )(
            <.span(ExploreStyles.ConfigurationStepMarker)(marker),
            <.span(step.label),
            blocker.map(b => <.span(ExploreStyles.ConfigurationStepBlockerNote)(b))
          )
        }.toVdomArray
      )
