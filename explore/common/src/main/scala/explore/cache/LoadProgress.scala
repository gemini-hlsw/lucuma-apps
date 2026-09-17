// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Eq
import cats.derived.*
import fs2.concurrent.SignallingRef

enum LoadStep(val tag: String) derives Eq:
  case Observations          extends LoadStep("Observations")
  case Groups                extends LoadStep("Groups")
  case ProgramDetails        extends LoadStep("Program details")
  case Targets               extends LoadStep("Targets")
  case Attachments           extends LoadStep("Attachments")
  case Programs              extends LoadStep("Program list")
  case ConfigurationRequests extends LoadStep("Configuration requests")

enum LoadStepState derives Eq:
  case InFlight, Done

// All gating queries start in the same tick, so a step is either in flight or done.
case class LoadProgress(states: Map[LoadStep, LoadStepState]) derives Eq:
  def start(step: LoadStep): LoadProgress =
    LoadProgress(states.updated(step, LoadStepState.InFlight))

  def complete(step: LoadStep): LoadProgress =
    LoadProgress(states.updated(step, LoadStepState.Done))

  def steps: List[(LoadStep, LoadStepState)] =
    states.toList.sortBy(_._1.ordinal)

  def isIdle: Boolean = states.isEmpty

object LoadProgress:
  val Empty: LoadProgress = LoadProgress(Map.empty)

type LoadProgressRef[F[_]] = SignallingRef[F, LoadProgress]
