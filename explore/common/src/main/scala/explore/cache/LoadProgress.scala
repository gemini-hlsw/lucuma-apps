// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Eq
import cats.Monad
import cats.effect.Clock
import cats.syntax.all.*
import explore.utils.*
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.Logger

// Declaration order is display order, so it follows the order the queries are kicked off.
enum LoadStep(val label: String):
  case Observations          extends LoadStep("Observations")
  case Groups                extends LoadStep("Groups")
  case ProgramDetails        extends LoadStep("Program details")
  case Targets               extends LoadStep("Targets")
  case Attachments           extends LoadStep("Attachments")
  case Programs              extends LoadStep("Program list")
  case ConfigurationRequests extends LoadStep("Configuration requests")

object LoadStep:
  given Eq[LoadStep] = Eq.fromUniversalEquals

enum LoadStepState:
  case Pending, InFlight, Done

object LoadStepState:
  given Eq[LoadStepState] = Eq.fromUniversalEquals

// `expected` is declared when a load starts, so the whole list is on screen from the
// first frame instead of growing a line at a time. Not every load runs every step:
// with no program in the URL only the program list is being waited on.
case class LoadProgress(expected: Set[LoadStep], states: Map[LoadStep, LoadStepState]):
  def start(step: LoadStep): LoadProgress =
    LoadProgress(expected + step, states.updated(step, LoadStepState.InFlight))

  def complete(step: LoadStep): LoadProgress =
    LoadProgress(expected + step, states.updated(step, LoadStepState.Done))

  def stateOf(step: LoadStep): LoadStepState =
    states.getOrElse(step, LoadStepState.Pending)

  def steps: List[(LoadStep, LoadStepState)] =
    LoadStep.values.toList.filter(expected.contains).map(step => (step, stateOf(step)))

  def isIdle: Boolean = expected.isEmpty

object LoadProgress:
  val Empty: LoadProgress = LoadProgress(Set.empty, Map.empty)

  def expecting(steps: Set[LoadStep]): LoadProgress = LoadProgress(steps, Map.empty)

  given Eq[LoadProgress] = Eq.by(p => (p.expected, p.states))

type LoadProgressRef[F[_]] = SignallingRef[F, LoadProgress]

extension [F[_], A](effect: F[A])
  // Sibling of `logTime`: reports the step to the UI as well as the log, so the two
  // can't drift. A step that fails is deliberately left in flight — the load collapses
  // to a Pot.Error anyway, and a step that never ticks is the useful signal.
  def reportingProgress(
    step: LoadStep
  )(using progress: LoadProgressRef[F], C: Clock[F], M: Monad[F], L: Logger[F]): F[A] =
    progress.update(_.start(step)) >>
      effect.logTime(step.label).flatTap(_ => progress.update(_.complete(step)))
