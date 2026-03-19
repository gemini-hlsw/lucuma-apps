// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.Step

import scala.collection.immutable.HashSet

opaque type Breakpoints = Set[Step.Id]

opaque type BreakpointsDelta = Set[(Step.Id, Breakpoint)]

object Breakpoints:
  val empty: Breakpoints                                                             = HashSet.empty
  def apply(steps: Set[Step.Id]): Breakpoints                                        = steps
  def fromStepsWithBreakpoints[F[_]](
    stepsWithBreakpoints: List[(EngineStep[F], Breakpoint)]
  ): Breakpoints =
    stepsWithBreakpoints
      .collect:
        case (s, b) if b === Breakpoint.Enabled => s.id
      .toSet
  private def fromAtom[D](atom: Atom[D]): Breakpoints                                =
    atom.steps
      .collect:
        case s if s.breakpoint === Breakpoint.Enabled => s.id
      .toSet
  private def fromExecutionSequence[D](seq: ExecutionSequence[D]): Breakpoints       =
    fromAtom(seq.nextAtom) ++ seq.possibleFuture.toSet.flatMap(atom => fromAtom(atom))
  def fromExecutionConfig[S, D](executionConfig: ExecutionConfig[S, D]): Breakpoints =
    executionConfig.acquisition.foldMap(fromExecutionSequence) ++
      executionConfig.science.foldMap(fromExecutionSequence)

  extension (breakpoints: Breakpoints)
    def value: Set[Step.Id]                                    = breakpoints
    def contains(stepId: Step.Id): Boolean                     = breakpoints.contains_(stepId)
    def +(stepId:        Step.Id): Breakpoints                 = breakpoints + stepId
    def -(stepId:        Step.Id): Breakpoints                 = breakpoints - stepId
    def merge(breakpointsDelta: BreakpointsDelta): Breakpoints =
      breakpoints ++ breakpointsDelta.collect:
        case (stepId, Breakpoint.Enabled) => stepId

object BreakpointsDelta:
  def apply(breakpointsDelta: Set[(Step.Id, Breakpoint)]): BreakpointsDelta =
    breakpointsDelta
  def fromStepsWithBreakpoints[F[_]](
    stepsWithBreakpoints: List[(EngineStep[F], Breakpoint)]
  ): BreakpointsDelta =
    stepsWithBreakpoints.map((s, b) => (s.id, b)).toSet
