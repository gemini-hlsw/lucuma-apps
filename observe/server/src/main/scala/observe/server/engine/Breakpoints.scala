// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step

import scala.collection.immutable.HashSet

opaque type Breakpoints = Set[Step.Id]

opaque type BreakpointsDelta = Set[(Step.Id, Breakpoint)]

object Breakpoints:
  val empty: Breakpoints = HashSet.empty

  def apply(steps: Set[Step.Id]): Breakpoints = steps

  def fromStepsWithBreakpoints[F[_]](
    stepsWithBreakpoints: List[(EngineStep[F], Breakpoint)]
  ): Breakpoints =
    stepsWithBreakpoints
      .collect:
        case (s, b) if b === Breakpoint.Enabled => s.id
      .toSet

  private def fromAtom[D](atom: Atom[D]): Breakpoints =
    atom.steps
      .collect:
        case s if s.breakpoint === Breakpoint.Enabled => s.id
      .toSet

  private def fromExecutionSequence[D](seq: ExecutionSequence[D]): Breakpoints =
    fromAtom(seq.nextAtom) ++ seq.possibleFuture.toSet.flatMap(atom => fromAtom(atom))

  private def stepIdsOf[D](seq: ExecutionSequence[D]): Set[Step.Id] =
    (seq.nextAtom :: seq.possibleFuture).flatMap(_.steps.toList.map(_.id)).toSet

  def fromExecutionConfig[S, D](executionConfig: ExecutionConfig[S, D]): Breakpoints =
    executionConfig.acquisition.foldMap(fromExecutionSequence) ++
      executionConfig.science.foldMap(fromExecutionSequence)

  // Applies `f` to the instrument's execution config; `default` for instruments without one.
  private def onExecutionConfig[A](config: InstrumentExecutionConfig, default: => A)(
    f: [S, D] => ExecutionConfig[S, D] => A
  ): A =
    config match
      case InstrumentExecutionConfig.GmosNorth(ec)  => f(ec)
      case InstrumentExecutionConfig.GmosSouth(ec)  => f(ec)
      case InstrumentExecutionConfig.Flamingos2(ec) => f(ec)
      case InstrumentExecutionConfig.Igrins2(ec)    => f(ec)
      case InstrumentExecutionConfig.Ghost(ec)      => f(ec)
      case InstrumentExecutionConfig.Gnirs(ec)      => f(ec)
      case InstrumentExecutionConfig.Visitor(_)     => default
      case InstrumentExecutionConfig.Exchange       => default

  /** The breakpoints of both sequences of `config`, as on load. */
  def fromInstrumentExecutionConfig(config: InstrumentExecutionConfig): Breakpoints =
    onExecutionConfig(config, empty)([S, D] =>
      (ec: ExecutionConfig[S, D]) => fromExecutionConfig(ec)
    )

  /**
   * The breakpoints after the acquisition has been regenerated in the ODB: the acquisition's are
   * taken from `config` (its steps have new ids), while the science ones are kept as they are,
   * since they may have been toggled in Observe, which the ODB never learns about. Ids no longer in
   * `config` (the discarded acquisition's) are dropped.
   */
  def withAcquisitionFrom(current: Breakpoints, config: InstrumentExecutionConfig): Breakpoints =
    onExecutionConfig(config, current): [S, D] =>
      (ec: ExecutionConfig[S, D]) =>
        current.filter(ec.science.foldMap(stepIdsOf).contains) ++
          ec.acquisition.foldMap(fromExecutionSequence)

  extension (breakpoints: Breakpoints)
    def value: Set[Step.Id]                                    = breakpoints
    def ++(other:        Breakpoints): Breakpoints             = breakpoints ++ other
    def contains(stepId: Step.Id): Boolean                     = breakpoints.contains_(stepId)
    def +(stepId:        Step.Id): Breakpoints                 = breakpoints + stepId
    def -(stepId:        Step.Id): Breakpoints                 = breakpoints - stepId
    def merge(breakpointsDelta: BreakpointsDelta): Breakpoints =
      breakpoints ++ breakpointsDelta.collect:
        case (stepId, Breakpoint.Enabled) => stepId

object BreakpointsDelta:
  def apply(breakpointsDelta: Set[(Step.Id, Breakpoint)]): BreakpointsDelta =
    breakpointsDelta
  def one(stepId: Step.Id, breakpoint: Breakpoint): BreakpointsDelta        =
    Set((stepId, breakpoint))
  def fromStepsWithBreakpoints[F[_]](
    stepsWithBreakpoints: List[(EngineStep[F], Breakpoint)]
  ): BreakpointsDelta                                                       =
    stepsWithBreakpoints.map((s, b) => (s.id, b)).toSet
