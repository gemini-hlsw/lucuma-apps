// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Ref
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Span
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.Tracer

/**
 * One root span per executing step, open from `START_STEP` until the next step has been read from
 * the ODB after `END_STEP`. The moments the sequence blocks on the ODB are recorded as child spans,
 * and every event sent for the step is parented under it, so a single trace shows what a step
 * waited for and why.
 *
 * Spans are root rather than children of the command that started the sequence, so that one trace
 * is one step and TraceQL can aggregate per step.
 */
private[odb] class StepSpans[F[_]](current: Ref[F, Map[Observation.Id, (Step.Id, Span[F])]])(using
  F: MonadCancelThrow[F],
  T: Tracer[F]
):

  def start(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    T.spanBuilder(StepSpans.StepSpan)
      .root
      .addAttributes(StepSpans.attributes(obsId, stepId.some)*)
      .build
      .startUnmanaged
      .flatMap: span =>
        current
          .modify(m => (m.updated(obsId, (stepId, span)), m.get(obsId)))
          .flatMap(_.traverse_(_._2.end))

  /** Ends the step's span; a no-op if another step has since started for the observation. */
  def end(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    current
      .modify: m =>
        m.get(obsId).filter(_._1 === stepId).fold((m, none))(s => (m - obsId, s._2.some))
      .flatMap(_.traverse_(_.end))

  def endCurrent(obsId: Observation.Id): F[Unit] =
    current.modify(m => (m - obsId, m.get(obsId))).flatMap(_.traverse_(_._2.end))

  private def context(obsId: Observation.Id): F[Option[(Step.Id, SpanContext)]] =
    current.get.map(_.get(obsId).map((stepId, span) => (stepId, span.context)))

  /** Records a blocking wait on the ODB as a child of the current step's span, if any. */
  def wait[A](name: String, obsId: Observation.Id)(fa: F[A]): F[A] =
    context(obsId).flatMap: ctx =>
      val builder = T.spanBuilder(name).addAttributes(StepSpans.attributes(obsId, ctx.map(_._1))*)
      ctx.fold(builder.root)(c => builder.withParent(c._2)).build.surround(fa)

  /** Runs `fa` scoped under the current step's span, so spans it creates become children. */
  def inStep[A](obsId: Observation.Id)(fa: F[A]): F[A] =
    context(obsId).flatMap(_.fold(fa)(c => T.childScope(c._2)(fa)))

object StepSpans:
  val StepSpan: String            = "observe-step"
  val WaitStepRecorded: String    = "odb-wait-step-recorded"
  val RecordDataset: String       = "odb-record-dataset"
  val RecordVisit: String         = "odb-record-visit"
  val Flush: String               = "odb-flush"
  val ReadExecutionConfig: String = "odb-read-execution-config"

  private def attributes(obsId: Observation.Id, stepId: Option[Step.Id]): List[Attribute[String]] =
    Attribute("observe.obs.id", obsId.show) :: stepId.toList.map(s =>
      Attribute("observe.step.id", s.show)
    )

  def apply[F[_]: {Concurrent, Tracer}]: F[StepSpans[F]] =
    Ref.of[F, Map[Observation.Id, (Step.Id, Span[F])]](Map.empty).map(new StepSpans[F](_))
