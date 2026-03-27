// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import monocle.Focus
import monocle.Lens
import observe.server.StepGen

final case class LoadedStep[F[_]](
  stepGen:         StepGen[F],
  executionZipper: ExecutionZipper[F]
):
  export stepGen.{atomId, generator, obsControl, resources}
  export executionZipper.{done, focus, id, pending}

  lazy val withNextExecution: Option[LoadedStep[F]] =
    executionZipper.withNextExecution.map(es => copy(executionZipper = es))

  def rollback: LoadedStep[F] = copy(executionZipper = executionZipper.rollback)

  def mark(i: Int)(r: Result): LoadedStep[F] =
    copy(executionZipper = ExecutionZipper.current[F].modify(_.mark(i)(r))(executionZipper))

  def start(i: Int): LoadedStep[F] =
    copy(executionZipper = ExecutionZipper.current[F].modify(_.start(i))(executionZipper))

  def update(stepDef: List[ParallelActions[F]]): LoadedStep[F] =
    copy(executionZipper = executionZipper.update(stepDef))

object LoadedStep:
  def stepGen[F[_]]: Lens[LoadedStep[F], StepGen[F]]                 = Focus[LoadedStep[F]](_.stepGen)
  def executionZipper[F[_]]: Lens[LoadedStep[F], ExecutionZipper[F]] =
    Focus[LoadedStep[F]](_.executionZipper)
