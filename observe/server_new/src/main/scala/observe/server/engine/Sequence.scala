// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.model.Observation

/**
 * A single pending step for an observation.
 */
case class Sequence[F[_]] private (
  obsId:       Observation.Id,
  loadedStep:  Option[EngineStep[F]],
  breakpoints: Breakpoints
)

object Sequence:

  def empty[F[_]](obsId: Observation.Id): Sequence[F] =
    Sequence(obsId, none, Breakpoints.empty)

  def apply[F[_]](
    obsId:       Observation.Id,
    loadedStep:  EngineStep[F],
    breakpoints: Breakpoints
  ): Sequence[F] =
    new Sequence(obsId, loadedStep.some, breakpoints)

  def apply[F[_]](
    obsId:       Observation.Id,
    loadedStep:  Option[EngineStep[F]],
    breakpoints: Breakpoints
  ): Sequence[F] =
    new Sequence(obsId, loadedStep, breakpoints)
