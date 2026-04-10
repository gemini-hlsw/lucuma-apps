// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptyList
import cats.syntax.all.*
import mouse.all.*
import observe.model.Subsystem
import observe.model.SystemOverrides
import observe.server.engine.Action
import observe.server.engine.ActionIndex
import observe.server.engine.ExecutionIndex
import observe.server.engine.ParallelActions

case class StepActionsGen[F[_]](
  preStep:     Action[F],
  preConfig:   Action[F],
  configs:     Map[Subsystem, SystemOverrides => Action[F]],
  postConfig:  Action[F],
  preObserve:  Action[F],
  post:        (HeaderExtraData, SystemOverrides) => List[ParallelActions[F]],
  postObserve: Action[F],
  postStep:    Action[F]
):
  def generate(ctx: HeaderExtraData, overrides: SystemOverrides): List[ParallelActions[F]] =
    List(
      NonEmptyList.one(preStep),
      NonEmptyList.one(preConfig)
    ) ++
      NonEmptyList.fromList(configs.values.toList.map(_(overrides))).toList ++
      List(
        NonEmptyList.one(postConfig),
        NonEmptyList.one(preObserve)
      ) ++
      post(ctx, overrides) ++
      List(
        NonEmptyList.one(postObserve),
        NonEmptyList.one(postStep)
      )

  val ConfigsExecutionIndex: Int                                             = 2
  def configActionCoord(r: Subsystem): Option[(ExecutionIndex, ActionIndex)] =
    val i = configs.keys.toIndexedSeq.indexOf(r)
    (i >= 0)
      .option(i)
      .map(i => (ExecutionIndex(ConfigsExecutionIndex), ActionIndex(i.toLong)))

  def resourceAtCoords(ex: ExecutionIndex, ac: ActionIndex): Option[Subsystem] =
    if (ex.value === ConfigsExecutionIndex) configs.keys.toList.get(ac.value)
    else None
