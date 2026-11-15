// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import crystal.Pot
import explore.model.ProgramInfoList
import explore.model.syntax.all.*
import explore.services.OdbProgramApi
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps

// The list of programs visible to the user. Kept apart from `ProgramSummaries` because it doesn't
// depend on the selected program, so switching programs neither reloads nor throttles it.
case class ProgramListCacheController(
  modPrograms:              (Pot[ProgramInfoList] => Pot[ProgramInfoList]) => IO[Unit],
  override val resetSignal: Stream[IO, ResetType]
)(using val odbApi: OdbProgramApi[IO])
    extends ReactFnProps[ProgramListCacheController](ProgramListCacheController.component)
    with CacheControllerComponent.Props[ProgramInfoList]:
  val modState         = modPrograms
  val onLoad: IO[Unit] = IO.unit

object ProgramListCacheController
    extends CacheControllerComponent[ProgramInfoList, ProgramListCacheController]:

  override protected val initial: ProgramListCacheController => IO[
    (ProgramInfoList, Stream[IO, ProgramInfoList => ProgramInfoList])
  ] =
    props =>
      props.odbApi.allPrograms
        .map(_.toSortedMap(_.id))
        .tupleRight(Stream.empty)

  override protected val updateStream
    : ProgramListCacheController => Resource[IO, Stream[IO, ProgramInfoList => ProgramInfoList]] =
    props =>
      props.odbApi.programDeltaSubscription
        .map(_.map(programInfo => _.updated(programInfo.id, programInfo)))
