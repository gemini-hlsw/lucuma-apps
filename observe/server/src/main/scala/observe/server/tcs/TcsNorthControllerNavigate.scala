// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.*
import cats.effect.Async
import clue.FetchClient
import lucuma.core.enums.Site
import lucuma.schemas.NavigateDB
import observe.model.enums.NodAndShuffleStage
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsNorthController.*
import org.typelevel.log4cats.Logger

final case class TcsNorthControllerNavigate[F[_]: {Async, Logger}](epicsSys: TcsEpics[F])(using
  FetchClient[F, NavigateDB]
) extends TcsNorthController[F] {
  private val commonController = TcsControllerNavigate[F, Site.GN.type](epicsSys)

  override def applyConfig(
    subsystems: NonEmptySet[Subsystem],
    tcs:        TcsNorthConfig
  ): F[Unit] = commonController.applyConfig(subsystems, tcs)

  override def notifyObserveStart: F[Unit] = commonController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = commonController.notifyObserveEnd

  override def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsNorthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit] =
    commonController.nod(subsystems, offset, guided, tcsConfig)
}
