// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.*
import cats.effect.Async
import cats.syntax.all.*
import clue.FetchClient
import lucuma.core.enums.Site
import observe.common.NavigateDB
import observe.model.enums.NodAndShuffleStage
import observe.server.ObserveFailure
import observe.server.gems.Gems
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsSouthController.*
import org.typelevel.log4cats.Logger

final case class TcsSouthControllerNavigate[F[_]: {Async, Logger}](epicsSys: TcsEpics[F])(using
  FetchClient[F, NavigateDB]
) extends TcsSouthController[F] {
  private val commonController = TcsControllerNavigate[F, Site.GS.type](epicsSys)

  override def applyConfig(
    subsystems: NonEmptySet[Subsystem],
    gaos:       Option[Gems[F]],
    tcs:        TcsSouthConfig
  ): F[Unit] =
    tcs match {
      case c: BasicTcsConfig[Site.GS.type] => commonController.applyBasicConfig(subsystems, c)
      case _: TcsSouthAoConfig             =>
        ObserveFailure
          .Execution("GeMS steps are not supported when configuring the TCS through Navigate")
          .raiseError[F, Unit]
    }

  override def notifyObserveStart: F[Unit] = commonController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = commonController.notifyObserveEnd

  override def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsSouthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit] =
    tcsConfig match {
      case c: BasicTcsConfig[Site.GS.type] => commonController.nod(subsystems, offset, guided, c)
      case _: TcsSouthAoConfig             =>
        ObserveFailure.Execution("N&S not supported when using GeMS").raiseError[F, Unit]
    }
}
