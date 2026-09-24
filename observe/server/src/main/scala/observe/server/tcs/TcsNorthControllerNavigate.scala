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
import observe.server.altair.Altair
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsNorthController.*
import org.typelevel.log4cats.Logger

final case class TcsNorthControllerNavigate[F[_]: {Async, Logger}](epicsSys: TcsEpics[F])(using
  FetchClient[F, NavigateDB]
) extends TcsNorthController[F] {
  private val commonController = TcsControllerNavigate[F, Site.GN.type](epicsSys)

  override def applyConfig(
    subsystems: NonEmptySet[Subsystem],
    gaos:       Option[Altair[F]],
    tcs:        TcsNorthConfig
  ): F[Unit] =
    tcs match {
      case c: BasicTcsConfig[Site.GN.type] => commonController.applyBasicConfig(subsystems, c)
      case _: TcsNorthAoConfig             =>
        ObserveFailure
          .Execution("Altair steps are not yet supported when configuring the TCS through Navigate")
          .raiseError[F, Unit]
    }

  override def notifyObserveStart: F[Unit] = commonController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = commonController.notifyObserveEnd

  override def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsNorthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit] =
    tcsConfig match {
      case c: BasicTcsConfig[Site.GN.type] => commonController.nod(subsystems, offset, guided, c)
      case _: TcsNorthAoConfig             =>
        ObserveFailure.Execution("N&S not supported when using Altair").raiseError[F, Unit]
    }
}
