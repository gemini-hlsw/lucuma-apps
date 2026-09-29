// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.NonEmptySet
import lucuma.core.enums.Site
import observe.model.enums.NodAndShuffleStage
import observe.server.tcs.TcsController.*

trait TcsSouthController[F[_]] {
  import TcsSouthController.*

  def applyConfig(
    subsystems: NonEmptySet[TcsController.Subsystem],
    tc:         TcsSouthConfig
  ): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(
    subsystems: NonEmptySet[Subsystem],
    tcsConfig:  TcsSouthConfig
  )(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[Unit]

}

object TcsSouthController {

  type TcsSouthConfig = TcsConfig[Site.GS.type]

}
