// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Async
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums
import lucuma.core.enums.Instrument
import navigate.model.enums.AcNdFilter

class TcsSouthControllerSim[F[_]: Async](stateRef: SignallingRef[F, TcsSimState])
    extends TcsBaseControllerSim[F](stateRef)
    with TcsSouthController[F] {

  override val acValidNdFilters: List[AcNdFilter] =
    List(AcNdFilter.Open, AcNdFilter.Nd3, AcNdFilter.Nd2, AcNdFilter.Nd1)

  override def getInstrumentPort(instrument: Instrument): F[Option[Int]] = (instrument match {
    case enums.Instrument.AcqCamSouth  => 2
    case enums.Instrument.Flamingos2   => 5
    case enums.Instrument.Ghost        => 1
    case enums.Instrument.GmosSouth    => 3
    case enums.Instrument.Gsaoi        => 0
    case enums.Instrument.Scorpio      => 0
    case enums.Instrument.VisitorSouth => 0
    case enums.Instrument.Zorro        => 2
    case _                             => 0
  }).some.filter(_ =!= 0).pure[F]

}

object TcsSouthControllerSim {
  def build[F[_]: Async]: F[TcsSouthControllerSim[F]] =
    SignallingRef.of[F, TcsSimState](TcsSimState.default).map(new TcsSouthControllerSim(_))
}
