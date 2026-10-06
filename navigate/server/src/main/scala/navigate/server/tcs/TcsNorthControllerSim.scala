// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Async
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums.Instrument
import navigate.model.enums.AcNdFilter

class TcsNorthControllerSim[F[_]: Async](stateRef: SignallingRef[F, TcsSimState])
    extends TcsBaseControllerSim[F](stateRef)
    with TcsNorthController[F] {

  override val acValidNdFilters: List[AcNdFilter] = List(AcNdFilter.Open,
                                                         AcNdFilter.Nd100,
                                                         AcNdFilter.Nd1000,
                                                         AcNdFilter.Filt04,
                                                         AcNdFilter.Filt06,
                                                         AcNdFilter.Filt08
  )

  override def getInstrumentPort(instrument: Instrument): F[Option[Int]] = (instrument match {
    case Instrument.AcqCamNorth  => 1
    case Instrument.Alopeke      => 2
    case Instrument.GmosNorth    => 5
    case Instrument.Gnirs        => 3
    case Instrument.Gpi          => 0
    case Instrument.Igrins2      => 1
    case Instrument.MaroonX      => 0
    case Instrument.VisitorNorth => 0
    case _                       => 0
  }).some.filter(_ =!= 0).pure[F]

}

object TcsNorthControllerSim {
  def build[F[_]: Async]: F[TcsNorthControllerSim[F]] =
    SignallingRef.of[F, TcsSimState](TcsSimState.default).map(new TcsNorthControllerSim(_))
}
