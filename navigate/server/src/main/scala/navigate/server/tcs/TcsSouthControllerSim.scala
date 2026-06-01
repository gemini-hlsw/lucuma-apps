// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums
import lucuma.core.enums.Instrument
import lucuma.core.enums.MountGuideOption
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import navigate.model.AcMechsState
import navigate.model.AllWfsConfiguration
import navigate.model.PwfsMechsState
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter

class TcsSouthControllerSim[F[_]: Async](
  guideRef:      Ref[F, GuideState],
  telStateRef:   Ref[F, TelescopeState],
  acMechRef:     Ref[F, AcMechsState],
  p1MechRef:     Ref[F, PwfsMechsState],
  p2MechRef:     Ref[F, PwfsMechsState],
  wfsConfigsRef: SignallingRef[F, AllWfsConfiguration]
) extends TcsBaseControllerSim[F](guideRef,
                                  telStateRef,
                                  acMechRef,
                                  p1MechRef,
                                  p2MechRef,
                                  wfsConfigsRef
    )
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
  def build[F[_]: Async]: F[TcsSouthControllerSim[F]] = for {
    x <- Ref.of(
           GuideState(
             MountGuideOption.MountGuideOff,
             M1GuideConfig.M1GuideOff,
             M2GuideConfig.M2GuideOff,
             none,
             false,
             false,
             false,
             false
           )
         )
    y <- Ref.of(TelescopeState.default)
    u <- Ref.of(AcMechsState(AcLens.Ac.some, AcNdFilter.Open.some, AcFilter.Neutral.some))
    v <- Ref.of(PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    w <- Ref.of(PwfsMechsState(PwfsFilter.Neutral.some, PwfsFieldStop.Open1.some))
    c <- SignallingRef.of(AllWfsConfiguration.default)
  } yield new TcsSouthControllerSim(x, y, u, v, w, c)
}
