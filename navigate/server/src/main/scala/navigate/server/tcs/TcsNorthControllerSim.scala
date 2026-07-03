// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums.Instrument
import lucuma.core.enums.MountGuideOption
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import navigate.model.AcMechsState
import navigate.model.AllWfsConfiguration
import navigate.model.GuideState
import navigate.model.PwfsMechsState
import navigate.model.TelescopeState
import navigate.model.enums.AcFilter
import navigate.model.enums.AcLens
import navigate.model.enums.AcNdFilter
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter

class TcsNorthControllerSim[F[_]: Async](
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
  def build[F[_]: Async]: F[TcsNorthControllerSim[F]] = for {
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
  } yield new TcsNorthControllerSim(x, y, u, v, w, c)
}
