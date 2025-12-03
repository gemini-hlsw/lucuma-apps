// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
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

  override def getInstrumentPorts: F[InstrumentPorts] =
    InstrumentPorts(
      flamingos2Port = 0,
      ghostPort = 0,
      gmosPort = 5,
      gnirsPort = 3,
      gpiPort = 0,
      gsaoiPort = 0,
      igrins2Port = 0,
      nifsPort = 1,
      niriPort = 0
    ).pure[F]

}

object TcsNorthControllerSim {
  def build[F[_]: Async]: F[TcsNorthControllerSim[F]] = for {
    x <- Ref.of(
           GuideState(
             MountGuideOption.MountGuideOff,
             M1GuideConfig.M1GuideOff,
             M2GuideConfig.M2GuideOff,
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
