// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gmos

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.all.*
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosPreImaging
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosNodAndShuffle
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan
import monocle.Getter
import observe.model.SystemOverrides
import observe.server.InstrumentStaticInfo
import observe.server.InstrumentStepBuilder
import observe.server.Systems
import observe.server.gmos.GmosController.Config.DTAX
import observe.server.gmos.GmosController.Config.GratingOrder
import observe.server.gmos.GmosController.GmosConfig
import observe.server.gmos.GmosController.GmosSite
import observe.server.gmos.GmosController.GmosSite.FPU
import observe.server.gmos.GmosController.GmosSite.Filter
import observe.server.gmos.GmosController.GmosSite.Grating
import observe.server.gmos.GmosController.GmosSite.StageMode
import observe.server.keywords.DhsClientProvider
import org.typelevel.log4cats.Logger

final case class GmosNorth[F[_]: {Temporal, Logger}] private (
  c:                 GmosController[F, GmosSite.North.type],
  dhsClientProvider: DhsClientProvider[F],
  nsCmdR:            Ref[F, Option[NSObserveCommand]],
  cfg:               GmosController.GmosConfig[GmosSite.North.type]
) extends Gmos[F, GmosSite.North.type](
      c,
      dhsClientProvider,
      nsCmdR,
      cfg
    ) {
  override val resource: Instrument      = Instrument.GmosNorth
  override val dhsInstrumentName: String = "GMOS-N"
}

object GmosNorth {

  given gnParamGetters: Gmos.ParamGetters[GmosSite.North.type] =
    new Gmos.ParamGetters[GmosSite.North.type] {
      override val exposure: Getter[D, TimeSpan]                            =
        DynamicConfig.GmosNorth.exposure.asGetter
      override val filter: Getter[D, Option[Filter[GmosSite.North.type]]]   =
        DynamicConfig.GmosNorth.filter.asGetter
      override val grating: Getter[D, Option[Grating[GmosSite.North.type]]] =
        DynamicConfig.GmosNorth.gratingConfig.asGetter.map(_.map(_.grating))
      override val order: Getter[D, Option[GratingOrder]]                   =
        DynamicConfig.GmosNorth.gratingConfig.asGetter.map(_.map(_.order))
      override val wavelength: Getter[D, Option[Wavelength]]                =
        DynamicConfig.GmosNorth.gratingConfig.asGetter.map(_.map(_.wavelength))
      override val builtinFpu: Getter[D, Option[FPU[GmosSite.North.type]]]  =
        DynamicConfig.GmosNorth.fpu.asGetter.map(_.flatMap(_.builtinFpu))
      override val customFpu: Getter[D, Option[String]]                     =
        DynamicConfig.GmosNorth.fpu.asGetter.map(_.flatMap(_.customFilename.map(_.toString)))
      override val dtax: Getter[D, DTAX]                                    =
        DynamicConfig.GmosNorth.dtax.asGetter
      override val stageMode: Getter[S, StageMode[GmosSite.North.type]]     =
        StaticConfig.GmosNorth.stageMode.asGetter
      override val nodAndShuffle: Getter[S, Option[GmosNodAndShuffle]]      =
        StaticConfig.GmosNorth.nodAndShuffle.asGetter
      override val roi: Getter[D, GmosRoi]                                  =
        DynamicConfig.GmosNorth.roi.asGetter
      override val readout: Getter[D, GmosCcdMode]                          =
        DynamicConfig.GmosNorth.readout.asGetter
      override val isMosPreimaging: Getter[S, MosPreImaging]                =
        StaticConfig.GmosNorth.mosPreImaging.asGetter
      override val centralWavelength: Getter[D, Option[Wavelength]]         =
        Getter[D, Option[Wavelength]](_.centralWavelength)
    }

  object staticInfo extends InstrumentStaticInfo {
    override val instrument: Instrument = Instrument.GmosNorth
  }

  def build[F[_]: {Async, Logger}]
    : F[InstrumentStepBuilder[F, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]] =
    Gmos.instrumentStepBuilder[F, GmosSite.North.type](
      staticInfo.instrument,
      (
        syss:   Systems.OverriddenSystems[F],
        r:      Ref[F, Option[NSObserveCommand]],
        config: GmosConfig[GmosSite.North.type]
      ) =>
        (systemOverrides: SystemOverrides) =>
          new GmosNorth[F](syss.gmosNorth(systemOverrides), syss.dhs(systemOverrides), r, config)
    )

}
