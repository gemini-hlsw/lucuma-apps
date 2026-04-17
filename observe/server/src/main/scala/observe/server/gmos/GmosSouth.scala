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

final case class GmosSouth[F[_]: {Temporal, Logger}](
  c:                 GmosSouthController[F],
  dhsClientProvider: DhsClientProvider[F],
  nsCmdR:            Ref[F, Option[NSObserveCommand]],
  cfg:               GmosController.GmosConfig[GmosSite.South.type]
) extends Gmos[F, GmosSite.South.type](
      c,
      dhsClientProvider,
      nsCmdR,
      cfg
    ) {
  override val resource: Instrument      = Instrument.GmosSouth
  override val dhsInstrumentName: String = "GMOS-S"
//  override val dhsClient: DhsClient[F]   = dhsClientProvider.dhsClient(dhsInstrumentName)

}

object GmosSouth {

  given gsParamGetters: Gmos.ParamGetters[GmosSite.South.type] =
    new Gmos.ParamGetters[GmosSite.South.type] {
      override val exposure: Getter[D, TimeSpan]                            =
        DynamicConfig.GmosSouth.exposure.asGetter
      override val filter: Getter[D, Option[Filter[GmosSite.South.type]]]   =
        DynamicConfig.GmosSouth.filter.asGetter
      override val grating: Getter[D, Option[Grating[GmosSite.South.type]]] =
        DynamicConfig.GmosSouth.gratingConfig.asGetter.map(_.map(_.grating))
      override val order: Getter[D, Option[GratingOrder]]                   =
        DynamicConfig.GmosSouth.gratingConfig.asGetter.map(_.map(_.order))
      override val wavelength: Getter[D, Option[Wavelength]]                =
        DynamicConfig.GmosSouth.gratingConfig.asGetter.map(_.map(_.wavelength))
      override val builtinFpu: Getter[D, Option[FPU[GmosSite.South.type]]]  =
        DynamicConfig.GmosSouth.fpu.asGetter.map(_.flatMap(_.builtinFpu))
      override val customFpu: Getter[D, Option[String]]                     =
        DynamicConfig.GmosSouth.fpu.asGetter.map(_.flatMap(_.customFilename.map(_.toString)))
      override val dtax: Getter[D, DTAX]                                    =
        DynamicConfig.GmosSouth.dtax.asGetter
      override val stageMode: Getter[S, StageMode[GmosSite.South.type]]     =
        StaticConfig.GmosSouth.stageMode.asGetter
      override val nodAndShuffle: Getter[S, Option[GmosNodAndShuffle]]      =
        StaticConfig.GmosSouth.nodAndShuffle.asGetter
      override val roi: Getter[D, GmosRoi]                                  =
        DynamicConfig.GmosSouth.roi.asGetter
      override val readout: Getter[D, GmosCcdMode]                          =
        DynamicConfig.GmosSouth.readout.asGetter
      override val isMosPreimaging: Getter[S, MosPreImaging]                =
        StaticConfig.GmosSouth.mosPreImaging.asGetter
      override val centralWavelength: Getter[D, Option[Wavelength]]         =
        Getter[D, Option[Wavelength]](_.centralWavelength)
    }

//  def build[F[_]: {Temporal, Logger}](
//    controller:        GmosController[F, GmosSite.South.type],
//    dhsClientProvider: DhsClientProvider[F],
//    nsCmdR:            Ref[F, Option[NSObserveCommand]],
//    stepType:          StepType,
//    staticCfg:         StaticConfig.GmosSouth,
//    dynamicCfg:        DynamicConfig.GmosSouth
//  ): GmosSouth[F] = GmosSouth(
//    controller,
//    dhsClientProvider,
//    nsCmdR,
//    Gmos.buildConfig[F, GmosSite.South.type](
//      stepType,
//      staticCfg,
//      dynamicCfg
//    )
//  )
//
//  def obsKeywordsReader[F[_]: MonadThrow](
//    staticConfig:  StaticConfig.GmosSouth,
//    dynamicConfig: DynamicConfig.GmosSouth
//  )(using
//    getters:       Gmos.ParamGetters[GmosSite.South.type]
//  ): GmosObsKeywordsReader[F, GmosSite.South.type] = GmosObsKeywordsReader(staticConfig, dynamicConfig)
//
//  object specifics extends InstrumentStep[StaticConfig.GmosSouth, DynamicConfig.GmosSouth] {
//    override val instrument: Instrument = Instrument.GmosSouth
//
//    override def calcStepType(
//      stepConfig:   StepConfig,
//      staticConfig: StaticConfig.GmosSouth,
//      instConfig:   DynamicConfig.GmosSouth,
//      obsClass:     ObserveClass
//    ): Either[ObserveFailure, StepType] =
//      Gmos.calcStepType(instrument,
//                        stepConfig,
//                        staticConfig,
//                        obsClass,
//                        gsParamGetters.nodAndShuffle
//      )
//
//    override def sfName(config: DynamicConfig.GmosSouth): LightSinkName = LightSinkName.Gmos
//
//    // TODO Use different value if using electronic offsets
//    override val oiOffsetGuideThreshold: Option[Quantity[Double, Millimeter]] =
//      (0.01.withUnit[ArcSecond] :\ FOCAL_PLANE_SCALE).some
//  }
  object staticInfo extends InstrumentStaticInfo {
    //    override def sfName: LightSinkName = LightSinkName.Gmos
    //
    //    // TODO Use different value if using electronic offsets
    //    override val oiOffsetGuideThreshold: Option[Quantity[Double, Millimeter]] =
    //      (0.01.withUnit[ArcSecond] :\ FOCAL_PLANE_SCALE).some
    override val instrument: Instrument = Instrument.GmosSouth
  }

  def build[F[_]: {Async, Logger}]
    : F[InstrumentStepBuilder[F, StaticConfig.GmosSouth, DynamicConfig.GmosSouth]] =
    Gmos.instrumentStepBuilder[F, GmosSite.South.type](
      staticInfo.instrument,
      (
        syss:   Systems.OverriddenSystems[F],
        r:      Ref[F, Option[NSObserveCommand]],
        config: GmosController.GmosConfig[GmosSite.South.type]
      ) =>
        (systemOverrides: SystemOverrides) =>
          new GmosSouth[F](syss.gmosSouth(systemOverrides), syss.dhs(systemOverrides), r, config)
    )

}
