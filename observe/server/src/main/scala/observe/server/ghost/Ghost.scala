// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.data.Kleisli
import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepType as OcsStepType
import lucuma.core.math.*
import lucuma.core.model.*
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Scalars.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.CurrentConditions
import observe.model.SystemOverrides
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.*
import observe.server.InstrumentSystem.*
import observe.server.keywords.GdsClient
import observe.server.keywords.GdsInstrument
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit

final case class Ghost[F[_]: {Logger, Async}](
  controller: GhostController[F],
  conditions: Ref[F, CurrentConditions],
  config:     GhostConfig
) extends GdsInstrument[F]
    with InstrumentSystem[F]
    with GhostLUT { self =>

  val readOutTimeExtra: TimeSpan = TimeSpan.unsafeFromDuration(420, ChronoUnit.SECONDS)

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Ghost

  override val contributorName: String = "ghost"

  override def observeControl: InstrumentSystem.ObserveControl[F] =
    Uncontrollable

  override def observe: Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      controller
        .observe(fileId, calcObserveTime)
        .as(ObserveCommandResult.Success: ObserveCommandResult)
    }

  override def configure: F[ConfigResult[F]] =
    conditions.get.flatMap(controller.applyConfig(config, _)).as(ConfigResult[F](self))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Async[F].unit

  override def calcObserveTime: TimeSpan =
    if (config.isBias) {
      val coAdds = config.coAdds.getOrElse(PosInt.unsafeFrom(1))
      totalObserveTime(GhostDetector.Blue(config.blueConfig.value.copy(exposureCount = coAdds)),
                       GhostDetector.Red(config.redConfig.value.copy(exposureCount = coAdds))
      ) +| readOutTimeExtra
    } else
      totalObserveTime(config.blueConfig, config.redConfig) +| readOutTimeExtra

  override def observeProgress(
    total:   TimeSpan,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    ProgressUtil.obsCountdown[F](total, elapsed.value)

  override def instrumentActions: InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

//trait GhostConfigUtil {
//  val INSTRUMENT_NAME_PROP: String = "GHOST"
//  val name: String                 = INSTRUMENT_NAME_PROP
//
//  def extractor[A: ClassTag](config: CleanConfig, propName: String): Option[A] =
//    config.extractInstAs[A](propName).toOption
//
//  def formatExtractor[A](
//    config: CleanConfig,
//    fmt:    Format[String, A]
//  ): String => Either[ExtractFailure, Option[A]] = { propName =>
//    // 1. If content is None, trivial success, so Right(None).
//    // 2. If processed content is Some(a), success, so Right(Some(content)).
//    // 3. If processed content is None, failure, so Left(error).
//    extractor[String](config, propName)
//      .map(fmt.getOption)
//      .map {
//        case None  =>
//          Left(ConversionError(INSTRUMENT_KEY / propName, s"Could not parse $propName"))
//        case other => Right(other)
//      }
//      .getOrElse(Right(None))
//  }
//
//  def raExtractorBase(config: CleanConfig) =
//    formatExtractor[RightAscension](config, RightAscension.fromStringHMS)
//
//  def decExtractorBase(config: CleanConfig) =
//    formatExtractor[Declination](config, Declination.fromStringSignedDMS)
//
//}

object Ghost {

//  def gainFromODB(n: GhostReadMode): ReadNoiseGain = n match {
//    case GhostReadMode.SLOW_LOW   => ReadNoiseGain.Slow
//    case GhostReadMode.MEDIUM_LOW => ReadNoiseGain.Medium
//    case GhostReadMode.FAST_LOW   => ReadNoiseGain.Fast
//    case GhostReadMode.FAST_HIGH  => ReadNoiseGain.Fast
//  }

  def buildConfig(
    static:            GhostStaticConfig,
    step:              Step[GhostDynamicConfig],
    targetEnvironment: TargetEnvironment,
    observingTime:     Timestamp
  ): Either[ObserveFailure, GhostConfig] =
    GhostConfig(
      static,
      step,
      targetEnvironment,
      observingTime
    )
//  {
//    val MaxTargets = 8
//
//    def userTargets: List[Option[Target]] = (for {
//      i <- 1 to MaxTargets
//    } yield {
//      val (a, _, _, _, d, e) = SPGhost.userTargetParams(i)
//      (for {
//        ra  <- raExtractor(d)
//        dec <- decExtractor(e)
//        c    = (ra, dec).mapN(Coordinates.apply)
//        n   <- config
//                 .extractInstAs[String](a)
//                 .flatMap(refineV[NonEmpty](_).leftMap(ContentError(_)))
//        // Note the coordinates are PM corrected on the OT side
//      } yield c.map(coord =>
//        Target.Sidereal(
//          n,
//          SiderealTracking(coord, Epoch.J2000, none, none, none),
//          SourceProfile.Point(
//            SpectralDefinition.BandNormalized(
//              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
//              SortedMap.empty
//            )
//          ),
//          None
//        )
//      )).toOption.flatten
//    }).toList

//    EitherT {
//      Sync[F].delay {
//        (for {
//          baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
//          baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)
//
//          fiberAgitator1 = dynamic.ifu1FiberAgitator
//            //config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_1).map(_.booleanValue())
//          fiberAgitator2 = dynamic.ifu2FiberAgitator
//            //config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_2).map(_.booleanValue())
//          srifu1Name     = extractor[String](config, SPGhost.SRIFU1_NAME)
//          srifu1RAHMS   <- raExtractor(SPGhost.SRIFU1_RA_HMS)
//          srifu1DecHDMS <- decExtractor(SPGhost.SRIFU1_DEC_DMS)
//          srifu1Type     = extractor[TargetType](config, SPGhost.SRIFU1_TYPE)
//
//          srifu2Name     = extractor[String](config, SPGhost.SRIFU2_NAME)
//          srifu2RAHMS   <- raExtractor(SPGhost.SRIFU2_RA_HMS)
//          srifu2DecHDMS <- decExtractor(SPGhost.SRIFU2_DEC_DMS)
//
//          hrifu1Name     = extractor[String](config, SPGhost.HRIFU1_NAME)
//          hrifu1RAHMS   <- raExtractor(SPGhost.HRIFU1_RA_HMS)
//          hrifu1DecHDMS <- decExtractor(SPGhost.HRIFU1_DEC_DMS)
//          hrifu1Type     = extractor[TargetType](config, SPGhost.HRIFU1_TYPE)
//
//          hrifu2RAHMS   <- raExtractor(SPGhost.HRIFU2_RA_HMS)
//          hrifu2DecHDMS <- decExtractor(SPGhost.HRIFU2_DEC_DMS)
//          obsClass      <- config.extractObsAs[String](OBS_CLASS_PROP)
//          obsType       <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
//          isScience      = obsType === SCIENCE_OBSERVE_TYPE
//          coAdds         = config.extractObsAs[JInt](COADDS_PROP).map(_.intValue())
//
//          blueBinning  <- config.extractInstAs[GhostBinning](SPGhost.BLUE_BINNING_PROP)
//          redBinning   <- config.extractInstAs[GhostBinning](SPGhost.RED_BINNING_PROP)
//          blueExposure = dynamic.blueCamera.value.binning
//            //config.extractObsAs[JDouble](SPGhost.BLUE_EXPOSURE_TIME_PROP).map(_.doubleValue())
//          redExposure  = dynamic.redCamera.value.binning
//            //config.extractObsAs[JDouble](SPGhost.RED_EXPOSURE_TIME_PROP).map(_.doubleValue())
//          blueCount    = dynamic.blueCamera.value.exposureCount // config.extractObsAs[JInt](SPGhost.BLUE_EXPOSURE_COUNT_PROP).map(_.intValue())
//          redCount     = dynamic.redCamera.value.exposureCount // config.extractObsAs[JInt](SPGhost.RED_EXPOSURE_COUNT_PROP).map(_.intValue())
//          blueReadMode = dynamic.blueCamera.value.readMode
////            config
////              .extractInstAs[GhostReadMode](SPGhost.BLUE_READ_NOISE_GAIN_PROP)
//          redReadMode  = dynamic.redCamera.value.readMode
////            config
////              .extractInstAs[GhostReadMode](SPGhost.RED_READ_NOISE_GAIN_PROP)
//          rm            = static.resolutionMode
////            config
////              .extractInstAs[GhostResolutionMode](SPGhost.RESOLUTION_MODE)
//          vMag         <-
//            config
//              .extractInstAs[JDouble](SPGhost.MAG_V_PROP)
//              .map(_.doubleValue().some)
//              .recoverWith(_ => none.asRight)
//
//          gMag                   <-
//            config
//              .extractInstAs[JDouble](SPGhost.MAG_G_PROP)
//              .map(_.doubleValue().some)
//              .recoverWith(_ => none.asRight)
//          guideCameraExposureTime =
//            config
//              .extractInstAs[JDouble](SPGhost.GUIDE_CAMERA_EXPOSURE_TIME_PROP)
//              .map(_.doubleValue)
//          svExposureTime          =
//            config
//              .extractInstAs[JDouble](SPGhost.SLIT_VIEWING_CAMERA_EXPOSURE_TIME_PROP)
//              .map(_.doubleValue)
//              if (step.stepConfig.isScience) {
//                GhostConfig.apply(
//                  staticConfig = static,
//                  step = step,
//                  targetEnvironment = targetEnvironment,
//                  observingTime = observingTime
//                )
//              } else {
//                val isHR = static.resolutionMode === GhostResolutionMode.High
//                GhostCalibration(
//
//                  obsType = obsType,
//                  obsClass = obsClass,
//                  blueConfig = tag[GhostDetector.Blue][ChannelConfig](
//                    ChannelConfig(blueBinning,
//                                  blueExposure.second,
//                                  blueCount,
//                                  gainFromODB(blueReadMode)
//                    )
//                  ),
//                  redConfig = tag[GhostDetector.Red][ChannelConfig](
//                    ChannelConfig(redBinning,
//                                  redExposure.second,
//                                  redCount,
//                                  gainFromODB(redReadMode)
//                    )
//                  ),
//                  baseCoords = (baseRAHMS, baseDecDMS).mapN(Coordinates.apply),
//                  fiberAgitator1 = FiberAgitator.fromBoolean(fiberAgitator1.getOrElse(false)),
//                  fiberAgitator2 = FiberAgitator.fromBoolean(fiberAgitator2.getOrElse(false)),
//                  rm.toOption,
//                  conditions,
//                  coAdds.toOption,
//                  isHR
//                ).asRight
//              }
//            }
//          }
//        } yield config).leftMap { e =>
//          ObserveFailure.Unexpected(ConfigUtilOps.explain(e))
//        }
//      }
//    }.widenRethrowT
//  }

  object specifics extends InstrumentStaticInfo {
    override val instrument: Instrument = Instrument.Ghost
  }

  def instrumentStepBuilder[F[_]: {Async, Logger}](
    conditionsRef: Ref[F, CurrentConditions]
  ): F[InstrumentStepBuilder[F, GhostStaticConfig, GhostDynamicConfig]] =
    new InstrumentStepBuilder[F, GhostStaticConfig, GhostDynamicConfig] {
      override def build(
        systems:           Systems.OverriddenSystems[F],
        stepType:          OcsStepType,
        targetEnvironment: TargetEnvironment,
        staticConf:        GhostStaticConfig,
        step:              Step[GhostDynamicConfig],
        observingTime:     Timestamp
      ): Either[ObserveFailure, InstrumentStep[F]] = for {
        stType <-
          SeqTranslate.calcStepType(specifics.instrument, step.stepConfig, step.observeClass)
        cfg    <- buildConfig(staticConf, step, targetEnvironment, observingTime)
      } yield new InstrumentStep[F] {
        override def stepType: StepKind = stType

        override def sfName: LightSinkName = LightSinkName.Ghost

        override def defocusB: Option[Length] = cfg.defocusOffset

        override def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F] =
          Ghost(systems.ghost(sysOverrides), conditionsRef, cfg)

        override def instrumentHeader(client: KeywordsClient[F]): Header[F] = GhostHeader.header(
          client,
          systems.systems.tcsKeywordReader,
          GhostKeywordsReader(cfg, conditionsRef),
          step
        )

        override def instrument: Instrument = specifics.instrument

        override def centralWavelength: Option[Wavelength] = none
      }
    }.pure[F]
}
