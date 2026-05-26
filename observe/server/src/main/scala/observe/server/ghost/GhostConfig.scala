// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import giapi.client.GiapiConfig
import giapi.client.commands.Configuration
import giapi.client.syntax.all.*
import giapi.enums.GiapiStatusApply
import giapi.enums.GiapiStatusApply.*
import lucuma.core
import lucuma.core.enums.Band
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target as GemTarget
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Scalars.TargetId
import lucuma.schemas.ObservationDB.Scalars.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.CurrentConditions
import observe.server.Length
import observe.server.ObserveFailure
import observe.server.ghost.given

import scala.annotation.unused

import GhostConfig.*

// GHOST has a number of different possible configuration modes: we add types for them here.
sealed trait GhostConfig extends GhostLUT {
  def obsType: StepConfig
  def obsClass: ObserveClass
  def blueConfig: GhostDetector.Blue
  def redConfig: GhostDetector.Red

  def baseCoords: Option[Coordinates]
  def userTargets: List[GemTarget]

  def fiberAgitator1: GhostIfu1FiberAgitator
  def fiberAgitator2: GhostIfu2FiberAgitator
  def ifu1TargetType: IFUTargetType
  def ifu2TargetType: IFUTargetType
  def ifu1BundleType: BundleConfig
  def ifu1Coordinates: Option[Coordinates]
  def ifu2Coordinates: Option[Coordinates]
  def ifu2BundleType: Option[BundleConfig]
  def resolutionMode: GhostResolutionMode
  def scienceMagnitude: Option[Double]
  def coAdds: Option[PosInt] = None
  def guideCameraOverride: Option[TimeSpan]
  def svCameraOverride: Option[TimeSpan]

  def baseConfiguration: Configuration = Configuration.Zero

  def slitMaskConfiguration: Configuration

  def targetConfig(t: GemTarget, i: Int): Configuration =
    // Note the base coordinates are already PM corrected in the OT
    t match {
      case GemTarget.Sidereal(_, SiderealTracking(baseCoordinates, _, _, _, _), _, _) =>
        GhostConfig.UserTargetsApply
          .get(i + 1)
          .map { case (name, ra, dec) =>
            GhostConfig.giapiConfig(name, s""""${t.name.value}"""") |+|
              GhostConfig.giapiConfig(ra, baseCoordinates.ra.toAngle.toDoubleDegrees) |+|
              GhostConfig.giapiConfig(dec, baseCoordinates.dec.toAngle.toSignedDoubleDegrees)
          }
          .combineAll
      case _                                                                          =>
        Configuration.Zero
    }

  def userTargetsConfig: Configuration =
    userTargets.zipWithIndex.map(Function.tupled(targetConfig)).combineAll |+|
      giapiConfig(GiapiStatusApply.GhostUserTargetCount, userTargets.length)

  def ifuNone(ifuNum: IFUNum): Configuration =
    giapiConfig(ifuNum.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
      giapiConfig(ifuNum.demandItem, DemandType.DemandNone: DemandType)

  def isDayCal: Boolean = obsClass match {
    case ObserveClass.DayCal => true
    case _                   => false
  }

  def ifuCalibration: Configuration =
    if (isDayCal) {
      giapiConfig(GhostIFU1Target, IFUTargetType.TargetXY: IFUTargetType) |+|
        giapiConfig(GhostIFU2Target, IFUTargetType.TargetXY: IFUTargetType) |+|
        giapiConfig(GhostIFU1Type, DemandType.DemandXY: DemandType) |+|
        giapiConfig(GhostIFU2Type, DemandType.DemandXY: DemandType) |+|
        giapiConfig(GhostIFU1MoveMode, "IFU_ABSOLUTE") |+|
        giapiConfig(GhostIFU2MoveMode, "IFU_ABSOLUTE") |+|
        giapiConfig(GhostIFU1X, 55.0) |+|
        giapiConfig(GhostIFU1Y, 0.0) |+|
        giapiConfig(GhostIFU2X, -55.0) |+|
        giapiConfig(GhostIFU2Y, 0.0)
    } else Configuration.Zero

  def ifu1Config: Configuration =
    ifu1Coordinates
      .map(GhostConfig.ifuConfig(IFUNum.IFU1, ifu1TargetType, _, ifu1BundleType))
      .orEmpty

  def isBias: Boolean = obsType === StepConfig.Bias
  def isFlat: Boolean = obsType.isFlat
  def isArc: Boolean  = obsType.isArc
  def isDark: Boolean = obsType === StepConfig.Dark

  def ifu2Configuration: Configuration

  def adcConfiguration: Configuration

  final def ifu2Config: Configuration =
    ifu2Configuration

  def channelConfig: Configuration =
    giapiConfig(GhostBlueBinningRcf, blueConfig.value.binning.spectralBinning) |+|
      giapiConfig(GhostBlueBinningCcf, blueConfig.value.binning.spectralBinning) |+|
      giapiConfig(GhostBlueDuration, blueConfig.value.exposureTime.toMilliseconds.intValue) |+|
      giapiConfig(GhostBlueUnit, 0.001) |+|
      giapiConfig(GhostBlueRequestType, "HARDWARE") |+|
      giapiConfig(GhostBlueExposureCount, calcBlueCount(obsType, coAdds, blueConfig).value) |+|
      giapiConfig(GhostBlueImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostBlueDoDisplay, 1) |+|
      giapiConfig(GhostBlueDoFlush, 1) |+|
      giapiConfig(GhostBlueDoContinuous, 0) |+|
      giapiConfig(GhostBlueDoReadout, 1) |+|
      giapiConfig(GhostBlueDoSave, 1) |+|
      giapiConfig(GhostBlueReadMode, blueConfig.value.readMode) |+|
      giapiConfig(GhostBlueCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostRedBinningRcf, redConfig.value.binning.spectralBinning) |+|
      giapiConfig(GhostRedBinningCcf, redConfig.value.binning.spectralBinning) |+|
      giapiConfig(GhostRedDuration, redConfig.value.exposureTime.toMilliseconds.intValue) |+|
      giapiConfig(GhostRedUnit, 0.001) |+|
      giapiConfig(GhostRedRequestType, "HARDWARE") |+|
      giapiConfig(GhostRedExposureCount, calcRedCount(obsType, coAdds, redConfig).value) |+|
      giapiConfig(GhostRedImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostRedDoDisplay, 1) |+|
      giapiConfig(GhostRedDoFlush, 1) |+|
      giapiConfig(GhostRedDoContinuous, 0) |+|
      giapiConfig(GhostRedDoReadout, 1) |+|
      giapiConfig(GhostRedDoSave, 1) |+|
      giapiConfig(GhostRedReadMode, redConfig.value.readMode) |+|
      giapiConfig(GhostRedCcdRequestType, "CCD_CAMERA_SET")

  def baseSVConfig: Configuration =
    giapiConfig(GhostSVCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostSVRequestType, "HARDWARE") |+|
      giapiConfig(GhostSVRunNumber, 0) |+|
      giapiConfig(GhostSVDoSave, 1) |+|
      giapiConfig(GhostSVDoDisplay, 1) |+|
      giapiConfig(GhostSVNRegions, 1) |+|
      giapiConfig(GhostSVRcf, 2) |+|
      giapiConfig(GhostSVCcf, 2) |+|
      giapiConfig(GhostSVImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostSVXO, 780) |+|
      giapiConfig(GhostSVYO, 650) |+|
      giapiConfig(GhostSVWidth, 300) |+|
      giapiConfig(GhostSVHeigth, 260) |+|
      giapiConfig(GhostSVIFU1BlueThreshold, 0) |+|
      giapiConfig(GhostSVIFU1BlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU1RedThreshold, 0) |+|
      giapiConfig(GhostSVIFU1RedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU2BlueThreshold, 0) |+|
      giapiConfig(GhostSVIFU2BlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU2RedThreshold, 0) |+|
      giapiConfig(GhostSVIFU2RedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVHIBlueThreshold, 0) |+|
      giapiConfig(GhostSVHIBlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVHIRedThreshold, 0) |+|
      giapiConfig(GhostSVHIRedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVZeroAccumulatedFlux, 1) |+|
      giapiConfig(GhostSVDoContinuous, 0)

  def baseAGConfig: Configuration =
    giapiConfig(GhostAGCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostAGRequestType, "HARDWARE") |+|
      giapiConfig(GhostAGRepeat, 1) |+|
      giapiConfig(GhostAGDoSave, 0) |+|
      giapiConfig(GhostAGDoDisplay, 1) |+|
      giapiConfig(GhostAGRcf, 2) |+|
      giapiConfig(GhostAGCcf, 2) |+|
      giapiConfig(GhostAGXO, 400) |+|
      giapiConfig(GhostAGYO, 300) |+|
      giapiConfig(GhostAGWidth, 128) |+|
      giapiConfig(GhostAGHeigth, 162) |+|
      giapiConfig(GhostAGBackground, 0) |+|
      giapiConfig(GhostAGSimulateFlux, 0) |+|
      giapiConfig(GhostAGDoContinuous, 1)

  val SVDurationFactor = 1000

  def svCalib: Configuration =
    baseSVConfig |+|
      giapiConfig(GhostSVDuration, svCalibExposureTime(obsType).toMilliseconds.toInt) |+|
      giapiConfig(GhostSVRepeat, svCalibSVRepeats(obsType, blueConfig, redConfig, coAdds)) |+|
      giapiConfig(GhostSVUnit, 1.0 / SVDurationFactor)

  def svConfiguration(
    timeOverride: Option[TimeSpan],
    mag:          Option[Double],
    conditions:   CurrentConditions
  ): Configuration = {
    val time    = timeOverride.getOrElse(svCameraTime(conditions, mag))
    val repeats = timeOverride
      .map(t => svOverrideCameraRepeats(t, blueConfig, redConfig))
      .getOrElse(svCameraRepeats(conditions, mag, this.blueConfig, this.redConfig))
    baseSVConfig |+|
      giapiConfig(GhostSVDuration, (time *| SVDurationFactor).toSeconds.toInt) |+|
      giapiConfig(GhostSVRepeat, repeats) |+|
      giapiConfig(GhostSVUnit, 1.0 / SVDurationFactor)
  }

  val AGDurationFactor = 10

  // Unused we are not setting AG unless we do an override
  def agConfiguration(mag: Option[Double], conditions: CurrentConditions): Configuration =
    baseAGConfig |+|
      giapiConfig(GhostAGDuration,
                  (agCameraTime(conditions, mag) *| AGDurationFactor).toSeconds.toInt
      ) |+|
      giapiConfig(GhostAGUnit, 1.0 / AGDurationFactor)

  // Unused, sets the ag exposure time override
  def agOverride: Configuration =
    guideCameraOverride.foldMap(f =>
      giapiConfig(GhostAGDuration, (f *| AGDurationFactor).toSeconds.toInt) |+|
        giapiConfig(GhostAGUnit, 1.0 / AGDurationFactor)
    )

  // TODO: Implement PRV mode when it is added in GPP
  def prvMode: Configuration = giapiConfig(GhostThXeLamp, 0)
//    if (isScience(obsType) && resolutionMode === Some(GhostResolutionMode.GhostPRV)) {
//      val blue   = blueConfig.value.exposureTime.toSeconds
//      val red    = redConfig.value.exposureTime.toSeconds
//      val result = if (blue > red) {
//        CameraFilterWheelLUT
//          .findLast(_.red.toSeconds <= red)
//          .map(_.pos)
//          .getOrElse(CameraFilterWheelLUT.headOption.foldMap(_.pos))
//      } else {
//        CameraFilterWheelLUT
//          .findLast(_.blue.toSeconds <= blue)
//          .map(_.pos)
//          .getOrElse(CameraFilterWheelLUT.headOption.foldMap(_.pos))
//      }
//
//      giapiConfig(GhostThXeLamp, 1) |+|
//        giapiConfig(GhostCalibrationFilterDemand, "CFW_DEMAND_POSITION") |+|
//        giapiConfig(GhostCalibrationFilterWheel, result)
//    } else {
//      giapiConfig(GhostThXeLamp, 0)
//    }

  // Move both IFUs to focus position. not desirable if guide
  def moveIFUToFocus: Configuration =
    giapiConfig(GhostBFocusType, "FOCUS_DEMAND_MOVETO_FOCUS_POSITION") |+|
      giapiConfig(GhostRFocusType, "FOCUS_DEMAND_MOVETO_FOCUS_POSITION")

  def configuration(conditions: CurrentConditions): Configuration =
    baseConfiguration |+| slitMaskConfiguration |+| (
      if (!isScience(obsType)) {
        // as per REL-4855 we move ifus to focus for calibrations
        ifuCalibration |+| channelConfig |+| moveIFUToFocus
        svCalib |+|
          GhostConfig.fiberConfig1(FiberAgitator.fromIfu1FiberAgitator(fiberAgitator1)) |+|
          GhostConfig.fiberConfig2(FiberAgitator.fromIfu2FiberAgitator(fiberAgitator2))
      } else
        ifu1Config |+| ifu2Config |+|
          GhostConfig.fiberConfig1(FiberAgitator.None) |+|
          GhostConfig.fiberConfig2(FiberAgitator.None)
          |+|
          userTargetsConfig |+| channelConfig |+| adcConfiguration |+|
          // agOverride |+|
          svConfiguration(svCameraOverride, scienceMagnitude, conditions) |+| prvMode
    ) |+| giapiConfig(GhostSlitMaskPositionerType, "SMP_DEMAND_POSITION")

  def defocusOffset: Option[Length] = {
    def defocusAmount(r: Double): Length = Length.fromDoubleMicrometers(4.85 * r * r + 0.067 * r)

    (ifu1TargetType, ifu2TargetType, baseCoords) match {
      // Dual target
      case (IFUTargetType.SiderealTarget(_), IFUTargetType.SiderealTarget(_), Some(baseCoords)) =>
        ifu1Coordinates.map { ifu1Coords =>
          // if not linked get the average distance of each target to the base position
          val r1 = ifu1Coords.angularDistance(baseCoords).toDoubleDegrees * 60
          val r2 = ifu2Coordinates.foldMap(_.angularDistance(baseCoords).toDoubleDegrees * 60)
          val r  = (r1 + r2) / 2
          defocusAmount(r)
        }
      case (IFUTargetType.SiderealTarget(_), IFUTargetType.SiderealTarget(_), None)             =>
        // If linked calculate the base the two targets
        ifu1Coordinates.map { ifu1Coords =>
          val base = ifu2Coordinates.map(a => a.interpolate(ifu1Coords, 0.5))
          val u    = base.foldMap(_.angularDistance(ifu1Coords).toDoubleDegrees * 60)
          defocusAmount(u)
        }
      case (IFUTargetType.SiderealTarget(_), _, _) | (IFUTargetType.NonsiderealTarget(_), _, _) =>
        ifu1Coordinates.map { ifu1Coords =>
          val r = baseCoords.foldMap(ifu1Coords.angularDistance(_).toDoubleDegrees * 60)
          defocusAmount(r)
        }
      case (_, IFUTargetType.SiderealTarget(_), _)                                              =>
        val r =
          (ifu2Coordinates, baseCoords).mapN((a, b) => a.angularDistance(b).toDoubleDegrees * 60)
        defocusAmount(r.orEmpty).some
      case _                                                                                    => Length.Zero.some
    }
  }

  def imageType: String = imageTypeConf(obsType)

}

// These are the parameters passed to GHOST from the WDBA.
// We use them to determine the type of configuration being used by GHOST, and instantiate it.
object GhostConfig {

  def giapiConfig[A: GiapiConfig](app: GiapiStatusApply, value: A): Configuration =
    Configuration.single(app.applyItem, value.configValue)

  private[ghost] def ifuConfig(
    ifuNum:        IFUNum,
    ifuTargetType: IFUTargetType,
    coordinates:   Coordinates,
    bundleConfig:  BundleConfig
  ): Configuration = {
    def cfg[P: GiapiConfig](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.configValue}.$paramName", paramVal)

    val demand: DemandType = DemandType.DemandRADec

    val current =
      giapiConfig(ifuNum.targetItem, ifuTargetType) |+|
        giapiConfig(ifuNum.demandItem, demand) |+|
        cfg("ra", coordinates.ra.toAngle.toDoubleDegrees) |+|
        cfg("dec", coordinates.dec.toAngle.toSignedDoubleDegrees) |+|
        giapiConfig(ifuNum.bundleItem, bundleConfig)
    current
  }

  private[ghost] def ifu2NonSidereal(
    bundleConfig: BundleConfig
  ): Configuration =
    bundleConfig match {
      case BundleConfig.Standard =>
        giapiConfig(IFUNum.IFU2.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
          giapiConfig(IFUNum.IFU2.demandItem, DemandType.DemandPark: DemandType) |+|
          giapiConfig(GhostIFU2X, 0.0) |+|
          giapiConfig(GhostIFU2Y, 0.0)
      case _                     =>
        giapiConfig(IFUNum.IFU2.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
          giapiConfig(IFUNum.IFU2.demandItem, DemandType.DemandXY: DemandType) |+|
          giapiConfig(GhostIFU2X, -100.0) |+|
          giapiConfig(GhostIFU2Y, 0.0)

    }

  private[ghost] def ifuConfigNonSidereal(
    ifuNum:        IFUNum,
    ifuTargetType: IFUTargetType,
    bundleConfig:  BundleConfig
  ): Configuration = {
    val demand: DemandType = DemandType.DemandXY

    giapiConfig(ifuNum.targetItem, ifuTargetType) |+|
      giapiConfig(ifuNum.demandItem, demand) |+|
      giapiConfig(GhostIFU1X, 0.0) |+|
      giapiConfig(GhostIFU1Y, 0.0) |+|
      giapiConfig(ifuNum.bundleItem, bundleConfig)
  }

  val UserTargetsApply: Map[Int, (GiapiStatusApply, GiapiStatusApply, GiapiStatusApply)] =
    Map(
      1 -> ((GhostUserTarget1Name, GhostUserTarget1CoordsRADeg, GhostUserTarget1CoordsDecDeg)),
      2 -> ((GhostUserTarget2Name, GhostUserTarget2CoordsRADeg, GhostUserTarget2CoordsDecDeg)),
      3 -> ((GhostUserTarget3Name, GhostUserTarget3CoordsRADeg, GhostUserTarget3CoordsDecDeg)),
      4 -> ((GhostUserTarget4Name, GhostUserTarget4CoordsRADeg, GhostUserTarget4CoordsDecDeg)),
      5 -> ((GhostUserTarget5Name, GhostUserTarget5CoordsRADeg, GhostUserTarget5CoordsDecDeg)),
      6 -> ((GhostUserTarget6Name, GhostUserTarget6CoordsRADeg, GhostUserTarget6CoordsDecDeg)),
      7 -> ((GhostUserTarget7Name, GhostUserTarget7CoordsRADeg, GhostUserTarget7CoordsDecDeg)),
      8 -> ((GhostUserTarget8Name, GhostUserTarget8CoordsRADeg, GhostUserTarget8CoordsDecDeg))
    )

  private[ghost] def ifuPark(ifuNum: IFUNum): Configuration =
    giapiConfig(ifuNum.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
      giapiConfig(ifuNum.demandItem, DemandType.DemandPark: DemandType)

  private[ghost] def fiberConfig1(fa: FiberAgitator): Configuration =
    giapiConfig(GhostFiberAgitator1, fa)

  private[ghost] def fiberConfig2(fa: FiberAgitator): Configuration =
    giapiConfig(GhostFiberAgitator2, fa)

  /* TODO: Fill all the following when the query is filled */
  private def retrieveBasePosition(
    targetEnvironment: TargetEnvironment
  ): Option[Coordinates] = targetEnvironment.explicitBase

  private def retrieveTarget1Id(@unused staticConfig: GhostStaticConfig): Option[TargetId] = none
  private def retrieveSky1Id(@unused staticConfig:    GhostStaticConfig): Option[TargetId] = none
  private def retrieveTarget2Id(@unused staticConfig: GhostStaticConfig): Option[TargetId] = none
  private def retrieveSky2Id(@unused staticConfig:    GhostStaticConfig): Option[TargetId] = none
  private def retrieveTarget(targetEnvironment: TargetEnvironment)(
    tid: Option[TargetId]
  ): Option[GemTarget] = tid.flatMap(i => targetEnvironment.asterism.find(_.id === i)).map(_.target)
  private def retrieveCoordinates(target: GemTarget, time: Timestamp): Option[Coordinates] =
    target match {
      case GemTarget.Sidereal(name, tracking, sourceProfile, catalogInfo) =>
        tracking.at(time.toInstant)
      case GemTarget.Nonsidereal(name, ephemerisKey, sourceProfile)       => ???
      case GemTarget.Opportunity(name, region, sourceProfile)             => ???
    }
  private def retrieveGuideCameraOverride(@unused st: GhostStaticConfig): Option[TimeSpan] = none
  private def retrieveCoadds(@unused st:              GhostStaticConfig): Option[PosInt]   = none

  /**
   * *
   */
  private def calculateScienceMagnitude(t: GemTarget): Option[Double] =
    SourceProfile.integratedBrightnesses
      .getOption(t.sourceProfile)
      .flatMap(_.get(Band.V))
      .orElse(
        SourceProfile.integratedBrightnesses
          .getOption(t.sourceProfile)
          .flatMap(_.get(Band.Gaia))
      )
      .map(_.value.value.value.toDouble)

  def apply(
    staticConfig:      GhostStaticConfig,
    step:              Step[GhostDynamicConfig],
    targetEnvironment: TargetEnvironment,
    observingTime:     Timestamp
  ): Either[ObserveFailure, GhostConfig] = {
    val extracted: Option[GhostConfig] = if (step.stepConfig.isScience) {
      import IFUTargetType.*

      val obsType = step.stepConfig

      val basePosition = retrieveBasePosition(targetEnvironment)

      val target1Type = determineType(
        retrieveTarget(targetEnvironment)(retrieveTarget1Id(staticConfig)),
        retrieveTarget(targetEnvironment)(retrieveSky1Id(staticConfig))
      )
      val target2Type = determineType(
        retrieveTarget(targetEnvironment)(retrieveTarget2Id(staticConfig)),
        retrieveTarget(targetEnvironment)(retrieveSky2Id(staticConfig))
      )

      staticConfig.resolutionMode match {
        case GhostResolutionMode.Standard        =>
          // Single, sidereal target
          (target1Type, target2Type) match {
            case (t1: SiderealTarget, NoTarget)           =>
              t1.target.tracking.at(observingTime.toInstant).map {
                StandardResolutionMode
                  .SingleTarget(
                    obsType,
                    step.observeClass,
                    step.instrumentConfig.blue,
                    step.instrumentConfig.red,
                    basePosition,
                    step.instrumentConfig.ifu1FiberAgitator,
                    step.instrumentConfig.ifu2FiberAgitator,
                    t1,
                    _,
                    List.empty,
                    calculateScienceMagnitude(t1.target),
                    retrieveGuideCameraOverride(staticConfig),
                    staticConfig.slitViewingCameraExposureTime
                  )
              }
            // Single, nonsidereal target
            case (t1: NonsiderealTarget, NoTarget)        =>
              StandardResolutionMode
                .NonSiderealTarget(
                  obsType,
                  step.observeClass,
                  step.instrumentConfig.blue,
                  step.instrumentConfig.red,
                  basePosition,
                  t1,
                  step.instrumentConfig.ifu1FiberAgitator,
                  step.instrumentConfig.ifu2FiberAgitator,
                  List.empty,
                  calculateScienceMagnitude(t1.target),
                  retrieveGuideCameraOverride(staticConfig),
                  staticConfig.slitViewingCameraExposureTime
                )
                .some
            // Target and Sky
            case (t1: SiderealTarget, s2: SkyPosition)    =>
              (retrieveCoordinates(t1.target, observingTime),
               retrieveCoordinates(s2.sky, observingTime)
              ).mapN {
                StandardResolutionMode
                  .TargetPlusSky(
                    obsType,
                    step.observeClass,
                    step.instrumentConfig.blue,
                    step.instrumentConfig.red,
                    basePosition,
                    step.instrumentConfig.ifu1FiberAgitator,
                    step.instrumentConfig.ifu2FiberAgitator,
                    t1,
                    _,
                    s2,
                    _,
                    List.empty,
                    calculateScienceMagnitude(t1.target),
                    retrieveGuideCameraOverride(staticConfig),
                    staticConfig.slitViewingCameraExposureTime
                  )
              }
            // Sky and Target
            case (s1: SkyPosition, t2: SiderealTarget)    =>
              (retrieveCoordinates(s1.sky, observingTime),
               retrieveCoordinates(t2.target, observingTime)
              ).mapN {
                StandardResolutionMode
                  .SkyPlusTarget(
                    obsType,
                    step.observeClass,
                    step.instrumentConfig.blue,
                    step.instrumentConfig.red,
                    basePosition,
                    step.instrumentConfig.ifu1FiberAgitator,
                    step.instrumentConfig.ifu2FiberAgitator,
                    s1,
                    _,
                    t2,
                    _,
                    List.empty,
                    calculateScienceMagnitude(t2.target),
                    retrieveGuideCameraOverride(staticConfig),
                    staticConfig.slitViewingCameraExposureTime
                  )
              }
            // Two targets
            case (t1: SiderealTarget, t2: SiderealTarget) =>
              (retrieveCoordinates(t1.target, observingTime),
               retrieveCoordinates(t2.target, observingTime)
              ).mapN {
                StandardResolutionMode
                  .DualTarget(
                    obsType,
                    step.observeClass,
                    step.instrumentConfig.blue,
                    step.instrumentConfig.red,
                    basePosition,
                    step.instrumentConfig.ifu1FiberAgitator,
                    step.instrumentConfig.ifu2FiberAgitator,
                    t1,
                    _,
                    t2,
                    _,
                    List.empty,
                    calculateScienceMagnitude(t1.target),
                    retrieveGuideCameraOverride(staticConfig),
                    staticConfig.slitViewingCameraExposureTime
                  )
              }
            case _                                        => none
          }
        case core.enums.GhostResolutionMode.High =>
          // Target and Sky
          (target1Type, target2Type) match {
            case (t1: SiderealTarget, s2: SkyPosition) =>
              (retrieveCoordinates(t1.target, observingTime),
               retrieveCoordinates(s2.sky, observingTime)
              ).mapN {
                StandardResolutionMode
                  .TargetPlusSky(
                    obsType,
                    step.observeClass,
                    step.instrumentConfig.blue,
                    step.instrumentConfig.red,
                    basePosition,
                    step.instrumentConfig.ifu1FiberAgitator,
                    step.instrumentConfig.ifu2FiberAgitator,
                    t1,
                    _,
                    s2,
                    _,
                    List.empty,
                    calculateScienceMagnitude(t1.target),
                    retrieveGuideCameraOverride(staticConfig),
                    staticConfig.slitViewingCameraExposureTime
                  )
              }
            // Single, nonsidereal target
            case (t1: NonsiderealTarget, NoTarget)     =>
              HighResolutionMode
                .NonSidereal(
                  obsType,
                  step.observeClass,
                  step.instrumentConfig.blue,
                  step.instrumentConfig.red,
                  basePosition,
                  step.instrumentConfig.ifu1FiberAgitator,
                  step.instrumentConfig.ifu2FiberAgitator,
                  t1,
                  List.empty,
                  calculateScienceMagnitude(t1.target),
                  retrieveGuideCameraOverride(staticConfig),
                  staticConfig.slitViewingCameraExposureTime
                )
                .some
            case _                                     => none
          }
      }
    } else
      GhostCalibration(
        step.stepConfig,
        step.observeClass,
        step.instrumentConfig.blue,
        step.instrumentConfig.red,
        retrieveBasePosition(targetEnvironment),
        step.instrumentConfig.ifu1FiberAgitator,
        step.instrumentConfig.ifu2FiberAgitator,
        staticConfig.resolutionMode,
        retrieveCoadds(staticConfig),
        staticConfig.resolutionMode === GhostResolutionMode.High
      ).some

    extracted.toRight(
      ObserveFailure.OdbSeqError(
        s"Unsupported GHOST configuration ${staticConfig.resolutionMode}, ${retrieveTarget1Id(staticConfig)}, ${retrieveSky1Id(staticConfig)}, ${retrieveTarget2Id(staticConfig)}, ${retrieveSky2Id(staticConfig)}"
      )
    )

  }

  given Eq[GhostConfig] = Eq.instance {
    case (a: StandardResolutionMode.SingleTarget, b: StandardResolutionMode.SingleTarget)   => a === b
    case (a: StandardResolutionMode.DualTarget, b: StandardResolutionMode.DualTarget)       => a === b
    case (a: StandardResolutionMode.TargetPlusSky, b: StandardResolutionMode.TargetPlusSky) =>
      a === b
    case (a: StandardResolutionMode.SkyPlusTarget, b: StandardResolutionMode.SkyPlusTarget) =>
      a === b
    case (a: StandardResolutionMode.NonSiderealTarget,
          b: StandardResolutionMode.NonSiderealTarget
        ) =>
      a === b
    case (a: HighResolutionMode.TargetPlusSky, b: HighResolutionMode.TargetPlusSky)         => a === b
    case _                                                                                  => false
  }

}

case class GhostCalibration(
  override val obsType:        StepConfig,
  override val obsClass:       ObserveClass,
  override val blueConfig:     GhostDetector.Blue,
  override val redConfig:      GhostDetector.Red,
  override val baseCoords:     Option[Coordinates],
  override val fiberAgitator1: GhostIfu1FiberAgitator,
  override val fiberAgitator2: GhostIfu2FiberAgitator,
  override val resolutionMode: GhostResolutionMode,
  override val coAdds:         Option[PosInt],
  isHR:                        Boolean
) extends GhostConfig {

  override val slitMaskConfiguration: Configuration =
    if (isHR) giapiConfig(GhostSlitMaskPositioner, "SMP_HI_ONLY")
    else giapiConfig(GhostSlitMaskPositioner, "SMP_STD_ONLY")

  override val baseConfiguration: Configuration =
    Configuration.Zero

  override def ifu1TargetType: IFUTargetType =
    IFUTargetType.NoTarget

  override def ifu2TargetType: IFUTargetType =
    IFUTargetType.NoTarget

  override def ifu1BundleType: BundleConfig =
    BundleConfig.Standard

  override def ifu2BundleType: Option[BundleConfig] =
    None

  override def ifu2Configuration: Configuration = Configuration.Zero

  override val ifu1Coordinates: Option[Coordinates] = Coordinates.Zero.some

  override val ifu2Coordinates: Option[Coordinates] = None

  override val userTargets: List[GemTarget] = Nil

  override val scienceMagnitude: Option[Double] = None

  def adcConfiguration: Configuration = Configuration.Zero

  override val guideCameraOverride: Option[TimeSpan] = None

  override val svCameraOverride: Option[TimeSpan] = None
}

sealed trait StandardResolutionMode extends GhostConfig {
  import StandardResolutionMode.*

  def ifu1Coordinates: Option[Coordinates]

  override val slitMaskConfiguration: Configuration =
    giapiConfig(GhostSlitMaskPositioner, "SMP_STD_ONLY")

  override val resolutionMode: GhostResolutionMode = GhostResolutionMode.Standard

  override def ifu1BundleType: BundleConfig =
    this match {
      case _: SingleTarget | _: DualTarget | _: TargetPlusSky => BundleConfig.Standard
      case _: SkyPlusTarget                                   => BundleConfig.Sky
      case _: NonSiderealTarget                               => BundleConfig.Standard
    }

  override def ifu2BundleType: Option[BundleConfig] =
    this match {
      case _: SingleTarget                  => None
      case _: DualTarget | _: SkyPlusTarget => Some(BundleConfig.Standard)
      case _: TargetPlusSky                 => Some(BundleConfig.Sky)
      case _: NonSiderealTarget             => Some(BundleConfig.Standard)
    }

  def adcConfiguration: Configuration =
    giapiConfig(GhostAdc1, "ADC_DEMAND_TRACK") |+|
      giapiConfig(GhostAdc2, "ADC_DEMAND_TRACK")
}

object StandardResolutionMode {
  final case class SingleTarget(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.SiderealTarget,
    ifu1Coords:                       Coordinates,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends StandardResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = ifu1Coords.some
    override val ifu2TargetType: IFUTargetType        = IFUTargetType.NoTarget
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifuPark(IFUNum.IFU2)

    override val ifu2Coordinates: Option[Coordinates] = None
  }

  given Eq[SingleTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetType,
     x.ifu1Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )

  final case class NonSiderealTarget(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val ifu1TargetType:      IFUTargetType.NonsiderealTarget,
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends StandardResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = none
    override val ifu2TargetType: IFUTargetType        = IFUTargetType.NoTarget
    override val ifu2Coordinates: Option[Coordinates] = none
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifu2NonSidereal(BundleConfig.Standard)
    override def ifu1Config: Configuration            =
      GhostConfig.ifuConfigNonSidereal(IFUNum.IFU1, ifu1TargetType, BundleConfig.Standard)

  }

  given Eq[NonSiderealTarget] = Eq.by(x =>
    (x.obsType,
     x.obsClass,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.ifu1TargetType,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )

  final case class DualTarget(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.SiderealTarget,
    ifu1Coords:                       Coordinates,
    override val ifu2TargetType:      IFUTargetType.SiderealTarget,
    ifu2Coords:                       Coordinates,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends StandardResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = ifu1Coords.some
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifuConfig(IFUNum.IFU2, ifu2TargetType, ifu2Coords, BundleConfig.Standard)
    override val ifu2Coordinates: Option[Coordinates] = ifu2Coords.some
  }

  given Eq[DualTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetType,
     x.ifu1Coordinates,
     x.ifu2TargetType,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )

  final case class TargetPlusSky(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.SiderealTarget,
    ifu1Coords:                       Coordinates,
    override val ifu2TargetType:      IFUTargetType.SkyPosition,
    ifu2Coords:                       Coordinates,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifuConfig(IFUNum.IFU2, ifu2TargetType, ifu2Coords, BundleConfig.Sky)
    override val ifu2Coordinates: Option[Coordinates] = ifu2Coords.some
    override val ifu1Coordinates: Option[Coordinates] = ifu1Coords.some
  }

  given Eq[TargetPlusSky] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetType,
     x.ifu1Coordinates,
     x.ifu2TargetType,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )

  final case class SkyPlusTarget(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.SkyPosition,
    ifu1Coords:                       Coordinates,
    override val ifu2TargetType:      IFUTargetType.SiderealTarget,
    ifu2Coords:                       Coordinates,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends StandardResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = ifu1Coords.some
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifuConfig(IFUNum.IFU2, ifu2TargetType, ifu2Coords, BundleConfig.Standard)
    override val ifu2Coordinates: Option[Coordinates] = ifu2Coords.some

  }

  given Eq[SkyPlusTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1Coordinates,
     x.ifu2TargetType,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )
}

sealed trait HighResolutionMode extends GhostConfig {

  override val slitMaskConfiguration: Configuration =
    giapiConfig(GhostSlitMaskPositioner, "SMP_HI_ONLY")

  override def ifu1BundleType: BundleConfig = BundleConfig.HighRes

  override def ifu2BundleType: Option[BundleConfig] =
    Some(BundleConfig.Sky)

  override val resolutionMode: GhostResolutionMode = GhostResolutionMode.High

  def adcConfiguration: Configuration =
    giapiConfig(GhostAdc1, "ADC_DEMAND_TRACK") |+|
      giapiConfig(GhostAdc2, "ADC_DEMAND_TRACK")
}

object HighResolutionMode {
  final case class TargetPlusSky(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.SiderealTarget,
    ifu1Coords:                       Coordinates,
    override val ifu2TargetType:      IFUTargetType.SkyPosition,
    ifu2Coords:                       Coordinates,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends HighResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = ifu1Coords.some
    override val ifu2Coordinates: Option[Coordinates] = ifu2Coords.some
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifuConfig(IFUNum.IFU2, ifu2TargetType, ifu2Coords, BundleConfig.Sky)
  }

  given Eq[TargetPlusSky] = Eq.by(x =>
    (x.obsType,
     x.obsClass,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetType,
     x.ifu1Coordinates,
     x.ifu2TargetType,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )

  final case class NonSidereal(
    override val obsType:             StepConfig,
    override val obsClass:            ObserveClass,
    override val blueConfig:          GhostDetector.Blue,
    override val redConfig:           GhostDetector.Red,
    override val baseCoords:          Option[Coordinates],
    override val fiberAgitator1:      GhostIfu1FiberAgitator,
    override val fiberAgitator2:      GhostIfu2FiberAgitator,
    override val ifu1TargetType:      IFUTargetType.NonsiderealTarget,
    override val userTargets:         List[GemTarget],
    override val scienceMagnitude:    Option[Double],
    override val guideCameraOverride: Option[TimeSpan],
    override val svCameraOverride:    Option[TimeSpan]
  ) extends HighResolutionMode {
    override val ifu1Coordinates: Option[Coordinates] = none
    override val ifu2TargetType: IFUTargetType        = IFUTargetType.NoTarget
    override val ifu2Coordinates: Option[Coordinates] = none
    override def ifu2Configuration: Configuration     =
      GhostConfig.ifu2NonSidereal(BundleConfig.HighRes)
    override def ifu1Config: Configuration            =
      GhostConfig.ifuConfigNonSidereal(IFUNum.IFU1, ifu1TargetType, BundleConfig.HighRes)
  }

  given Eq[NonSidereal] = Eq.by(x =>
    (x.obsType,
     x.obsClass,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetType,
     x.ifu1Coordinates,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.scienceMagnitude
    )
  )
}
