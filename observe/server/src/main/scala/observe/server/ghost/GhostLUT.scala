// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.SkyBackground
import lucuma.core.model
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimeSpan.given
import observe.model.CurrentConditions

import java.time.Duration as JDuration
import java.time.temporal.ChronoUnit

// times in seconds
final case class GuideCameraTimes(gMag: Double, poorWeather: TimeSpan, goodWeather: TimeSpan)
// times in seconds
final case class SVCameraTimes(gMag: Double, poorWeather: TimeSpan, goodWeather: TimeSpan)
final case class ReadoutTimes(
  mode:     GhostReadMode,
  binning:  GhostBinning,
  readRed:  TimeSpan,
  readBlue: TimeSpan
)
// times in seconds
final case class CalibrationWheelPosition(pos: String, blue: TimeSpan, red: TimeSpan)

// GHOST Lookup tables
trait GhostLUT {
  val CameraFilterWheelLUT: List[CalibrationWheelPosition] =
    List(
      CalibrationWheelPosition("CFW_CLEAR",
                               TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS)
      ),
      // The roles below essentially overrride the first row
      // CalibrationWheelPosition("CFW_OD_0_5", TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS), TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS)),
      // CalibrationWheelPosition("CFW_OD_1_0", TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS), TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS)),
      CalibrationWheelPosition("CFW_OD_1_5",
                               TimeSpan.unsafeFromDuration(5, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(1, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_2_0",
                               TimeSpan.unsafeFromDuration(15, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(3, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_2_5",
                               TimeSpan.unsafeFromDuration(90, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(10, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_3_0",
                               TimeSpan.unsafeFromDuration(150, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(15, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_3_5",
                               TimeSpan.unsafeFromDuration(1000, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(45, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_4_0",
                               TimeSpan.unsafeFromDuration(2500, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(120, ChronoUnit.SECONDS)
      ),
      CalibrationWheelPosition("CFW_OD_5_0",
                               TimeSpan.unsafeFromDuration(3600, ChronoUnit.SECONDS),
                               TimeSpan.unsafeFromDuration(300, ChronoUnit.SECONDS)
      )
    )

  private val GuideCameraTimesLUT: List[GuideCameraTimes] =
    List(
      GuideCameraTimes(03.0, duration(00.1), duration(00.1)),
      GuideCameraTimes(03.5, duration(00.1), duration(00.1)),
      GuideCameraTimes(04.0, duration(00.2), duration(00.1)),
      GuideCameraTimes(04.5, duration(00.2), duration(00.1)),
      GuideCameraTimes(05.0, duration(00.2), duration(00.1)),
      GuideCameraTimes(05.5, duration(00.3), duration(00.2)),
      GuideCameraTimes(06.0, duration(00.3), duration(00.2)),
      GuideCameraTimes(06.5, duration(00.4), duration(00.2)),
      GuideCameraTimes(07.0, duration(00.5), duration(00.2)),
      GuideCameraTimes(07.5, duration(00.6), duration(00.3)),
      GuideCameraTimes(08.0, duration(00.7), duration(00.3)),
      GuideCameraTimes(08.5, duration(00.8), duration(00.4)),
      GuideCameraTimes(09.0, duration(00.9), duration(00.5)),
      GuideCameraTimes(09.5, duration(01.0), duration(00.6)),
      GuideCameraTimes(10.0, duration(01.0), duration(00.7)),
      GuideCameraTimes(10.5, duration(02.0), duration(00.8)),
      GuideCameraTimes(11.0, duration(02.0), duration(00.9)),
      GuideCameraTimes(11.5, duration(02.0), duration(01.0)),
      GuideCameraTimes(12.0, duration(03.0), duration(01.0)),
      GuideCameraTimes(12.5, duration(03.0), duration(02.0)),
      GuideCameraTimes(13.0, duration(04.0), duration(02.0)),
      GuideCameraTimes(13.5, duration(05.0), duration(02.0)),
      GuideCameraTimes(14.0, duration(06.0), duration(02.0)),
      GuideCameraTimes(14.5, duration(07.0), duration(03.0)),
      GuideCameraTimes(15.0, duration(08.0), duration(03.0)),
      GuideCameraTimes(15.5, duration(10.0), duration(04.0)),
      GuideCameraTimes(16.0, duration(11.0), duration(05.0)),
      GuideCameraTimes(16.5, duration(14.0), duration(06.0)),
      GuideCameraTimes(17.0, duration(16.0), duration(06.0)),
      GuideCameraTimes(17.5, duration(19.0), duration(08.0)),
      GuideCameraTimes(18.0, duration(23.0), duration(09.0)),
      GuideCameraTimes(18.5, duration(28.0), duration(11.0)),
      GuideCameraTimes(19.0, duration(30.0), duration(12.0)),
      GuideCameraTimes(19.5, duration(40.0), duration(15.0)),
      GuideCameraTimes(20.0, duration(50.0), duration(17.0))
    ).sortBy(_.gMag)

  // the List is never empty
  // Value for mag 17 good weather, i.e. 6 secs
  private val AGMinimumTime: TimeSpan =
    GuideCameraTimesLUT.find(_.gMag == 17.0).map(_.goodWeather).get

  private val SVCameraTimesLUT: List[SVCameraTimes] =
    List(
      SVCameraTimes(3.0, duration(0.2), duration(0.2)),
      SVCameraTimes(3.5, duration(0.2), duration(0.2)),
      SVCameraTimes(4.0, duration(0.3), duration(0.2)),
      SVCameraTimes(4.5, duration(0.3), duration(0.3)),
      SVCameraTimes(5.0, duration(0.4), duration(0.3)),
      SVCameraTimes(5.5, duration(0.6), duration(0.4)),
      SVCameraTimes(6.0, duration(0.7), duration(0.6)),
      SVCameraTimes(6.5, duration(1.0), duration(0.7)),
      SVCameraTimes(7.0, duration(1.0), duration(1.0)),
      SVCameraTimes(7.5, duration(2.0), duration(1.0)),
      SVCameraTimes(8.0, duration(2.0), duration(2.0)),
      SVCameraTimes(8.5, duration(3.0), duration(2.0)),
      SVCameraTimes(9.0, duration(4.0), duration(3.0)),
      SVCameraTimes(9.5, duration(5.0), duration(3.0)),
      SVCameraTimes(10.0, duration(6.0), duration(4.0)),
      SVCameraTimes(10.5, duration(8.0), duration(6.0)),
      SVCameraTimes(11.0, duration(10.0), duration(7.0)),
      SVCameraTimes(11.5, duration(14.0), duration(9.0)),
      SVCameraTimes(12.0, duration(18.0), duration(12.0)),
      SVCameraTimes(12.5, duration(23.0), duration(15.0)),
      SVCameraTimes(13.0, duration(30.0), duration(20.0)),
      SVCameraTimes(13.5, duration(40.0), duration(25.0)),
      SVCameraTimes(14.0, duration(50.0), duration(30.0)),
      SVCameraTimes(14.5, duration(70.0), duration(40.0)),
      SVCameraTimes(15.0, duration(90.0), duration(50.0)),
      SVCameraTimes(15.5, duration(110.0), duration(70.0)),
      SVCameraTimes(16.0, duration(150.0), duration(90.0)),
      SVCameraTimes(16.5, duration(190.0), duration(110.0)),
      SVCameraTimes(17.0, duration(250.0), duration(150.0)),
      SVCameraTimes(17.5, duration(300.0), duration(190.0)),
      SVCameraTimes(18.0, duration(300.0), duration(240.0)),
      SVCameraTimes(18.5, duration(300.0), duration(300.0)),
      SVCameraTimes(19.0, duration(300.0), duration(300.0)),
      SVCameraTimes(19.5, duration(600.0), duration(600.0)),
      SVCameraTimes(20.0, duration(600.0), duration(600.0)),
      SVCameraTimes(24.0, duration(600.0), duration(600.0))
    ).sortBy(_.gMag)

  // the List is never empty
  private val SVMinimumTime: SVCameraTimes = SVCameraTimesLUT.minBy(_.goodWeather)

  private def duration(sec: Int, milliSeconds: Int): TimeSpan = TimeSpan.unsafeFromDuration(
    JDuration.ofSeconds(sec.toLong, milliSeconds.toLong * 1000000L)
  )

  private def duration(sec: Double): TimeSpan = TimeSpan.unsafeFromMicroseconds((sec * 1e6).toLong)

  // TODO: Call directly the OCS code
  // Taken from OCS GhostCameras.scala
  private object Red {
    val ReadoutTime: Map[(GhostBinning, GhostReadMode), TimeSpan] = {
      import GhostBinning.*
      import GhostReadMode.*
      Map(
        (OneByOne, Slow)     -> duration(100, 675),
        (OneByOne, Medium)   -> duration(58, 994),
        (OneByOne, Fast)     -> duration(23, 520),
        (OneByTwo, Slow)     -> duration(51, 271),
        (OneByTwo, Medium)   -> duration(30, 230),
        (OneByTwo, Fast)     -> duration(12, 341),
        (OneByFour, Slow)    -> duration(26, 564),
        (OneByFour, Medium)  -> duration(15, 838),
        (OneByFour, Fast)    -> duration(6, 773),
        (OneByEight, Slow)   -> duration(14, 198),
        (OneByEight, Medium) -> duration(8, 686),
        (OneByEight, Fast)   -> duration(3, 977),
        (TwoByTwo, Slow)     -> duration(28, 364),
        (TwoByTwo, Medium)   -> duration(17, 696),
        (TwoByTwo, Fast)     -> duration(8, 577),
        (TwoByFour, Slow)    -> duration(15, 146),
        (TwoByFour, Medium)  -> duration(9, 638),
        (TwoByFour, Fast)    -> duration(4, 929),
        (TwoByEight, Slow)   -> duration(8, 534),
        (TwoByEight, Medium) -> duration(5, 580),
        (TwoByEight, Fast)   -> duration(3, 578),
        (FourByFour, Slow)   -> duration(9, 536),
        (FourByFour, Medium) -> duration(6, 581),
        (FourByFour, Fast)   -> duration(4, 77)
      )
    }
  }

  private object Blue {

    val ReadoutTime: Map[(GhostBinning, GhostReadMode), TimeSpan] = {
      import GhostBinning.*
      import GhostReadMode.*
      Map(
        (OneByOne, Slow)     -> duration(45, 957),
        (OneByOne, Medium)   -> duration(27, 118),
        (OneByOne, Fast)     -> duration(11, 78),
        (OneByTwo, Slow)     -> duration(23, 808),
        (OneByTwo, Medium)   -> duration(14, 237),
        (OneByTwo, Fast)     -> duration(6, 72),
        (OneByFour, Slow)    -> duration(12, 741),
        (OneByFour, Medium)  -> duration(7, 784),
        (OneByFour, Fast)    -> duration(3, 575),
        (OneByEight, Slow)   -> duration(7, 229),
        (OneByEight, Medium) -> duration(4, 574),
        (OneByEight, Fast)   -> duration(3, 75),
        (TwoByTwo, Slow)     -> duration(13, 644),
        (TwoByTwo, Medium)   -> duration(8, 633),
        (TwoByTwo, Fast)     -> duration(4, 425),
        (TwoByFour, Slow)    -> duration(7, 68),
        (TwoByFour, Medium)  -> duration(5, 24),
        (TwoByFour, Fast)    -> duration(3, 71),
        (TwoByEight, Slow)   -> duration(4, 722),
        (TwoByEight, Medium) -> duration(3, 223),
        (TwoByEight, Fast)   -> duration(3, 42),
        (FourByFour, Slow)   -> duration(5, 226),
        (FourByFour, Medium) -> duration(3, 722),
        (FourByFour, Fast)   -> duration(3, 44)
      )
    }
  }

  def calcBlueCount(
    obsType:    StepConfig,
    coAdds:     Option[PosInt],
    blueConfig: GhostDetector.Blue
  ): PosInt =
    calcCount(obsType, coAdds, blueConfig.value.exposureCount)

  private def calcCount(obsType: StepConfig, coAdds: Option[PosInt], configCount: PosInt): PosInt =
    if (obsType === StepConfig.Bias) coAdds.getOrElse(configCount) else configCount

  def calcRedCount(
    obsType:   StepConfig,
    coAdds:    Option[PosInt],
    redConfig: GhostDetector.Red
  ): PosInt =
    calcCount(obsType, coAdds, redConfig.value.exposureCount)

  // Readout time to fallback
  private val fallbackReadoutTimeRed: TimeSpan =
    TimeSpan.fromMicrosecondsBounded(Red.ReadoutTime.values.map(_.toMilliseconds).max)

  private val fallbackReadoutTimeBlue: TimeSpan =
    TimeSpan.fromMicrosecondsBounded(Blue.ReadoutTime.values.map(_.toMilliseconds).max)

  private def blueReadoutTime(blueChannel: GhostDetector.Blue): TimeSpan = {
    val blueKey =
      (blueChannel.value.binning, blueChannel.value.readMode)
    Blue.ReadoutTime.getOrElse(blueKey, fallbackReadoutTimeBlue)
  }

  private def redReadoutTime(redChannel: GhostDetector.Red): TimeSpan = {
    val redKey =
      (redChannel.value.binning, redChannel.value.readMode)
    Red.ReadoutTime.getOrElse(redKey, fallbackReadoutTimeRed)
  }

  def readoutTime(
    blueChannel: GhostDetector.Blue,
    redChannel:  GhostDetector.Red
  ): TimeSpan =
    blueReadoutTime(blueChannel).max(redReadoutTime(redChannel))

  // REL-4239
  def totalObserveTime(
    blueChannel: GhostDetector.Blue,
    redChannel:  GhostDetector.Red
  ): TimeSpan = {
    val blueKey   =
      (blueChannel.value.binning, blueChannel.value.readMode)
    val blue      = Blue.ReadoutTime.getOrElse(blueKey, fallbackReadoutTimeBlue)
    val redKey    =
      (redChannel.value.binning, redChannel.value.readMode)
    val red       = Red.ReadoutTime.getOrElse(redKey, fallbackReadoutTimeRed)
    val blueTotal =
      (blueChannel.value.exposureTime +| blue) *| blueChannel.value.exposureCount.value
    val redTotal  =
      (redChannel.value.exposureTime +| red) *| redChannel.value.exposureCount.value

    blueTotal.max(redTotal)
  }

  private def isPoorWeather(conditions: CurrentConditions): Boolean =
    conditions.sb.forall(_ >= SkyBackground.Gray) || conditions.ce.forall(
      _.percentile.toPercent >= BigDecimal(80)
    ) || conditions.iq.forall(
      _.value.value.value >= ImageQuality.Preset.TwoPointZero.toImageQuality.value.value.value
    )

  def svCameraTime(conditions: CurrentConditions, mag: Option[Double]): TimeSpan = {
    val times = mag
      .flatMap(mag =>
        SVCameraTimesLUT
          .find(_.gMag > mag)
      )
      .getOrElse(SVMinimumTime)
    if (isPoorWeather(conditions)) times.poorWeather else times.goodWeather
  }

  private val svReadoutTime: TimeSpan = TimeSpan.unsafeFromMicroseconds(200000)

  // REL-4270
  def svCameraRepeats(
    conditions: CurrentConditions,
    mag:        Option[Double],
    blueConfig: GhostDetector.Blue,
    redConfig:  GhostDetector.Red
  ): Int = {
    val total  = totalObserveTime(blueConfig, redConfig)
    val svTime = svCameraTime(conditions, mag)
    (total.toSeconds.toDouble / (svTime +| svReadoutTime).toSeconds.toDouble).floor.toInt
  }

  // REL-4372
  def svOverrideCameraRepeats(
    svOverrideTime: TimeSpan,
    blueConfig:     GhostDetector.Blue,
    redConfig:      GhostDetector.Red
  ): Int = {
    val total = totalObserveTime(blueConfig, redConfig)
    (total.toSeconds.toDouble / (svOverrideTime +| svReadoutTime).toSeconds.toDouble).floor.toInt
  }

  def svCalibSVRepeats(
    obsType:    StepConfig,
    blueConfig: GhostDetector.Blue,
    redConfig:  GhostDetector.Red,
    coAdds:     Option[PosInt]
  ): Int =
    if (obsType === StepConfig.Bias) {
      val scienceReadout  = readoutTime(blueConfig, redConfig)
      val noCoaddsDefined =
        (scienceReadout.toSeconds.toDouble / svReadoutTime.toSeconds.toDouble).floor.toInt.min(100)
      val coAddsDefined   = coAdds.map { c =>
        // Science time taking coadds amount of exposures on red and blue
        val scienceReadout: TimeSpan =
          (blueReadoutTime(blueConfig) *| c.value).max(redReadoutTime(redConfig) *| c.value)
        (scienceReadout.toSeconds.toDouble / svReadoutTime.toSeconds.toDouble).floor.toInt.min(100)
      }
      coAddsDefined.getOrElse(noCoaddsDefined)

    } else GhostCalibrationSVRepeat

  def agCameraTime(conditions: CurrentConditions, mag: Option[Double]): TimeSpan = {
    val times = mag
      .flatMap(mag =>
        GuideCameraTimesLUT
          .find(_.gMag > mag)
      )
    (if (isPoorWeather(conditions)) times.map(_.poorWeather) else times.map(_.goodWeather))
      .getOrElse(AGMinimumTime)
  }

  def imageTypeConf(stepCfg: StepConfig): String = stepCfg match {
    case StepConfig.Science => "OBJECT"
    case StepConfig.Bias    => "BIAS"
    case StepConfig.Dark    => "DARK"
    case x if x.isArc       => "ARC"
    case x if x.isFlat      => "FLAT"
    case _                  => ""
  }

  def isScience(obsType: StepConfig): Boolean = obsType === StepConfig.Science

  private val BiasSVTime: TimeSpan = duration(0.0)
  private val FlatSVTime: TimeSpan = duration(0.1)
  private val ArcSVTime: TimeSpan  = duration(300.0)
  val GhostCalibrationSVRepeat     = 2

  def svCalibExposureTime(stepCfg: StepConfig): TimeSpan = stepCfg match {
    case StepConfig.Bias => BiasSVTime
    case x if x.isArc    => ArcSVTime
    case x if x.isFlat   => FlatSVTime
    case _               => TimeSpan.Zero
  }

}

object GhostLUT extends GhostLUT
