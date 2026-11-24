// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.ObservingMode

/**
 * The science steps of one SCORPIO block, laid out on a shared clock that starts at zero. Every
 * step starts at the same time on all channels, when the slowest channel of the previous step
 * ends. Telescope offsets between steps take no time here.
 */
final case class ScorpioBlockTimeline(steps: NonEmptyList[ScorpioBlockTimeline.Step]) derives Eq:
  val duration: TimeSpan = steps.last.end

  def segments(channel: ScorpioBlockTimeline.Channel): List[ScorpioBlockTimeline.Segment] =
    steps.toList.flatMap(_.segments.filter(_.channel === channel))

object ScorpioBlockTimeline:
  // Mirrors the placeholder `scorpio_write` time estimate of the ODB (lucuma-odb#3389).
  val WriteTime: TimeSpan = TimeSpan.fromSecondsBounded(10)

  val ReadoutTime: TimeSpan = ScorpioVisReadoutMode.Default.longSlitReadoutTime

  enum Channel(val tag: String) derives Enumerated:
    case G extends Channel("g")
    case R extends Channel("r")
    case I extends Channel("i")
    case Z extends Channel("z")

  enum SegmentKind(val tag: String) derives Enumerated:
    case Exposure extends SegmentKind("exposure")
    case Readout  extends SegmentKind("readout")
    case Write    extends SegmentKind("write")

  /** Exposure time and number of exposures a channel takes in every step. */
  final case class Exposures(time: TimeSpan, count: PosInt) derives Eq:
    def duration(readout: TimeSpan, write: TimeSpan): TimeSpan =
      (time +| readout +| write) *| count.value

  object Exposures:
    def fromExposureTimeMode(etm: ExposureTimeMode): Option[Exposures] =
      ExposureTimeMode.timeAndCount.getOption(etm).map(tc => Exposures(tc.time, tc.count))

  /** One piece of an exposure. `exposureIndex` is zero based, out of `exposureCount`. */
  final case class Segment(
    channel:       Channel,
    stepIndex:     Int,
    exposureIndex: Int,
    exposureCount: PosInt,
    kind:          SegmentKind,
    start:         TimeSpan,
    end:           TimeSpan
  ) derives Eq:
    val duration: TimeSpan = end -| start

  /** Steps at the same offset share a `label`. */
  final case class Step(
    index:           Int,
    label:           String,
    telescopeConfig: TelescopeConfig,
    start:           TimeSpan,
    end:             TimeSpan,
    segments:        List[Segment]
  ) derives Eq:
    val duration: TimeSpan = end -| start

  def apply(
    exposures:        Channel => Exposures,
    telescopeConfigs: NonEmptyList[TelescopeConfig],
    readout:          TimeSpan = ReadoutTime,
    write:            TimeSpan = WriteTime
  ): ScorpioBlockTimeline =
    val channels: List[(Channel, Exposures)] =
      Enumerated[Channel].all.map(c => c -> exposures(c))

    val stepDuration: TimeSpan =
      channels.map(_._2.duration(readout, write)).maximumOption.orEmpty

    val labels: NonEmptyList[String] = stepLabels(telescopeConfigs.map(_.offset))

    ScorpioBlockTimeline:
      telescopeConfigs
        .zip(labels)
        .zipWithIndex
        .map:
          case ((telescopeConfig, label), index) =>
            val start: TimeSpan = stepDuration *| index
            Step(
              index,
              label,
              telescopeConfig,
              start,
              start +| stepDuration,
              channels.flatMap: (channel, exp) =>
                channelSegments(channel, exp, index, start, readout, write)
            )

  /** None when a channel is not in time and count mode, as its exposures are then unknown. */
  def fromMode(mode: ObservingMode.ScorpioLongSlit): Option[ScorpioBlockTimeline] =
    (Exposures.fromExposureTimeMode(mode.g),
     Exposures.fromExposureTimeMode(mode.r),
     Exposures.fromExposureTimeMode(mode.i),
     Exposures.fromExposureTimeMode(mode.z)
    ).mapN: (g, r, i, z) =>
      ScorpioBlockTimeline(
        {
          case Channel.G => g
          case Channel.R => r
          case Channel.I => i
          case Channel.Z => z
        },
        mode.telescopeConfigs.telescopeConfigs
      )

  private def channelSegments(
    channel:   Channel,
    exposures: Exposures,
    stepIndex: Int,
    stepStart: TimeSpan,
    readout:   TimeSpan,
    write:     TimeSpan
  ): List[Segment] =
    val kinds: List[(SegmentKind, TimeSpan)] =
      List(
        SegmentKind.Exposure -> exposures.time,
        SegmentKind.Readout  -> readout,
        SegmentKind.Write    -> write
      )

    List
      .range(0, exposures.count.value)
      .flatMap(exposureIndex => kinds.map(exposureIndex -> _))
      .mapAccumulate(stepStart):
        case (start, (exposureIndex, (kind, duration))) =>
          val end: TimeSpan = start +| duration
          end -> Segment(channel, stepIndex, exposureIndex, exposures.count, kind, start, end)
      ._2

  // A, B, ... in order of first appearance; past Z the labels carry on as A1, B1, ...
  private def stepLabels(offsets: NonEmptyList[Offset]): NonEmptyList[String] =
    val distinct: List[Offset] =
      offsets.foldLeft(List.empty[Offset])((acc, o) => if acc.contains_(o) then acc else acc :+ o)
    offsets.map: offset =>
      val position: Int  = distinct.indexWhere(_ === offset)
      val letter: String = ('A' + position % 26).toChar.toString
      if position < 26 then letter else s"$letter${position / 26}"
