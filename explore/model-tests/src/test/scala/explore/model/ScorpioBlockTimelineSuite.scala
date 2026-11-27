// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.ScorpioBlockTimeline.*
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.TimeSpan
import munit.FunSuite

class ScorpioBlockTimelineSuite extends FunSuite:
  private def seconds(s: Int): TimeSpan = TimeSpan.fromSecondsBounded(s.toDouble)

  private val readout: TimeSpan = seconds(20)
  private val write: TimeSpan   = seconds(10)

  private def at(q: Int): TelescopeConfig =
    TelescopeConfig(
      Offset.signedDecimalArcseconds.reverseGet((BigDecimal(0), BigDecimal(q))),
      StepGuideState.Enabled
    )

  private val abba: NonEmptyList[TelescopeConfig] = NonEmptyList.of(at(5), at(-5), at(-5), at(5))

  private def exposures(time: Int, count: Int): Exposures =
    Exposures(seconds(time), PosInt.unsafeFrom(count))

  private def timeline(
    gr:               Exposures,
    iz:               Exposures,
    telescopeConfigs: NonEmptyList[TelescopeConfig] = abba
  ): ScorpioBlockTimeline =
    ScorpioBlockTimeline(
      {
        case Channel.G | Channel.R => gr
        case Channel.I | Channel.Z => iz
      },
      telescopeConfigs,
      readout,
      write
    )

  private def ends(t: ScorpioBlockTimeline, channel: Channel, stepIndex: Int): TimeSpan =
    t.segments(channel).filter(_.stepIndex === stepIndex).last.end

  test("equal settings leave no idle time"):
    val t = timeline(exposures(300, 2), exposures(300, 2))
    // Each step is 2 x (300 + 20 + 10).
    assertEquals(t.steps.map(_.duration), abba.as(seconds(660)))
    assertEquals(t.duration, seconds(2640))
    Channel.values.foreach: channel =>
      t.steps.toList.foreach: step =>
        assertEquals(ends(t, channel, step.index), step.end)

  test("a faster pair ends early and the next step still starts aligned"):
    val t = timeline(exposures(300, 2), exposures(100, 1))
    assertEquals(t.duration, seconds(2640))
    assertEquals(ends(t, Channel.G, 0), seconds(660))
    assertEquals(ends(t, Channel.I, 0), seconds(130))
    Channel.values.foreach: channel =>
      assertEquals(
        t.segments(channel)
          .filter(_.exposureIndex === 0)
          .filter(_.kind === SegmentKind.Science)
          .map(_.start),
        List(0, 660, 1320, 1980).map(seconds)
      )

  test("the slowest channel sets the step, whichever pair it is"):
    val t = timeline(exposures(100, 3), exposures(600, 1))
    // g/r: 3 x 130 = 390, i/z: 630.
    assertEquals(t.steps.head.duration, seconds(630))
    assertEquals(ends(t, Channel.R, 0), seconds(390))

  test("every exposure is followed by its readout and write"):
    val t = timeline(exposures(300, 2), exposures(300, 2), NonEmptyList.one(at(0)))
    assertEquals(
      t.segments(Channel.G).map(s => (s.exposureIndex, s.kind, s.start, s.end)),
      List(
        (0, SegmentKind.Science, seconds(0), seconds(300)),
        (0, SegmentKind.Readout, seconds(300), seconds(320)),
        (0, SegmentKind.Write, seconds(320), seconds(330)),
        (1, SegmentKind.Science, seconds(330), seconds(630)),
        (1, SegmentKind.Readout, seconds(630), seconds(650)),
        (1, SegmentKind.Write, seconds(650), seconds(660))
      )
    )
    assertEquals(t.segments(Channel.G).map(_.exposureCount.value).distinct, List(2))

  test("a single telescope config is a one step block"):
    val t = timeline(exposures(60, 1), exposures(60, 1), NonEmptyList.one(at(0)))
    assertEquals(t.steps.map(_.label), NonEmptyList.one("A"))
    assertEquals(t.duration, seconds(90))

  test("equal offsets share a letter in order of first appearance"):
    val gr = exposures(60, 1)
    assertEquals(timeline(gr, gr).steps.map(_.label).toList, List("A", "B", "B", "A"))
    assertEquals(
      timeline(gr, gr, NonEmptyList.of(at(0), at(3), at(-3), at(0), at(3))).steps
        .map(_.label)
        .toList,
      List("A", "B", "C", "A", "B")
    )

  test("steps keep their telescope config"):
    val gr = exposures(60, 1)
    assertEquals(timeline(gr, gr).steps.map(_.telescopeConfig), abba)

  test("the write time mirrors the ODB placeholder"):
    assertEquals(WriteTime, seconds(10))
