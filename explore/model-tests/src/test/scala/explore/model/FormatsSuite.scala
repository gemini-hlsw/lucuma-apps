// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import explore.model.formats.*
import lucuma.core.arb.*
import lucuma.core.arb.ArbTime.given
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.LocalTime

class FormatsSuite extends munit.DisciplineSuite:
  override def scalaCheckInitialSeed = "tprrQ0295RsiQ-5mvSW9ajurDGsf3haEulf38PNqa0K="

  private val perturbations: List[String => Gen[String]] =
    List(_ => arbitrary[String]) // swap for a random string

  val finiteDurationsHM: Gen[String] =
    arbitrary[TimeSpan]
      .map { ts =>
        s"${ts.toHoursPart}:${ts.toMinutesPart}"
      }
      .flatMapOneOf(Gen.const, perturbations*)

  val finiteDurationsHMS: Gen[String] =
    arbitrary[TimeSpan]
      .map { ts =>
        val secs =
          if (ts.toMillisPart > 0)
            f"${ts.toSecondsPart}%02d.${ts.toMillisPart}%03d"
          else
            f"${ts.toSecondsPart}%02d"
        s"${ts.toHoursPart}:${ts.toMinutes}:$secs"
      }
      .flatMapOneOf(Gen.const, (((_: String) => finiteDurationsHM) :: perturbations)*)

  // Tests durationS
  assertEquals(parsers.durationS.parseAll("00.001").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(0)).some
  )
  assertEquals(parsers.durationS.parseAll("45").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45)).some
  )
  assertEquals(parsers.durationS.parseAll("45.0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45)).some
  )
  assertEquals(parsers.durationS.parseAll("45.034").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45)).some
  )
  assertEquals(parsers.durationS.parseAll("0.005046").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(0)).some
  )
  assertEquals(parsers.durationS.parseAll("35").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(35)).some
  )
  assertEquals(parsers.durationS.parseAll("0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(0)).some
  )
  assertEquals(parsers.durationS.parseAll("60").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(60)).some
  )
  // Test durationMs
  assertEquals(parsers.durationMs.parseAll("00.001").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(1)).some
  )
  assertEquals(parsers.durationMs.parseAll("45").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45)).some
  )
  assertEquals(parsers.durationMs.parseAll("45.0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45)).some
  )
  assertEquals(parsers.durationMs.parseAll("45.034").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(45).withNanos(34 * 1000000)).some
  )
  assertEquals(parsers.durationMs.parseAll("35").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(35)).some
  )
  assertEquals(parsers.durationMs.parseAll("0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofSeconds(0)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.35").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(350)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.350").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(350)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.000").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(0)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.045").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(45)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.701").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(701)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.38").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(380)).some
  )
  // Extra digits beyond 3 should be discarded
  assertEquals(parsers.durationMs.parseAll("0.00045").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(0)).some
  )
  assertEquals(parsers.durationMs.parseAll("0.0000701").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMillis(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:0").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:00").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(0)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:2").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(2)).some
  )
  assertEquals(parsers.durationHM.parseAll("0:48").toOption,
               TimeSpan.unsafeFromDuration(Duration.ofMinutes(48)).some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:26.000").toOption,
    TimeSpan.unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(26)).some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:09.033").toOption,
    TimeSpan
      .unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(9).plusMillis(33))
      .some
  )
  assertEquals(
    parsers.durationHMS.parseAll("18179:02:09.0319029201091").toOption,
    TimeSpan
      .unsafeFromDuration(Duration.ofHours(18179).plusMinutes(2).plusSeconds(9).plusMillis(31))
      .some
  )
  checkAll(
    "durationHMValidWedge",
    ValidWedgeTests(durationHM).validWedgeLawsWith(finiteDurationsHM)
  )
  checkAll(
    "durationHMSValidWedge",
    ValidWedgeTests(durationHMS).validWedgeLawsWith(finiteDurationsHMS)
  )
  checkAll(
    "durationMsWedge",
    ValidWedgeTests(durationMs).validWedgeLaws
  )
  checkAll(
    "durationSWedge",
    ValidWedgeTests(durationS).validWedgeLaws
  )
  assertEquals(durationS.reverseGet(TimeSpan.fromSeconds(60).get), "60")
  assertEquals(durationS.reverseGet(TimeSpan.fromSeconds(70).get), "70")

  assertEquals(parsers.timeHM.parseAll("0:48").toOption, LocalTime.of(0, 48).some)
  assertEquals(parsers.timeHM.parseAll("21:48").toOption, LocalTime.of(21, 48).some)
  assertEquals(parsers.timeHM.parseAll("0:0").toOption, LocalTime.of(0, 0).some)
  assertEquals(parsers.timeHM.parseAll("0").toOption, LocalTime.of(0, 0).some)
  assertEquals(parsers.timeHM.parseAll("23").toOption, LocalTime.of(23, 0).some)
  checkAll(
    "time24h",
    ValidWedgeTests(time24h).validWedgeLaws
  )

  private def n(i: Int): NonNegInt = NonNegInt.unsafeFrom(i)

  private def mins(m: Long): TimeSpan = TimeSpan.unsafeFromMicroseconds(m * 60_000_000L)

  test("calibration sets has no text without sets"):
    assertEquals(formatCalibrationSets(n(0), mins(10)), None)

  test("calibration sets shows the time of a single set"):
    assertEquals(formatCalibrationSets(n(1), mins(24)), Some("1 set, 24m 0s"))

  test("calibration sets shows the total and the time of each"):
    assertEquals(formatCalibrationSets(n(2), mins(48)), Some("2 sets, 48m 0s (24m 0s each)"))

  test("calibration sets without time shows only the count"):
    assertEquals(formatCalibrationSets(n(1), TimeSpan.Zero), Some("1 set"))
    assertEquals(formatCalibrationSets(n(3), TimeSpan.Zero), Some("3 sets"))

  test("count times each has no text without a count"):
    assertEquals(formatCountTimesEach(n(0), mins(10)), None)

  test("count times each shows the time of each"):
    assertEquals(formatCountTimesEach(n(1), mins(15)), Some("1 × 15m 0s"))
    assertEquals(formatCountTimesEach(n(2), mins(30)), Some("2 × 15m 0s"))

  test("count times each without time shows only the count"):
    assertEquals(formatCountTimesEach(n(2), TimeSpan.Zero), Some("2"))
