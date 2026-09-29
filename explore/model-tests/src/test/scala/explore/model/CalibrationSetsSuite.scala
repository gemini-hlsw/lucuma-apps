// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.util.TimeSpan
import munit.FunSuite

class CalibrationSetsSuite extends FunSuite:
  private def n(i: Int): NonNegInt = NonNegInt.unsafeFrom(i)

  private def mins(m: Long): TimeSpan = TimeSpan.unsafeFromMicroseconds(m * 60_000_000L)

  test("no sets has no text"):
    assertEquals(CalibrationSets.text(n(0), mins(10)), None)

  test("a single set shows its time"):
    assertEquals(CalibrationSets.text(n(1), mins(24)), Some("1 set, 24m 0s"))

  test("several sets show the total and the time of each"):
    assertEquals(CalibrationSets.text(n(2), mins(48)), Some("2 sets, 48m 0s (24m 0s each)"))

  test("sets without time show only the count"):
    assertEquals(CalibrationSets.text(n(1), TimeSpan.Zero), Some("1 set"))
    assertEquals(CalibrationSets.text(n(3), TimeSpan.Zero), Some("3 sets"))
