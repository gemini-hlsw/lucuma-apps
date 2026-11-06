// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.Instrument
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.ui.input.InputCodec
import munit.FunSuite

final class ExposureTimeRulesSuite extends FunSuite:

  private def errors(codec: InputCodec[Option[TimeSpan]], text: String): List[String] =
    codec.valid.getValid(text).left.toOption.toList.flatMap(_.toList.map(_.value))

  test("an exposure below the instrument minimum is rejected with the core message"):
    val codec = inputCodecs.exposureTimeSeconds(ExposureTimeRules(Instrument.GmosNorth.some))
    assertEquals(errors(codec, "0"), List("Exposure times for GMOS North must be at least 1 s."))
    assertEquals(codec.valid.getValid("1"), 1.secTimeSpan.some.asRight)

  test("with no explicit read mode the minimum is that of the read mode for the shortest exposures"):
    val codec = inputCodecs.exposureTimeMilliseconds(ExposureTimeRules(Instrument.Gnirs.some))
    assertEquals(errors(codec, "0.1"), List("Exposure times for GNIRS very bright read mode must be at least 0.2 s."))
    assertEquals(errors(codec, "0.2"), Nil)

  test("an explicit read mode raises the minimum"):
    val codec = inputCodecs.exposureTimeMilliseconds(ExposureTimeRules.gnirs(GnirsReadMode.Faint.some))
    assertEquals(errors(codec, "5"), List("Exposure times for GNIRS faint read mode must be at least 9 s."))

  test("warnings are not applied and an empty field is accepted"):
    val codec = inputCodecs.exposureTimeSeconds(ExposureTimeRules(Instrument.GmosNorth.some))
    assertEquals(errors(codec, "2000"), Nil)
    assertEquals(codec.valid.getValid(""), none[TimeSpan].asRight)
