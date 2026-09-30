// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas

import io.circe.Json
import io.circe.parser.parse
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.schemas.decoders.given
import lucuma.schemas.model.ModeSignalToNoise
import munit.FunSuite

class ModeSignalToNoiseDecoderSuite extends FunSuite:

  private def sn(single: Double, total: Double): String =
    s"""{ "wavelength": { "picometers": 1650000 }, "single": $single, "total": $total }"""

  private def selected(exposureMicros: Long, coadds: Int, single: Double, total: Double): String =
    s"""{
      "results": {
        "selected": {
          "exposureTime": { "microseconds": $exposureMicros },
          "coadds": $coadds,
          "signalToNoiseAt": ${sn(single, total)},
          "peakPixel": { "flux": 1000.5, "adu": 200 }
        }
      }
    }"""

  private def entry(
    wavelengthPm:   Int,
    exposureMicros: Long,
    coadds:         Int,
    single:         Double,
    total:          Double
  ): String =
    val sel = selected(exposureMicros, coadds, single, total)
    s"""{ "centralWavelength": { "picometers": $wavelengthPm }, ${sel.trim.drop(1)}"""

  private val acquisition: String =
    s"""{
      "selected": {
        "signalToNoiseAt": ${sn(5.0, 20.0)},
        "peakPixel": { "flux": 10.0, "adu": 20 }
      }
    }"""

  private def payload(scienceEntries: String*): Json =
    parse(s"""{
      "itcType": "GNIRS_SPECTROSCOPY",
      "acquisition": $acquisition,
      "gnirsSpectroscopyScience": [${scienceEntries.mkString(",")}]
    }""").fold(throw _, identity)

  test("decode GNIRS spectroscopy with repeated central wavelengths"):
    val json = payload(
      entry(1650000, 60000000L, 1, 10.0, 40.0),
      entry(1650000, 30000000L, 2, 12.0, 48.0)
    )

    json.as[ModeSignalToNoise] match
      case Right(ModeSignalToNoise.GnirsSpectroscopy(acq, science)) =>
        assertEquals(acq.signalToNoise.map(_.total.value.toBigDecimal.toDouble), Some(20.0))
        assertEquals(acq.peakPixel.map(_.adu), Some(20))
        assertEquals(science.length, 2)
        assertEquals(science.map(_.centralWavelength).distinct.length, 1)
        assertEquals(science.map(_.centralWavelength),
                     List.fill(2)(Wavelength.intPicometers.getOption(1650000).get)
        )
        assertEquals(science.map(_.exposureTime), List(60.secondTimeSpan, 30.secondTimeSpan))
        assertEquals(science.map(_.coadds.value), List(1, 2))
        assertEquals(science.map(_.values.signalToNoise.map(_.single.value.toBigDecimal.toDouble)),
                     List(Some(10.0), Some(12.0))
        )
        assertEquals(science.map(_.values.peakPixel.map(_.flux)), List(Some(1000.5), Some(1000.5)))
      case other                                                    =>
        fail(s"Unexpected decoding result: $other")

  test("GNIRS spectroscopy missing results.selected fails to decode"):
    val json = payload(
      """{ "centralWavelength": { "picometers": 1650000 }, "results": {} }"""
    )
    assert(json.as[ModeSignalToNoise].isLeft)
