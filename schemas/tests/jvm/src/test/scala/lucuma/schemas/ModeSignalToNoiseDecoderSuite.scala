// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas

import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.schemas.decoders.given
import lucuma.schemas.model.ModeSignalToNoise
import munit.FunSuite

class ModeSignalToNoiseDecoderSuite extends FunSuite:

  private def picometers(pm: Int): Json = Json.obj("picometers" -> pm.asJson)

  private def sn(single: Double, total: Double): Json =
    Json.obj(
      "wavelength" -> picometers(1650000),
      "single"     -> single.asJson,
      "total"      -> total.asJson
    )

  private def entry(
    wavelengthPm:   Int,
    exposureMicros: Long,
    coadds:         Int,
    single:         Double,
    total:          Double
  ): Json =
    Json.obj(
      "centralWavelength" -> picometers(wavelengthPm),
      "results"           -> Json.obj(
        "selected" -> Json.obj(
          "exposureTime"    -> Json.obj("microseconds" -> exposureMicros.asJson),
          "coadds"          -> coadds.asJson,
          "signalToNoiseAt" -> sn(single, total),
          "peakPixel"       -> Json.obj("flux" -> 1000.5.asJson, "adu" -> 200.asJson)
        )
      )
    )

  private val acquisition: Json =
    Json.obj(
      "selected" -> Json.obj(
        "signalToNoiseAt" -> sn(5.0, 20.0),
        "peakPixel"       -> Json.obj("flux" -> 10.0.asJson, "adu" -> 20.asJson)
      )
    )

  private def payload(scienceEntries: Json*): Json =
    Json.obj(
      "itcType"                  -> "GNIRS_SPECTROSCOPY".asJson,
      "acquisition"              -> acquisition,
      "gnirsSpectroscopyScience" -> Json.arr(scienceEntries*)
    )

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
      Json.obj("centralWavelength" -> picometers(1650000), "results" -> Json.obj())
    )
    assert(json.as[ModeSignalToNoise].isLeft)
