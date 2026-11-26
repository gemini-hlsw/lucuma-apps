// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.parser.decode
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.util.TimeSpan
import lucuma.refined.*
import munit.FunSuite

class ObservingModeSuite extends FunSuite:

  test("decode obsmode.json as ObservingMode"):
    val jsonSource = scala.io.Source.fromResource("obsmode.json")
    val json       = jsonSource.mkString
    jsonSource.close()

    val result = decode[ObservingMode](json)

    result match {
      case Right(
            ObservingMode.GmosNorthImaging(
              variant = variant,
              initialFilters = initialFilters,
              filters = filters,
              explicitBin = explicitBin,
              explicitAmpReadMode = explicitAmpReadMode,
              explicitAmpGain = explicitAmpGain,
              explicitRoi = explicitRoi
            )
          ) =>
        assertEquals(
          variant,
          ImagingVariant.Grouped(
            order = WavelengthOrder.Increasing,
            offsets = none,
            skyCount = 0.refined,
            skyOffsets = none,
            exposuresPerOffset = 1.refined
          )
        )
        assertEquals(initialFilters.size, 1)
        assertEquals(filters.size, 1)
        assertEquals(explicitBin, none)
        assertEquals(explicitAmpReadMode, none)
        assertEquals(explicitAmpGain, none)
        assertEquals(explicitRoi, none)
      case Right(_)    =>
        assert(
          false,
          s"Decoded $json but got wrong ObservingMode subtype: ${result.toString}"
        )
      case Left(error) =>
        assert(false, s"Failed to decode - Error: $error")
    }

  private def timeAndCount(seconds: Int, count: Int, nm: Int): String =
    s"""{
          "signalToNoise": null,
          "timeAndCount": {
            "time": { "microseconds": ${seconds * 1000000L} },
            "count": $count,
            "at": { "picometers": ${nm * 1000} }
          }
        }"""

  private def alongSlit(qs: Int*): String =
    val configs = qs
      .map(q => s"""{ "q": { "microarcseconds": ${q * 1000000L} }, "guiding": "ENABLED" }""")
      .mkString(", ")
    s"""{ "offsetMode": "NOD_ALONG_SLIT", "alongSlit": [$configs], "toSky": null }"""

  private def etm(seconds: Int, count: Int, nm: Int): ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(
      TimeSpan.unsafeFromMicroseconds(seconds * 1000000L),
      PosInt.unsafeFrom(count),
      Wavelength.fromIntNanometers(nm).get
    )

  test("decode scorpioLongSlit as ObservingMode"):
    val json = s"""{
      "igrins2LongSlit": null,
      "ghostIfu": null,
      "scorpioLongSlit": {
        "fpu": "LONG_SLIT_1_08",
        "g": ${timeAndCount(10, 2, 469)},
        "r": ${timeAndCount(10, 2, 622)},
        "i": ${timeAndCount(20, 1, 755)},
        "z": ${timeAndCount(20, 1, 889)},
        "defaultTelescopeConfigs": ${alongSlit(0, -15, 15, 0)},
        "explicitTelescopeConfigs": ${alongSlit(-5, 5)},
        "repeats": 3
      },
      "visitor": null
    }"""

    val explicitConfigs = SlitTelescopeConfigs.AlongSlit(
      NonEmptyList
        .of(-5, 5)
        .map(q =>
          TelescopeConfigAlongSlit(Offset.Q(Angle.fromMicroarcseconds(q * 1000000L)),
                                   StepGuideState.Enabled
          )
        )
    )

    decode[ObservingMode](json) match
      case Right(s: ObservingMode.ScorpioLongSlit) =>
        assertEquals(s.fpu, ScorpioFpu.LongSlit_1_08)
        assertEquals(s.g, etm(10, 2, 469))
        assertEquals(s.r, etm(10, 2, 622))
        assertEquals(s.i, etm(20, 1, 755))
        assertEquals(s.z, etm(20, 1, 889))
        assertEquals(s.defaultTelescopeConfigs.telescopeConfigs.length, 4)
        assertEquals(s.explicitTelescopeConfigs, explicitConfigs.some)
        assertEquals(s.telescopeConfigs, explicitConfigs)
        assertEquals(s.repeats.value, 3)
        assert(s.isCustomized)
      case other                                   =>
        fail(s"Expected ScorpioLongSlit, got: $other")

  test("daytime calibration without calibrationSets does not decode"):
    val json = """{ "daytimeCalibration": { "instrument": "GMOS_NORTH" } }"""
    assert(decode[ObservingMode](json).isLeft)

  test("daytime calibration with calibrationSets decodes"):
    val json = """{ "daytimeCalibration": { "instrument": "GMOS_NORTH", "calibrationSets": [] } }"""
    assertEquals(
      decode[ObservingMode](json),
      Right(ObservingMode.DaytimeCalibration(Instrument.GmosNorth, Nil))
    )
