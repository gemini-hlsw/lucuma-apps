// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import io.circe.parser.decode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import munit.FunSuite

class BasicConfigurationSuite extends FunSuite:

  private def ghostDetector(us: Long, count: Int, pm: Int): String =
    s"""{
          "exposureTimeMode": {
            "signalToNoise": null,
            "timeAndCount": {
              "time": { "microseconds": $us },
              "count": $count,
              "at": { "picometers": $pm }
            }
          },
          "readMode": "SLOW",
          "binning": "ONE_BY_ONE"
        }"""

  test("a null igrins2LongSlit must not be swallowed as Igrins2LongSlit"):
    // Reproduces the original bug: the server returns every union member
    val json =
      """{ "igrins2LongSlit": null, "ghostIfu": null, "visitor": null, "exchange": null }"""
    assert(decode[BasicConfiguration](json).isLeft, "all-null observingMode should fail to decode")

  test("a present igrins2LongSlit decodes as Igrins2LongSlit"):
    val json = """{ "igrins2LongSlit": { "offsetMode": null } }"""
    assertEquals(decode[BasicConfiguration](json), Right(BasicConfiguration.Igrins2LongSlit))

  test("ghostIfu decodes even though it is ordered after igrins2LongSlit"):
    val json = s"""{
      "igrins2LongSlit": null,
      "ghostIfu": {
        "resolutionMode": "STANDARD",
        "stepCount": 2,
        "red": ${ghostDetector(1000, 3, 500000)},
        "blue": ${ghostDetector(2000, 4, 400000)}
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(_: BasicConfiguration.GhostIfu) => ()
      case other                                 => fail(s"Expected GhostIfu, got: $other")

  test("exchange decodes as KeckExchange"):
    val json = """{
      "igrins2LongSlit": null,
      "exchange": {
        "keckInstrument": "HIRES",
        "subaruInstrument": null,
        "totalRequestTime": { "microseconds": 5000 }
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(_: BasicConfiguration.KeckExchange) => ()
      case other                                     => fail(s"Expected KeckExchange, got: $other")

  test("gnirsImaging decodes as GnirsImaging"):
    val json = """{
      "igrins2LongSlit": null,
      "gnirsImaging": {
        "filters": [{ "filter": "Y" }, { "filter": "J" }],
        "camera": "SHORT_BLUE"
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(g: BasicConfiguration.GnirsImaging) =>
        assertEquals(g.filters.toList, List(GnirsFilter.Y, GnirsFilter.J))
        assertEquals(g.camera, GnirsCamera.ShortBlue)
      case other                                     => fail(s"Expected GnirsImaging, got: $other")

  test("gnirsSpectroscopy (long slit) decodes as GnirsSpectroscopy"):
    val json = """{
      "igrins2LongSlit": null,
      "gnirsImaging": null,
      "gnirsSpectroscopy": {
        "filter": "CROSS_DISPERSED",
        "slit": { "fpu": "LONG_SLIT_0_10" },
        "ifu": null,
        "prism": "SXD",
        "grating": "D10",
        "camera": "SHORT_BLUE",
        "centralWavelength": { "picometers": 1600000 }
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(_: BasicConfiguration.GnirsSpectroscopy) => ()
      case other                                          => fail(s"Expected GnirsSpectroscopy, got: $other")

  test("gnirsSpectroscopy (IFU) decodes as GnirsSpectroscopy"):
    // Reproduces the reported bug: the ODB always returns both `slit` and `ifu`
    // keys with one of them null, and the double `downField("ifu").downField("fpu")`
    // hop through that null used to hard-fail instead of yielding `None`.
    val json = """{
      "igrins2LongSlit": null,
      "gnirsImaging": null,
      "gnirsSpectroscopy": {
        "filter": "CROSS_DISPERSED",
        "slit": null,
        "ifu": { "fpu": "HIGH_RESOLUTION" },
        "prism": "SXD",
        "grating": "D10",
        "camera": "SHORT_BLUE",
        "centralWavelength": { "picometers": 1600000 }
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(_: BasicConfiguration.GnirsSpectroscopy) => ()
      case other                                          => fail(s"Expected GnirsSpectroscopy, got: $other")

  test("exchange decodes as SubaruExchange"):
    val json = """{
      "igrins2LongSlit": null,
      "exchange": {
        "keckInstrument": null,
        "subaruInstrument": "FOCAS",
        "totalRequestTime": { "microseconds": 5000 }
      }
    }"""
    decode[BasicConfiguration](json) match
      case Right(_: BasicConfiguration.SubaruExchange) => ()
      case other                                       => fail(s"Expected SubaruExchange, got: $other")
