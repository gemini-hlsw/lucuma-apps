// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.syntax.option.*
import io.circe.parser.decode
import lucuma.core.enums.WavelengthOrder
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
          GmosImagingVariant.Grouped(
            order = WavelengthOrder.Increasing,
            offsets = none,
            skyCount = 0.refined,
            skyOffsets = none
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
