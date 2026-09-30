// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import navigate.model.enums.LightSink
import navigate.model.enums.LightSource
import navigate.server.acm.Encoder.*
import navigate.server.tcs.ScienceFoldPositionCodex.given
import navigate.server.tcs.ScienceFoldPositionCodex.scienceFoldSinkName

class ScienceFoldPositionCodexSuite extends munit.FunSuite {

  private def encodedName(sink: LightSink, from: LightSource, port: Int): String =
    ScienceFold.Position(from, sink.scienceFoldSinkName(from), port).encode[String]

  test("GMOS IFU light sinks are encoded as \"gmos\" + port without AO") {
    assertEquals(encodedName(LightSink.GmosNorthIfu, LightSource.Sky, 3), "gmos3")
    assertEquals(encodedName(LightSink.GmosNorthIfu, LightSource.GCAL, 3), "gcal2gmos3")
    assertEquals(encodedName(LightSink.GmosSouthIfu, LightSource.Sky, 5), "gmos5")
    assertEquals(encodedName(LightSink.GmosSouthIfu, LightSource.AO, 5), "ao2gmos5")
  }

  test("GMOS North IFU with Altair is encoded as \"gmosifu\" + port") {
    assertEquals(encodedName(LightSink.GmosNorthIfu, LightSource.AO, 3), "ao2gmosifu3")
  }

  test("GMOS imaging light sinks are always encoded as \"gmos\" + port") {
    assertEquals(encodedName(LightSink.GmosNorth, LightSource.AO, 3), "ao2gmos3")
    assertEquals(encodedName(LightSink.GmosSouth, LightSource.Sky, 3), "gmos3")
  }

}
