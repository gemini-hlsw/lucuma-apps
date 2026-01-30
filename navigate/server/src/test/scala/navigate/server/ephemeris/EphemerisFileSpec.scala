// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.ephemeris

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.model.Ephemeris
import lucuma.core.util.Timestamp
import munit.CatsEffectSuite

import java.nio.file.Paths
import java.time.LocalDateTime

class EphemerisFieSpec extends CatsEffectSuite {

  val expectedFiles = List(
    EphemerisFile(
      Path("AsteroidNew_A804+RA.eph"),
      Ephemeris.Key.fromTypeAndDes.unsafeGet((EphemerisKeyType.AsteroidNew, "A804 RA")),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 27, 10, 0, 0)),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 28, 9, 59, 0))
    ),
    EphemerisFile(
      Path("MajorBody_503.eph"),
      Ephemeris.Key.fromTypeAndDes.unsafeGet((EphemerisKeyType.MajorBody, "503")),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 27, 10, 0, 0)),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 28, 9, 59, 0))
    ),
    EphemerisFile(
      Path("Comet_P%2F2009+WX51.eph"),
      Ephemeris.Key.fromTypeAndDes.unsafeGet((EphemerisKeyType.Comet, "P/2009 WX51")),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 27, 10, 0, 0)),
      Timestamp.unsafeFromLocalDateTime(LocalDateTime.of(2026, 1, 28, 9, 59, 0))
    )
  )

  test("Read ephemeris files") {
    val basePath = Path.fromNioPath(Paths.get(getClass.getResource("/ephemerides").toURI))
    Files.forIO
      .list(basePath)
      .filter(_.extName === ".eph")
      .evalMap(EphemerisFile.EphemerisParser.parse[IO])
      .compile
      .toList
      .map(_.flattenOption)
      .map(l =>
        expectedFiles.map(x => x.copy(fileName = basePath / x.fileName)).foreach { expected =>
          assert(l.contains(expected))
        }
        assertEquals(l.length, expectedFiles.length)
      )
  }

}
