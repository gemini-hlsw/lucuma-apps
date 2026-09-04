// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gws

import algebra.instances.all.given
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.temperature.Celsius
import coulomb.units.temperature.Temperature.withTemperature
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.math.Angle
import lucuma.core.math.units.MetersPerSecond
import munit.CatsEffectSuite
import observe.common.test.observationId
import observe.model.dhs.ImageFileId
import observe.model.enums.KeywordName
import observe.server.EpicsHealth
import observe.server.keywords.DhsInstrument
import observe.server.keywords.KeywordBag
import observe.server.keywords.KeywordsBundler
import observe.server.keywords.KeywordsClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

class GwsHeaderSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger.impl[IO]

  // Some constant values to simulate GWS
  private val reader: GwsKeywordReader[IO] = new GwsKeywordReader[IO]:
    override def health = EpicsHealth.Good.pure[IO].widen

    override def temperature = 15.0.withTemperature[Celsius].pure[IO]

    override def dewPoint = DewPoint(1.0.withTemperature[Celsius]).pure[IO]

    override def airPressure = 620.0.withUnit[Millibar].toUnit[Bar].pure[IO]

    override def windVelocity = 13.7870992.withUnit[MetersPerSecond].pure[IO]

    override def windDirection = Angle.fromDoubleDegrees(275.0).pure[IO]

    override def humidity = 20.0.pure[IO]

  private def sentKeywords: IO[Map[KeywordName, String]] =
    Ref
      .of[IO, KeywordBag](KeywordBag.empty)
      .flatMap: ref =>
        val client = new KeywordsClient[IO]:
          override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean) =
            ref.set(keywords)

          override def openImage(obsId: observe.model.Observation.Id, id: ImageFileId) = IO.unit

          override def closeImage(id: ImageFileId) = IO.unit

          override def keywordsBundler: KeywordsBundler[IO] = DhsInstrument.kb[IO]("GWS")

        GwsHeader
          .header[IO](client, reader)
          .sendBefore(observationId(1),
                      ImageFileId("fileId"),
                      none,
                      ExecutionEnvironment.Development
          ) *> ref.get.map(_.keywords.map(k => k.name -> k.value).toMap)

  test("GwsHeader writes wind speed in m/s and mph"):
    sentKeywords.map: ks =>
      assertEqualsDouble(ks(KeywordName.WINDSPEE).toDouble, 13.7870992, 1e-7)
      assertEqualsDouble(ks(KeywordName.WINDSPE2).toDouble, 30.84086, 1e-4)

  test("GwsHeader writes pressure in mmHg and pascals"):
    sentKeywords.map: ks =>
      assertEqualsDouble(ks(KeywordName.PRESSURE).toDouble, 465.038, 1e-3)
      assertEqualsDouble(ks(KeywordName.PRESSUR2).toDouble, 62000.0, 1e-6)

  test("GwsHeader writes temperatures in celsius and fahrenheit"):
    sentKeywords.map: ks =>
      assertEqualsDouble(ks(KeywordName.TAMBIENT).toDouble, 15.0, 1e-9)
      assertEqualsDouble(ks(KeywordName.TAMBIEN2).toDouble, 59.0, 1e-9)
      assertEqualsDouble(ks(KeywordName.DEWPOINT).toDouble, 1.0, 1e-9)
      assertEqualsDouble(ks(KeywordName.DEWPOIN2).toDouble, 33.8, 1e-9)

  test("GwsHeader writes wind direction in degrees"):
    sentKeywords.map: ks =>
      assertEqualsDouble(ks(KeywordName.WINDDIRE).toDouble, 275.0, 1e-9)
