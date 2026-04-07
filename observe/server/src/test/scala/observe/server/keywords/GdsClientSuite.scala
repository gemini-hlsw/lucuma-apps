// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.effect.*
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.refined.*
import munit.CatsEffectSuite
import observe.model.dhs.*
import observe.model.enums.KeywordName
import observe.server.ObserveFailure
import observe.server.keywords.GdsClient.json.IdRequest
import observe.server.keywords.GdsClient.json.KeywordRequest
import observe.server.keywords.GdsClient.json.OpenObservationRequest
import org.http4s.*
import org.http4s.client.Client
import org.http4s.syntax.all.*

class GdsClientSuite extends munit.CatsEffectSuite:
  val uri: Uri    = uri"http://localhost:8888"
  val badStatuses = List(Status.BadRequest, Status.NotFound, Status.RequestTimeout)
  val obsId       = Observation.Id(1L.refined)
  val id          = ImageFileId("label")

  test("openObservation should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    val ks     =
      KeywordBag(BooleanKeyword(KeywordName.SSA: KeywordName, false),
                 DoubleKeyword(KeywordName.OBJECT, 98.76)
      )
    assertIOBoolean(client.openObservation(obsId, id, ks).attempt.map(_.isRight))

  test("openObservation should throw exception if http client returns a bad status"):
    val obsId = Observation.Id(1L.refined)
    val ks    =
      KeywordBag(BooleanKeyword(KeywordName.SSA, false), DoubleKeyword(KeywordName.OBJECT, 98.76))
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)

      interceptIO[ObserveFailure.GdsException](client.openObservation(obsId, id, ks))

  test("closeObservation should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    assertIO_(client.closeObservation(id))

  test("closeObservation should throw exception if http client returns a bad status"):
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)
      interceptIO[ObserveFailure.GdsException](client.closeObservation(id))

  test("abortObservation should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    assertIO_(client.abortObservation(id))

  test("abortObservation should throw exception if http client returns a bad status"):
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)
      interceptIO[ObserveFailure.GdsException](client.abortObservation(id))

  test("setKeywords should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    val ks     = KeywordBag(StringKeyword(KeywordName.INSTRUMENT, "The INSTR."),
                        Int32Keyword(KeywordName.OBJECT, 123)
    )
    assertIO_(client.setKeywords(id, ks))

  test("setKeywords should throw exception if http client returns a bad status"):
    val ks = KeywordBag(StringKeyword(KeywordName.INSTRUMENT, "The INSTR."),
                        Int32Keyword(KeywordName.OBJECT, 123)
    )
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)
      interceptIO[ObserveFailure.GdsException](client.setKeywords(id, ks))

  test("OpenObservationRequests should encode to JSON properly"):
    val ks =
      KeywordBag(BooleanKeyword(KeywordName.SSA, false), DoubleKeyword(KeywordName.OBJECT, 98.76))

    val json = OpenObservationRequest(obsId, id, ks).asJson
    assertEquals(json, openObsExpectedJson)

  test("IdRequests should encode to JSON properly"):

    val json = IdRequest(id).asJson
    assertEquals(json, idRequestExpectedJson)

  test("KeywordRequests should encode to JSON properly"):
    val ks = KeywordBag(StringKeyword(KeywordName.INSTRUMENT, "The INSTR."),
                        Int32Keyword(KeywordName.OBJECT, 123)
    )

    val json = KeywordRequest(id, ks).asJson
    assertEquals(json, keywordExpectedJson)

  test("filter non-ASCII characters from string keywords"):
    val keyword = StringKeyword(KeywordName.OBJECT, "Café\u0001Test")
    assertEquals(keyword.stringValue, "CafTest")

  def httpClient(status: Status, body: String): Client[IO] =
    val service = HttpRoutes.of[IO]: _ =>
      Response[IO](status).withEntity(body).pure[IO]
    Client.fromHttpApp(service.orNotFound)

  val openObsExpectedJson =
    json"""{
      "program_id" : "o-1",
      "data_label" : "label",
      "keywords" : [
        {
          "keyword" : "SSA",
          "value_type" : "BOOLEAN",
          "value" : "false"
        },
        {
          "keyword" : "OBJECT",
          "value_type" : "DOUBLE",
          "value" : "98.76"
        }
      ]
    }"""

  val idRequestExpectedJson =
    json"""{
      "data_label" : "label"
    }"""

  val keywordExpectedJson =
    json"""{
      "data_label" : "label",
      "keywords" : [
        {
          "keyword" : "INSTRUME",
          "value_type" : "STRING",
          "value" : "The INSTR."
        },
        {
          "keyword" : "OBJECT",
          "value_type" : "INT",
          "value" : "123"
        }
      ]
    }"""
