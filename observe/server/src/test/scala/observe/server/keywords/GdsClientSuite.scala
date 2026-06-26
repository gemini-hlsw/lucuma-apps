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
import org.http4s.scalaxml.*
import org.http4s.syntax.all.*

import scala.xml.Elem
import scala.xml.XML

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
    assertIOBoolean(client.openImage(obsId, id, ks).attempt.map(_.isRight))

  test("openObservation should throw exception if http client returns a bad status"):
    val obsId = Observation.Id(1L.refined)
    val ks    =
      KeywordBag(BooleanKeyword(KeywordName.SSA, false), DoubleKeyword(KeywordName.OBJECT, 98.76))
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)

      interceptIO[ObserveFailure.GdsException](client.openImage(obsId, id, ks))

  test("closeImage should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    assertIO_(client.closeImage(id))

  test("closeImage should throw exception if http client returns a bad status"):
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)
      interceptIO[ObserveFailure.GdsException](client.closeImage(id))

  test("abortImage should succeed if http client returns Status.Ok"):
    val client = GdsClient.json(httpClient(Status.Ok, "Success"), uri)
    assertIO_(client.abortImage(id))

  test("abortImage should throw exception if http client returns a bad status"):
    badStatuses.traverse_ : status =>
      val client = GdsClient.json(httpClient(status, "Error"), uri)
      interceptIO[ObserveFailure.GdsException](client.abortImage(id))

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

  // XML-RPC client tests

  val faultResponse: Elem =
    <methodResponse>
      <fault>
        <value>
          <struct>
            <member>
              <name>faultCode</name>
              <value><int>1</int></value>
            </member>
            <member>
              <name>faultString</name>
              <value><string>Something went wrong</string></value>
            </member>
          </struct>
        </value>
      </fault>
    </methodResponse>

  def xmlClient(body: Elem): Client[IO] =
    val service = HttpRoutes.of[IO]: _ =>
      Response[IO](Status.Ok).withEntity(body).pure[IO]
    Client.fromHttpApp(service.orNotFound)

  test("xmlrpc openObservation should succeed if the GDS returns a non-fault response"):
    val client = GdsClient.xmlrpc(GdsClient.xmlrpc.alwaysOkClient[IO], uri)
    val ks     =
      KeywordBag(BooleanKeyword(KeywordName.SSA, false), DoubleKeyword(KeywordName.OBJECT, 98.76))
    assertIO_(client.openImage(obsId, id, ks))

  test("xmlrpc openObservation should fail if the GDS returns a fault"):
    val client = GdsClient.xmlrpc(xmlClient(faultResponse), uri)
    val ks     =
      KeywordBag(BooleanKeyword(KeywordName.SSA, false), DoubleKeyword(KeywordName.OBJECT, 98.76))
    interceptIO[ObserveFailure.GdsXmlError](client.openImage(obsId, id, ks))

  test("xmlrpc setKeywords should succeed if the GDS returns a non-fault response"):
    val client = GdsClient.xmlrpc(GdsClient.xmlrpc.alwaysOkClient[IO], uri)
    val ks     = KeywordBag(StringKeyword(KeywordName.INSTRUMENT, "The INSTR."),
                        Int32Keyword(KeywordName.OBJECT, 123)
    )
    assertIO_(client.setKeywords(id, ks))

  test("xmlrpc setKeywords should fail if the GDS returns a fault"):
    val client = GdsClient.xmlrpc(xmlClient(faultResponse), uri)
    val ks     = KeywordBag(StringKeyword(KeywordName.INSTRUMENT, "The INSTR."),
                        Int32Keyword(KeywordName.OBJECT, 123)
    )
    interceptIO[ObserveFailure.GdsXmlError](client.setKeywords(id, ks))

  test("xmlrpc closeImage should succeed if the GDS returns a non-fault response"):
    val client = GdsClient.xmlrpc(GdsClient.xmlrpc.alwaysOkClient[IO], uri)
    assertIO_(client.closeImage(id))

  test("xmlrpc closeImage should fail if the GDS returns a fault"):
    val client = GdsClient.xmlrpc(xmlClient(faultResponse), uri)
    interceptIO[ObserveFailure.GdsXmlError](client.closeImage(id))

  test("xmlrpc parseError extracts the fault string"):
    assertEquals(GdsClient.xmlrpc.parseError(faultResponse), Left("Something went wrong"))

  test("xmlrpc parseError should reject a real GDS fault response"):
    val xml = XML.load(getClass.getResource("/gds-bad-resp.xml"))
    assert(GdsClient.xmlrpc.parseError(xml).isLeft)

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
