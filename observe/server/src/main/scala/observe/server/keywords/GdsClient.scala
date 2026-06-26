// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.kernel.Temporal
import cats.effect.std.MapRef
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import observe.model.Observation
import observe.model.dhs.ImageFileId
import observe.server.ObserveFailure
import observe.server.overrideLogMessage
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.scalaxml.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*
import scala.xml.Elem

/**
 * Gemini Data service client
 */
sealed trait GdsClient[F[_]] extends Http4sClientDsl[F]:

  /**
   * Set the keywords for an image
   */
  def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit]

  def openImage(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag): F[Unit]

  def closeImage(id: ImageFileId): F[Unit]

  def abortImage(id: ImageFileId): F[Unit]

object GdsClient:
  private def makeClient[F[_]](base: Client[F])(implicit timer: Temporal[F]) = {
    val max             = 2
    var attemptsCounter = 1
    val policy          =
      RetryPolicy[F](attempts =>
        if (attempts >= max) None
        else {
          attemptsCounter = attemptsCounter + 1
          Some(10.milliseconds)
        }
      )
    Retry(policy)(base)
  }

  def loggingClient[F[_]: Logger](name: String) =
    new GdsClient[F]:
      override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
        overrideLogMessage(name, "setKeywords")

      override def openImage(
        obsId: Observation.Id,
        id:    ImageFileId,
        ks:    KeywordBag
      ): F[Unit] =
        overrideLogMessage(name, "openObservation")

      override def closeImage(id: ImageFileId): F[Unit] =
        overrideLogMessage(name, "closeObservation")

      override def abortImage(id: ImageFileId): F[Unit] =
        overrideLogMessage(name, "abortObservation")

  def simulatedClient[F[_]: {Concurrent, Logger}](
    name:        String,
    accumulator: MapRef[F, ImageFileId, Option[KeywordBag]]
  ): GdsClient[F] = new GdsClient[F] {
    override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
      accumulator(id).flatModify { oldKeywordBag =>
        val newKeywordBag: KeywordBag = oldKeywordBag.fold(ks)(_.combine(ks))

        (
          newKeywordBag.some,
          Logger[F].trace:
            s"Simulated GDS $name for file [$id], Accumulating keywords: ${ks.keywords.map(k => s"${k.name} = ${k.value}").mkString(", ")}"
        )
      }

    override def openObservation(
      obsId: Observation.Id,
      id:    ImageFileId,
      ks:    KeywordBag
    ): F[Unit] = Logger[F].debug(s"Simulated GDS $name for file [$id], Opening observation")

    override def closeObservation(id: ImageFileId): F[Unit] =
      accumulator(id).flatModify { kso =>
        val finalKeywords: SortedMap[String, String] = kso
          .map(ks =>
            SortedMap.from(
              ks.keywords.map(k =>
                k.name.name.padTo(8, ' ') -> // FITS Keys are 8 characters long
                  s"${k.value} [${KeywordType.dhsKeywordType(k.keywordType)}]"
              )
            )
          )
          .orEmpty
        (
          none,
          Logger[F].debug(
            s"Simulated GDS $name for file [$id], Closing observation. Final keywords: \n${finalKeywords
                .map { case (k, v) => s"$k: $v" }
                .mkString("\n")}"
          )
        )
      }

    override def abortObservation(id: ImageFileId): F[Unit] =
      Logger[F].debug(s"Simulated GDS $name for file [$id], Aborting observation")
  }

  object json:

    /**
     * Client for testing always returns ok
     */
    def alwaysOkClient[F[_]: Async]: Client[F] =
      val service = HttpRoutes.of[F] { case _ =>
        Response[F](Status.Ok).withEntity("Success").pure[F]
      }
      Client.fromHttpApp(service.orNotFound)

    def apply[F[_]: Temporal](base: Client[F], gdsUri: Uri): GdsClient[F] =
      new GdsClient[F] {

        private val client = makeClient(base)

        /**
         * Set the keywords for an image
         */
        override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
          makeRequest("keywords", KeywordRequest(id, ks).asJson)

        override def openImage(
          obsId: Observation.Id,
          id:    ImageFileId,
          ks:    KeywordBag
        ): F[Unit] =
          // Maybe this should be a no-op
          makeRequest("open-observation", OpenObservationRequest(obsId, id, ks).asJson)

        override def closeImage(id: ImageFileId): F[Unit] =
          makeRequest("close-observation", IdRequest(id).asJson)

        override def abortImage(id: ImageFileId): F[Unit] =
          makeRequest("abort-observation", IdRequest(id).asJson)

        private def makeRequest(path: String, body: Json): F[Unit] = {
          val uri         = gdsUri / path
          val postRequest = POST(body, uri)

          // Do the request
          client
            .expect[String](postRequest)
            .adaptErr { case e => ObserveFailure.GdsException(e, uri) }
            .void
        }
      }

    case class KeywordRequest(id: ImageFileId, ks: KeywordBag)
    case class OpenObservationRequest(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag)
    case class IdRequest(id: ImageFileId)

    given Encoder[InternalKeyword] =
      Encoder.forProduct3("keyword", "value_type", "value")(ikw =>
        (ikw.name.name, KeywordType.gdsKeywordType(ikw.keywordType), ikw.value)
      )

    given Encoder[KeywordRequest] =
      Encoder.forProduct2("data_label", "keywords")(kwr => (kwr.id, kwr.ks.keywords))

    given Encoder[OpenObservationRequest] =
      Encoder.forProduct3("program_id", "data_label", "keywords")(oor =>
        (oor.obsId.show, oor.id.value, oor.ks.keywords)
      )

    given Encoder[IdRequest] =
      Encoder.forProduct1("data_label")(_.id.value)

  object xmlrpc:
    def apply[F[_]: Async](base: Client[F], gdsUri: Uri): GdsClient[F] =
      new GdsClient[F] {

        private val client = makeClient(base)

        /**
         * Set the keywords for an image
         */
        override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
          makeRequest(storeKeywords(id, ks))

        override def openImage(
          obsId: Observation.Id,
          id:    ImageFileId,
          ks:    KeywordBag
        ): F[Unit] =
          makeRequest(openObservationRPC(obsId, id, ks))

        override def closeImage(id: ImageFileId): F[Unit] =
          makeRequest(closeObservationRPC(id))

        override def abortImage(id: ImageFileId): F[Unit] =
          Async[F].unit

        private def makeRequest(xmlRpc: Elem): F[Unit] =
          val postRequest = POST(xmlRpc, gdsUri)
          client
            .expect[Elem](postRequest)
            .map(parseError)
            .ensureOr(v => ObserveFailure.GdsXmlError(v.left.getOrElse(""), gdsUri))(_.isRight)
            .void

        private def storeKeywords(id: ImageFileId, ks: KeywordBag): Elem =
          <methodCall>
            <methodName>HeaderReceiver.storeKeywords</methodName>
            <params>
              <param>
                <value>
                  <string>{id.value}</string>
                </value>
              </param>
              {keywordsParam(ks)}
            </params>
          </methodCall>

        private def openObservationRPC(
          obsId: Observation.Id,
          id:    ImageFileId,
          ks:    KeywordBag
        ): Elem =
          <methodCall>
            <methodName>HeaderReceiver.openObservation</methodName>
            <params>
              <param>
                <value>
                  <string>{obsId.show}</string>
                </value>
              </param>
              <param>
                <value>
                  <string>{id.value}</string>
                </value>
              </param>
              {keywordsParam(ks)}
            </params>
          </methodCall>

        private def closeObservationRPC(id: ImageFileId): Elem =
          <methodCall>
            <methodName>HeaderReceiver.closeObservation</methodName>
            <params>
              <param>
                <value>
                  <string>{id.value}</string>
                </value>
              </param>
            </params>
          </methodCall>

        private def keywordsParam(ks: KeywordBag): Elem =
          <param>
            <value>
              <array>
                <data>
                  {
            ks.keywords.map { k =>
              <value><string>{
                s"${k.name.name},${KeywordType.gdsKeywordType(k.keywordType)},${k.value}"
              }</string></value>
            }
          }
                </data>
              </array>
            </value>
          </param>
      }

    // Parse an XML-RPC fault response, returning the fault string on the left
    def parseError(e: Elem): Either[String, Elem] =
      val v =
        for {
          m <- e \\ "methodResponse" \ "fault" \ "value" \ "struct" \\ "member"
          if (m \ "name").text === "faultString"
        } yield (m \ "value").text.trim
      v.headOption.toLeft(e)

    /**
     * Client for testing always returns ok
     */
    def alwaysOkClient[F[_]: Async]: Client[F] =
      val service = HttpRoutes.of[F]: _ =>
        val response =
          <methodResponse>
            <params>
              <param>
                <value><string>Ok</string></value>
              </param>
            </params>
          </methodResponse>
        Response[F](Status.Ok).withEntity(response).pure[F]
      Client.fromHttpApp(service.orNotFound)
