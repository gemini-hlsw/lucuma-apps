// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.effect.Async
import cats.effect.kernel.Temporal
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
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Gemini Data service client
 */
sealed trait GdsClient[F[_]] extends Http4sClientDsl[F]:

  /**
   * Set the keywords for an image
   */
  def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit]

  def openObservation(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag): F[Unit]

  def closeObservation(id: ImageFileId): F[Unit]

  def abortObservation(id: ImageFileId): F[Unit]

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

  /**
   * Client for testing always returns ok
   */
  def alwaysOkClient[F[_]: Async]: Client[F] =
    val service = HttpRoutes.of[F] { case _ =>
      Response[F](Status.Ok).withEntity("Success").pure[F]
    }
    Client.fromHttpApp(service.orNotFound)

  def loggingClient[F[_]: Logger](name: String) =
    new GdsClient[F]:
      override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
        overrideLogMessage(name, "setKeywords")

      override def openObservation(
        obsId: Observation.Id,
        id:    ImageFileId,
        ks:    KeywordBag
      ): F[Unit] =
        overrideLogMessage(name, "openObservation")

      override def closeObservation(id: ImageFileId): F[Unit] =
        overrideLogMessage(name, "closeObservation")

      override def abortObservation(id: ImageFileId): F[Unit] =
        overrideLogMessage(name, "abortObservation")

  object json:
    def apply[F[_]: Temporal](base: Client[F], gdsUri: Uri): GdsClient[F] =
      new GdsClient[F] {

        private val client = makeClient(base)

        /**
         * Set the keywords for an image
         */
        override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
          makeRequest("keywords", KeywordRequest(id, ks).asJson)

        override def openObservation(
          obsId: Observation.Id,
          id:    ImageFileId,
          ks:    KeywordBag
        ): F[Unit] =
          makeRequest("open-observation", OpenObservationRequest(obsId, id, ks).asJson)

        override def closeObservation(id: ImageFileId): F[Unit] =
          makeRequest("close-observation", IdRequest(id).asJson)

        override def abortObservation(id: ImageFileId): F[Unit] =
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

