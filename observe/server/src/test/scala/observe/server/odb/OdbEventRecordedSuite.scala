// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.data.Ior
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.ResponseException
import clue.model.GraphQLError
import clue.model.GraphQLResponse
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

class OdbEventRecordedSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger.impl[IO]

  private val errors: NonEmptyList[GraphQLError] =
    NonEmptyList.one(GraphQLError("something went wrong"))

  private def check(response: GraphQLResponse[String]): IO[Either[Throwable, Unit]] =
    OdbCommandsImpl.checkEventRecorded[IO, String]("step EndStep")(response).attempt

  test("a response with data means the event was recorded"):
    check(GraphQLResponse(Ior.right("recorded"))).map(r => assert(r.isRight))

  test("a response without data is a failure"):
    check(GraphQLResponse(Ior.left(errors))).map:
      case Left(ResponseException(es, data)) =>
        assertEquals(es, errors)
        assertEquals(data, none)
      case other                             =>
        fail(s"expected a ResponseException, got $other")

  test("errors alongside data don't fail the event"):
    check(GraphQLResponse(Ior.both(errors, "recorded"))).map(r => assert(r.isRight))

  test("an unrecorded event surfaces on the next flush"):
    OdbEventSender[IO].use: sender =>
      val obsId = lucuma.core.model.Observation.Id.fromLong(1).get
      for
        _      <- sender.submit(
                    obsId,
                    "step EndStep",
                    OdbCommandsImpl.checkEventRecorded[IO, String]("step EndStep"):
                      GraphQLResponse(Ior.left(errors))
                  )
        result <- sender.flush(obsId).attempt
      yield assert(result.isLeft, "expected the unrecorded event to fail the flush")
