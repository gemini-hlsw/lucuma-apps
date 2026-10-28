// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import fs2.Stream
import munit.CatsEffectSuite
import navigate.server.ApplyCommandResult

import CarState.*

class GeminiApplyCommandSuite extends CatsEffectSuite {

  // Runs the state machine for the command with the given id. The values of car.CLID are the
  // ones returned in each read, in order, the last one is repeated.
  private def run(id: Int, clids: List[Int], cars: CarState*): IO[ApplyCommandResult] =
    Ref.of[IO, List[Int]](clids).flatMap { r =>
      GeminiApplyCommand.commandStateMachine(
        "car",
        id,
        "boom".pure[IO],
        r.modify {
          case h :: Nil => (List(h), h)
          case h :: t   => (t, h)
          case Nil      => (Nil, 0)
        },
        Stream.emits(cars)
      )
    }

  test("Command completion happy path") {
    run(1, List(1), BUSY, IDLE).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("Command paused") {
    run(1, List(1), BUSY, PAUSED).map(assertEquals(_, ApplyCommandResult.Paused))
  }

  test("Command processing error") {
    run(1, List(1), BUSY, ERROR).attempt.map { r =>
      assertEquals(r.left.map(_.getMessage), Left("CAR record car has error: boom"))
    }
  }

  // BUSY to IDLE with CLID=c completes every command with id <= c, and only those
  test("IDLE with a lower CLID does not complete the command") {
    run(2, List(1, 2), BUSY, IDLE, BUSY, IDLE).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("The CAR already busy with another command completes with the final IDLE") {
    run(2, List(2), BUSY, IDLE).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("An IDLE seen before the CAR goes BUSY does not end the command") {
    // If the first IDLE counted, the command would complete and not fail with the CAR error
    run(1, List(1), IDLE, BUSY, ERROR).attempt.map(r => assert(r.isLeft))
  }

  test("A CAR error of an earlier command does not fail this one") {
    run(2, List(1, 2), BUSY, ERROR, IDLE).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("A CAR error fails every command in flight") {
    (run(1, List(2), BUSY, ERROR).attempt, run(2, List(2), BUSY, ERROR).attempt).parTupled.map {
      case (a, b) =>
        assert(a.isLeft)
        assert(b.isLeft)
    }
  }

}
