// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import munit.CatsEffectSuite
import navigate.server.ApplyCommandResult

import CarState.*

class TaskControlCommandSuite extends CatsEffectSuite {

  private def run(events: TaskControlCommand.Event*): IO[ApplyCommandResult] =
    TaskControlCommand.stateMachine(
      "rec",
      "boom".pure[IO],
      Stream.emits[IO, TaskControlCommand.Event](events)
    )

  private def val0: TaskControlCommand.Event            = 0.asLeft
  private def st(s: CarState): TaskControlCommand.Event = s.asRight

  test("Completes when the record goes busy and then idle after acknowledging") {
    run(val0, st(BUSY), st(IDLE)).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("The record can go busy before it acknowledges the directive") {
    run(st(BUSY), val0, st(IDLE)).map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("An idle seen before the record is busy does not end the command") {
    run(val0, st(IDLE), st(IDLE), st(BUSY), st(IDLE))
      .map(assertEquals(_, ApplyCommandResult.Completed))
  }

  test("A value other than zero in VAL is an error") {
    run(1.asLeft).attempt.map { r =>
      assertEquals(r.left.map(_.getMessage), Left("taskControl record rec failed with error: boom"))
    }
  }

  test("The error state ends the command with an error") {
    run(val0, st(BUSY), st(ERROR)).attempt.map(r => assert(r.isLeft))
  }

  test("The error state before the acknowledge is also an error") {
    run(st(ERROR)).attempt.map(r => assert(r.isLeft))
  }
}
