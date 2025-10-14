// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import munit.CatsEffectSuite
import navigate.server.ApplyCommandResult

import GeminiApplyCommand.{ApplyValChange, CarValChange, Event}

class GeminiApplyCommandSuite extends CatsEffectSuite {

  test("Command completion happy path") {
    val seq: List[Event] = List(
      ApplyValChange(1),
      CarValChange(CarState.BUSY),
      CarValChange(CarState.IDLE)
    )

    GeminiApplyCommand
      .commandStateMachine("dummy",
                           "dummyC",
                           "".pure[IO],
                           "".pure[IO],
                           1.pure[IO],
                           Stream.emits(seq)
      )
      .map { r =>
        assertEquals(r, ApplyCommandResult.Completed)
      }
  }

  test("Command trigger error") {
    val seq: List[Event] = List(
      ApplyValChange(-1),
      CarValChange(CarState.BUSY),
      CarValChange(CarState.IDLE)
    )

    GeminiApplyCommand
      .commandStateMachine("dummy",
                           "dummyC",
                           "".pure[IO],
                           "".pure[IO],
                           1.pure[IO],
                           Stream.emits(seq)
      )
      .attempt
      .map { r =>
        assert(r.isLeft)
      }
  }

  test("Command processing error") {
    val seq: List[Event] = List(
      ApplyValChange(1),
      CarValChange(CarState.BUSY),
      CarValChange(CarState.ERROR)
    )

    GeminiApplyCommand
      .commandStateMachine("dummy",
                           "dummyC",
                           "".pure[IO],
                           "".pure[IO],
                           1.pure[IO],
                           Stream.emits(seq)
      )
      .attempt
      .map { r =>
        assert(r.isLeft)
      }
  }

}
