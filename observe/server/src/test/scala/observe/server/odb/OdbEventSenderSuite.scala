// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import java.util.UUID
import scala.concurrent.duration.*

class OdbEventSenderSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger.impl[IO]

  private val obs1: Observation.Id = Observation.Id.fromLong(1).get
  private val obs2: Observation.Id = Observation.Id.fromLong(2).get

  private val step1: Step.Id =
    Step.Id.fromUuid(UUID.fromString("00000000-0000-0000-0000-000000000001"))
  private val step2: Step.Id =
    Step.Id.fromUuid(UUID.fromString("00000000-0000-0000-0000-000000000002"))

  private val sender: Resource[IO, OdbEventSender[IO]] = OdbEventSender[IO]

  // Records which sends completed, each one taking `delay` to be acknowledged.
  private def recorder: IO[(Ref[IO, List[String]], (String, FiniteDuration) => IO[Unit])] =
    Ref
      .of[IO, List[String]](List.empty)
      .map: ref =>
        (ref, (name, delay) => IO.sleep(delay) >> ref.update(name :: _))

  test("submit returns without waiting for the acknowledgement"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- s.submit(obs1, "slow", send("slow", 1.hour))
            now <- IO.monotonic
            ack <- recorded.get
          yield
            assertEquals(now, 0.nanos)
            assertEquals(ack, List.empty)

  test("a step's events are sent concurrently, so a flush costs one round trip"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- (1 to 11).toList.traverse_(i =>
                     s.submit(obs1, s"event$i", send(s"event$i", 1.second))
                   )
            _   <- s.flush(obs1)
            now <- IO.monotonic
            r   <- recorded.get
          yield
            assertEquals(r.size, 11)
            // Serially this would be 11 seconds.
            assertEquals(now, 1.second)

  test("flush awaits the slowest event in flight"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- s.submit(obs1, "quick", send("quick", 1.second))
            _   <- s.submit(obs1, "slow", send("slow", 5.seconds))
            _   <- s.flush(obs1)
            now <- IO.monotonic
            r   <- recorded.get
          yield
            assertEquals(r.size, 2)
            assertEquals(now, 5.seconds)

  test("flush of an observation ignores the events of another one"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (_, send) =>
          for
            _   <- s.submit(obs2, "slow", send("slow", 1.hour))
            _   <- s.submit(obs1, "quick", send("quick", 1.second))
            _   <- s.flush(obs1)
            now <- IO.monotonic
          yield assertEquals(now, 1.second)

  test("a second flush after new events awaits those too"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- s.submit(obs1, "first", send("first", 1.second))
            _   <- s.flush(obs1)
            _   <- s.submit(obs1, "second", send("second", 2.seconds))
            _   <- s.flush(obs1)
            now <- IO.monotonic
            r   <- recorded.get
          yield
            assertEquals(r.size, 2)
            assertEquals(now, 3.seconds)

  test("two observations run their steps concurrently"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          def step(obs: Observation.Id, tag: String): IO[Unit] =
            (1 to 4).toList
              .traverse_(i => s.submit(obs, s"$tag$i", send(s"$tag$i", 1.second))) >>
              s.flush(obs)

          for
            _   <- (step(obs1, "a"), step(obs2, "b")).parTupled
            now <- IO.monotonic
            r   <- recorded.get
          yield
            assertEquals(r.size, 8)
            // Everything overlaps: one round trip for both observations together.
            assertEquals(now, 1.second)

  test("flush raises a failed send, and doesn't report it twice"):
    TestControl.executeEmbed:
      sender.use: s =>
        for
          _      <- s.submit(obs1, "boom", IO.raiseError(new RuntimeException("boom")))
          failed <- s.flush(obs1).attempt
          _      <- IO(assert(failed.isLeft, "expected the failure to be raised at flush"))
          again  <- s.flush(obs1).attempt
        yield assert(again.isRight, "expected the failure to be reported only once")

  test("a failed send doesn't prevent the others"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _ <- s.submit(obs1, "boom", IO.raiseError(new RuntimeException("boom")))
            _ <- s.submit(obs1, "after", send("after", 1.second))
            _ <- s.flush(obs1).attempt
            r <- recorded.get
          yield assertEquals(r, List("after"))

  test("a failure in one observation is not reported to another"):
    TestControl.executeEmbed:
      sender.use: s =>
        for
          _   <- s.submit(obs1, "boom", IO.raiseError(new RuntimeException("boom")))
          _   <- s.submit(obs2, "fine", IO.unit)
          ok  <- s.flush(obs2).attempt
          _   <- IO(assert(ok.isRight, "obs2 should not see obs1's failure"))
          bad <- s.flush(obs1).attempt
        yield assert(bad.isLeft, "obs1 should still report its own failure")

  test("awaitStepRecorded waits for that step's event only"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (_, send) =>
          for
            _   <- s.submitStepEvent(obs1, step1, "StartStep", send("start", 1.second))
            _   <- s.submit(obs1, "unrelated", send("unrelated", 1.hour))
            _   <- s.awaitStepRecorded(obs1, step1)
            now <- IO.monotonic
          yield assertEquals(now, 1.second)

  test("awaitStepRecorded is released by a failed step event rather than hanging"):
    TestControl.executeEmbed:
      sender.use: s =>
        for
          _   <-
            s.submitStepEvent(obs1, step1, "StartStep", IO.raiseError(new RuntimeException("no")))
          _   <- s.awaitStepRecorded(obs1, step1)
          now <- IO.monotonic
        yield assertEquals(now, 0.nanos)

  test("awaitStepRecorded returns immediately for a step with no submitted event"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (_, send) =>
          for
            _   <- s.submitStepEvent(obs1, step1, "StartStep", send("start", 1.hour))
            _   <- s.awaitStepRecorded(obs1, step2)
            now <- IO.monotonic
          yield assertEquals(now, 0.nanos)

  test("forgetStep drops the step's marker"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (_, send) =>
          for
            _   <- s.submitStepEvent(obs1, step1, "StartStep", send("start", 1.hour))
            _   <- s.forgetStep(obs1, step1)
            _   <- s.awaitStepRecorded(obs1, step1)
            now <- IO.monotonic
          yield assertEquals(now, 0.nanos)

  test("a step event does not hold up the other events of its step"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- s.submitStepEvent(obs1, step1, "StartStep", send("start", 1.second))
            _   <- (1 to 10).toList.traverse_(i =>
                     s.submit(obs1, s"event$i", send(s"event$i", 1.second))
                   )
            _   <- s.flush(obs1)
            now <- IO.monotonic
            r   <- recorded.get
          yield
            assertEquals(r.size, 11)
            // The marker gates recordDataset, not the other events.
            assertEquals(now, 1.second)

  test("flush of an observation without events returns immediately"):
    TestControl.executeEmbed:
      sender.use(_.flush(obs1)) >> IO.monotonic.map(assertEquals(_, 0.nanos))
