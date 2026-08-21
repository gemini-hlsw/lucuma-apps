// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testkit.TestControl
import lucuma.core.model.Observation
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

class OdbEventSenderSuite extends CatsEffectSuite:

  private given Logger[IO] = NoOpLogger.impl[IO]

  private val obs1: Observation.Id = Observation.Id.fromLong(1).get
  private val obs2: Observation.Id = Observation.Id.fromLong(2).get

  private val sender: Resource[IO, OdbEventSender[IO]] = OdbEventSender[IO]

  // Records the order in which sends complete, each one taking `delay` to be acknowledged.
  private def recorder: IO[(Ref[IO, List[String]], (String, FiniteDuration) => IO[Unit])] =
    Ref.of[IO, List[String]](List.empty).map: ref =>
      (ref, (name, delay) => IO.sleep(delay) >> ref.update(name :: _))

  test("submit returns without waiting for the acknowledgement"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _   <- s.submit(obs1, "slow", send("slow", 1.hour))
            now <- IO.monotonic
            _   <- IO(assertEquals(now, 0.nanos))
            ack <- recorded.get
            _   <- IO(assertEquals(ack, List.empty))
          yield ()

  test("events of an observation are acknowledged in submission order"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            // Decreasing delays: parallel sends would complete in the opposite order.
            _ <- s.submit(obs1, "first", send("first", 3.seconds))
            _ <- s.submit(obs1, "second", send("second", 2.seconds))
            _ <- s.submit(obs1, "third", send("third", 1.second))
            _ <- s.flush(obs1)
            r <- recorded.get
          yield assertEquals(r.reverse, List("first", "second", "third"))

  test("flush awaits all the events submitted so far"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _     <- s.submit(obs1, "one", send("one", 1.second))
            _     <- s.submit(obs1, "two", send("two", 1.second))
            _     <- s.flush(obs1)
            r     <- recorded.get
            after <- IO.monotonic
          yield
            assertEquals(r.size, 2)
            assertEquals(after, 2.seconds)

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

  test("flush raises a failed send, and doesn't report it twice"):
    TestControl.executeEmbed:
      sender.use: s =>
        for
          _      <- s.submit(obs1, "boom", IO.raiseError(new RuntimeException("boom")))
          failed <- s.flush(obs1).attempt
          _      <- IO(assert(failed.isLeft, "expected the failure to be raised at flush"))
          again  <- s.flush(obs1).attempt
        yield assert(again.isRight, "expected the failure to be reported only once")

  test("a failed send doesn't prevent the following ones"):
    TestControl.executeEmbed:
      sender.use: s =>
        recorder.flatMap: (recorded, send) =>
          for
            _ <- s.submit(obs1, "boom", IO.raiseError(new RuntimeException("boom")))
            _ <- s.submit(obs1, "after", send("after", 1.second))
            _ <- s.flush(obs1).attempt
            r <- recorded.get
          yield assertEquals(r, List("after"))

  test("flush of an observation without events returns immediately"):
    TestControl.executeEmbed:
      sender.use(_.flush(obs1)) >> IO.monotonic.map(assertEquals(_, 0.nanos))
