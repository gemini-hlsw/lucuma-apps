// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.syntax.all.*

import scala.concurrent.duration.*

class UndoEffectRunnerSpec extends munit.CatsEffectSuite:
  private val Timeout: FiniteDuration = 10.seconds

  private val Boom: IO[Unit] = IO.raiseError(new RuntimeException("boom"))

  private case object Rejected extends RuntimeException("rejected")

  private val Reject: IO[Unit] = IO.raiseError(Rejected)

  private val IsRejection: Throwable => Boolean =
    case Rejected => true
    case _        => false

  private def serial(
    onTimeout: IO[Unit] = IO.unit,
    onError:   Throwable => IO[Unit] = _ => IO.unit
  ): IO[UndoEffectRunner[IO]] =
    UndoEffectRunner.serial[IO](Timeout, IsRejection, onError, onTimeout)

  // What happened to the effects of a run, by name.
  private case class Outcome(ran: List[String], rolledBack: List[String], failed: List[String])

  private class Recorder(
    ran:        Ref[IO, List[String]],
    rolledBack: Ref[IO, List[String]],
    failed:     Ref[IO, List[String]]
  ):
    // Submits `fa` as a fiber, giving earlier submissions time to enter the queue first.
    def start(runner: UndoEffectRunner[IO], name: String, fa: IO[Unit]): IO[Unit] =
      runner
        .submit(
          fa >> ran.update(_ :+ name),
          rolledBack.update(_ :+ name),
          failed.update(_ :+ name)
        )
        .attempt
        .start *> IO.sleep(1.millis)

    val outcome: IO[Outcome] = (ran.get, rolledBack.get, failed.get).mapN(Outcome.apply)

  private val recorder: IO[Recorder] =
    (Ref[IO].of(List.empty[String]), Ref[IO].of(List.empty[String]), Ref[IO].of(List.empty[String]))
      .mapN(new Recorder(_, _, _))

  // Runs `first` with B and C waiting behind it.
  private def withTwoWaiting(first: IO[Unit], onTimeout: IO[Unit] = IO.unit): IO[Outcome] =
    TestControl.executeEmbed(
      for
        runner <- serial(onTimeout)
        rec    <- recorder
        _      <- rec.start(runner, "A", IO.sleep(2.seconds) >> first)
        _      <- rec.start(runner, "B", IO.unit)
        _      <- rec.start(runner, "C", IO.unit)
        _      <- IO.sleep(1.minute)
        o      <- rec.outcome
      yield o
    )

  test("serial runs effects in submission order without overlapping"):
    TestControl.executeEmbed(
      for
        runner  <- serial()
        order   <- Ref[IO].of(List.empty[Int])
        running <- Ref[IO].of(0)
        maxSeen <- Ref[IO].of(0)
        effect   = (i: Int) =>
                     running.updateAndGet(_ + 1).flatMap(r => maxSeen.update(_.max(r))) *>
                       IO.sleep((5 - i).seconds) *>
                       order.update(_ :+ i) *>
                       running.update(_ - 1)
        _       <- (0 until 5).toList.traverse_ : i =>
                     runner.submit(effect(i), IO.unit, IO.unit).start *> IO.sleep(1.millis)
        _       <- IO.sleep(1.minute)
        result  <- order.get
        max     <- maxSeen.get
      yield
        assertEquals(result, List(0, 1, 2, 3, 4))
        assertEquals(max, 1)
    )

  test("a rejection rolls back only the rejected effect; the waiting ones still run"):
    withTwoWaiting(Reject).map: outcome =>
      assertEquals(outcome, Outcome(List("B", "C"), List("A"), List("A")))

  test("an error that is not a rejection rolls nothing back; the waiting ones still run"):
    withTwoWaiting(Boom).map: outcome =>
      assertEquals(outcome, Outcome(List("B", "C"), Nil, List("A")))

  test("a timeout rolls nothing back, calls onTimeout once; the waiting ones still run"):
    Ref[IO]
      .of(0)
      .flatMap: timeouts =>
        withTwoWaiting(IO.never, timeouts.update(_ + 1)).flatMap: outcome =>
          timeouts.get.map: t =>
            assertEquals(outcome, Outcome(List("B", "C"), Nil, List("A")))
            assertEquals(t, 1)

  test("a successful effect runs neither rollback nor onFailure"):
    withTwoWaiting(IO.unit).map: outcome =>
      assertEquals(outcome, Outcome(List("A", "B", "C"), Nil, Nil))

  test("onError sees rejections, other errors and timeouts"):
    TestControl.executeEmbed(
      for
        seen   <- Ref[IO].of(List.empty[String])
        runner <- serial(onError = e => seen.update(_ :+ e.getClass.getSimpleName))
        rec    <- recorder
        _      <- rec.start(runner, "rejected", Reject)
        _      <- rec.start(runner, "boom", Boom)
        _      <- rec.start(runner, "hangs", IO.never)
        _      <- rec.start(runner, "ok", IO.unit)
        _      <- IO.sleep(1.minute)
        s      <- seen.get
      yield assertEquals(s.size, 3)
    )

  test("unordered rolls back only on a rejection and runs onFailure on any error"):
    TestControl.executeEmbed(
      for
        runner <- IO.pure(UndoEffectRunner.unordered[IO](IsRejection, _ => IO.unit))
        rec    <- recorder
        _      <- rec.start(runner, "ok", IO.unit)
        _      <- rec.start(runner, "boom", Boom)
        _      <- rec.start(runner, "rejected", Reject)
        o      <- rec.outcome
      yield assertEquals(o, Outcome(List("ok"), List("rejected"), List("boom", "rejected")))
    )

  test("local never rolls back"):
    TestControl.executeEmbed(
      for
        rec <- recorder
        _   <- rec.start(UndoEffectRunner.local[IO], "rejected", Reject)
        o   <- rec.outcome
      yield assertEquals(o, Outcome(Nil, Nil, List("rejected")))
    )
