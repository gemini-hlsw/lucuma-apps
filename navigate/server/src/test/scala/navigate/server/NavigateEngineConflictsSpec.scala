// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.effect.Deferred
import cats.effect.IO
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import lucuma.core.enums.GuideProbe
import lucuma.core.model.GuideConfig
import lucuma.core.util.TimeSpan
import munit.CatsEffectSuite
import navigate.model.CommandResult
import navigate.server.tcs.TcsSimState
import navigate.server.tcs.TcsSouthControllerSim
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class NavigateEngineConflictsSpec extends CatsEffectSuite {

  private given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("navigate-engine")

  private val period = TimeSpan.unsafeFromMicroseconds(1000000)

  /**
   * A PWFS1 sky command that signals when it starts and then waits to be released with the result
   * it must produce. That keeps it in progress for as long as a test needs.
   */
  private class Gate(
    val started: Deferred[IO, Unit],
    val release: Deferred[IO, Either[Throwable, ApplyCommandResult]]
  ) {
    def awaitStarted: IO[Unit]                                     = started.get.timeout(5.seconds)
    def finish(r: Either[Throwable, ApplyCommandResult]): IO[Unit] = release.complete(r).void
  }

  private class GatedController(state: SignallingRef[IO, TcsSimState], gate: Gate)
      extends TcsSouthControllerSim[IO](state) {
    override def pwfs1Sky(exposureTime: TimeSpan)(guide: GuideConfig): IO[ApplyCommandResult] =
      gate.started.complete(()) *> gate.release.get.rethrow
  }

  private class Env(
    val eng:  NavigateEngine[IO],
    val gate: Gate,
    val sim:  SignallingRef[IO, TcsSimState]
  ) {
    // A PWFS1 sky exposure that stays in progress until the gate is released
    val startSky: IO[IO[CommandResult]] =
      eng.wfsSky(GuideProbe.PWFS1, period).start.flatMap(f => gate.awaitStarted.as(f.joinWithNever))

    val pwfs1Integrating: IO[Boolean] = sim.get.map(_.guide.p1Integrating)
  }

  private def withGatedEngine(f: Env => IO[Unit]): IO[Unit] =
    for {
      started <- Deferred[IO, Unit]
      release <- Deferred[IO, Either[Throwable, ApplyCommandResult]]
      gate     = new Gate(started, release)
      sim     <- SignallingRef.of[IO, TcsSimState](TcsSimState.default)
      eng     <- NavigateEngineSpec.buildEngineWith[IO](new GatedController(sim, gate))
      _       <- eng.eventStream.compile.drain.background.use(_ => f(new Env(eng, gate, sim)))
    } yield ()

  test("A conflicting command is rejected while another is in progress, and does not run") {
    withGatedEngine { env =>
      for {
        sky <- env.startSky
        r   <- env.eng.pwfs1Observe(period)
        _    = r match {
                 case CommandResult.CommandFailure(msg) =>
                   assert(msg.contains("in progress"), msg)
                   assert(msg.contains("PWFS1"), msg)
                 case other                             => fail(s"Expected a failure, got $other")
               }
        _   <- env.pwfs1Integrating.map(assertEquals(_, false))
        _   <- env.gate.finish(ApplyCommandResult.Completed.asRight)
        _   <- sky.map(assertEquals(_, CommandResult.CommandSuccess))
      } yield ()
    }
  }

  test("Commands that do not conflict run while another is in progress") {
    withGatedEngine { env =>
      for {
        sky <- env.startSky
        r1  <- env.eng.pwfs2Observe(period)
        r2  <- env.eng.mcsFollow(true)
        _    = assertEquals(r1, CommandResult.CommandSuccess)
        _    = assertEquals(r2, CommandResult.CommandSuccess)
        _   <- env.gate.finish(ApplyCommandResult.Completed.asRight)
        _   <- sky
      } yield ()
    }
  }

  // The command is issued as soon as the first one returns its result, so this also checks that the
  // command is no longer in progress by the time its result is delivered.
  List(
    ("completes", ApplyCommandResult.Completed.asRight[Throwable], CommandResult.CommandSuccess),
    ("is paused", ApplyCommandResult.Paused.asRight[Throwable], CommandResult.CommandPaused),
    ("fails",
     new RuntimeException("boom").asLeft[ApplyCommandResult],
     CommandResult.CommandFailure(
       "Command failed with error type java.lang.RuntimeException, message: boom"
     )
    )
  ).foreach { (name, ending, expected) =>
    test(s"A conflicting command is accepted right after the one in progress $name") {
      withGatedEngine { env =>
        for {
          sky <- env.startSky
          _   <- env.gate.finish(ending)
          r1  <- sky
          _    = assertEquals(r1, expected)
          r2  <- env.eng.pwfs1Observe(period)
          _    = assertEquals(r2, CommandResult.CommandSuccess)
          _   <- env.pwfs1Integrating.map(assertEquals(_, true))
        } yield ()
      }
    }
  }

}
