// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.Concurrent
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.Mutex
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import munit.CatsEffectSuite
import navigate.epics.Channel
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult
import navigate.server.acm.ParameterList.*
import org.epics.ca.AccessRights
import org.epics.ca.ConnectionState

import scala.concurrent.duration.*

/**
 * Checks that several commands posted on the same apply record do not collide. The EPICS channels
 * are replaced by in-memory fakes that log every put, and the tests run under TestControl, so the
 * interleavings are forced and deterministic instead of being left to the scheduler.
 */
class GeminiApplyCommandConcurrencySuite extends CatsEffectSuite {
  import GeminiApplyCommandConcurrencySuite.*

  private val Timeout = 5.seconds

  test("Concurrent commands write their parameters and START without interleaving") {
    TestControl.executeEmbed(
      for {
        gate         <- Deferred[IO, Unit]
        // Command A is held in the middle of its parameter writes, B is posted meanwhile
        r            <- withEnv(p2Hook = v => if (v === "A") gate.get else IO.unit) { e =>
                          for {
                            fa  <- e.command("A", Timeout).start
                            _   <- IO.sleep(500.millis)
                            fb  <- e.command("B", Timeout).start
                            _   <- IO.sleep(1.second)
                            _   <- gate.complete(())
                            ra  <- fa.joinWithNever
                            rb  <- fb.joinWithNever
                            log <- e.log.get
                          } yield (ra, rb, log)
                        }
        (ra, rb, log) = r
        _             = assertEquals(ra, ApplyCommandResult.Completed)
        _             = assertEquals(rb, ApplyCommandResult.Completed)
        _             = blocks(log).foreach { b =>
                          assertEquals(b.map(_._2).distinct.length, 1, s"Mixed parameters before a START: $b")
                          assertEquals(b.length, 2, s"Unexpected parameter writes before a START: $b")
                        }
        _             = assertEquals(blocks(log).length, 2)
      } yield ()
    )
  }

  test(
    "A command stalled while writing parameters holds back the others, and cancelling releases them"
  ) {
    TestControl.executeEmbed(
      withEnv(p2Hook = v => if (v === "A") IO.never else IO.unit) { e =>
        for {
          fa   <- e.command("A", Timeout).start
          _    <- IO.sleep(500.millis)
          fb   <- e.command("B", Timeout).start
          _    <- IO.sleep(1.second)
          log1 <- e.log.get
          _     = assert(log1.forall(_._2 =!= "B"), s"B wrote while A was stalled: $log1")
          _    <- fa.cancel
          rb   <- fb.joinWithNever
          log2 <- e.log.get
          _     = assertEquals(rb, ApplyCommandResult.Completed)
          _     = assertEquals(log2.filter(_._2 === "B").map(_._1).sorted, List("p1", "p2").toVector)
          _     = assertEquals(log2.last, ("dir", "START"))
        } yield ()
      }
    )
  }

  test("A command that cannot write its parameters fails and releases the lock for the others") {
    TestControl.executeEmbed(
      withEnv(p2Hook = v => if (v === "A") IO.never else IO.unit) { e =>
        for {
          fa <- e.command("A", 30.seconds).attempt.start
          _  <- IO.sleep(500.millis)
          fb <- e.command("B", 30.seconds).start
          ra <- fa.joinWithNever
          rb <- fb.joinWithNever
        } yield {
          assertEquals(
            ra.left.map(_.getMessage),
            Left(
              s"Apply record apply did not accept the command in ${GeminiApplyCommand.DefaultTriggerTimeout} (writing parameters or START)"
            )
          )
          assert(ra.left.exists(_.isInstanceOf[GeminiApplyCommand.ApplyTriggerTimeout]))
          assertEquals(rb, ApplyCommandResult.Completed)
        }
      }
    )
  }

  test("A command whose START is not acknowledged fails with the apply record name") {
    TestControl.executeEmbed(
      withEnv(p2Hook = _ => IO.unit, iocAcks = false) { e =>
        e.command("A", 30.seconds).attempt.map { r =>
          assertEquals(
            r.left.map(_.getMessage),
            Left(
              s"Apply record apply did not acknowledge the command in ${GeminiApplyCommand.DefaultAckTimeout}"
            )
          )
        }
      }
    )
  }

  test("A START rejected by the apply record fails with its message, without waiting") {
    TestControl.executeEmbed(
      withEnv(p2Hook = _ => IO.unit, rejectWith = Some((-2, "bad value"))) { e =>
        for {
          t0 <- IO.monotonic
          r  <- e.command("A", 30.seconds).attempt
          t1 <- IO.monotonic
        } yield {
          assert(r.left.exists(_.isInstanceOf[GeminiApplyCommand.ApplyRejected]), s"$r")
          assertEquals(r.left.map(_.getMessage),
                       Left("Apply record apply failed with error: bad value")
          )
          assert(t1 - t0 < GeminiApplyCommand.DefaultAckTimeout, s"Took ${t1 - t0}")
        }
      }
    )
  }

  test("An apply.VAL of zero after the START means it was triggered externally") {
    TestControl.executeEmbed(
      withEnv(p2Hook = _ => IO.unit, rejectWith = Some((0, "")), apply0 = 1) { e =>
        e.command("A", 30.seconds).attempt.map { r =>
          assertEquals(r.left.map(_.getMessage),
                       Left("Apply record apply triggered externally while processing.")
          )
        }
      }
    )
  }

  test("A command queued behind a stalled one reports that it was waiting for the lock") {
    TestControl.executeEmbed(
      withEnv(p2Hook = v => if (v === "A") IO.never else IO.unit, triggerTimeout = 20.seconds) { e =>
        for {
          fa <- e.command("A", 30.seconds).attempt.start
          _  <- IO.sleep(500.millis)
          rb <- e.command("B", 3.seconds).attempt
          _  <- fa.cancel
        } yield {
          assert(rb.left.exists(_.isInstanceOf[GeminiApplyCommand.ApplyLockTimeout]), s"$rb")
          assertEquals(
            rb.left.map(_.getMessage),
            Left(
              "Timed out after 3 seconds waiting for another command on apply record apply to be triggered"
            )
          )
        }
      }
    )
  }

  test("A command posted while the CAR is already BUSY completes with the final IDLE") {
    TestControl.executeEmbed(
      withEnv(p2Hook = _ => IO.unit, car0 = CarState.BUSY, apply0 = 1, clid0 = 1) { e =>
        for {
          fb <- e.command("B", Timeout).start
          _  <- IO.sleep(200.millis)
          // The command that was in flight finishes. The START of B raised CLID to 2.
          _  <- e.carVal.set(CarState.IDLE)
          rb <- fb.joinWithNever
        } yield assertEquals(rb, ApplyCommandResult.Completed)
      }
    )
  }

}

object GeminiApplyCommandConcurrencySuite {

  type Log = Vector[(String, String)]

  /** Splits the log at every START. Each block holds the parameter writes preceding a START. */
  def blocks(log: Log): List[Log] =
    log
      .filterNot(_ === ("dir" -> "CLEAR"))
      .foldLeft((List.empty[Log], Vector.empty[(String, String)])) {
        case ((done, cur), ("dir", "START")) => (done :+ cur, Vector.empty)
        case ((done, cur), e)                => (done, cur :+ e)
      }
      ._1

  final class FakeChannel[A](
    name:  String,
    val v: SignallingRef[IO, A],
    onPut: A => IO[Unit]
  ) extends Channel[IO, A] {
    override val get: IO[A]                                        = v.get
    override def get(timeout:       FiniteDuration): IO[A]         = v.get
    override val getOption: IO[Option[A]]                          = v.get.map(_.some)
    override def getOption(timeout: FiniteDuration): IO[Option[A]] = getOption
    override def put(a:             A): IO[Unit]                   = onPut(a) *> v.set(a)

    override def valueStream(using Dispatcher[IO]): Resource[IO, Stream[IO, A]]            =
      Resource.pure(v.discrete)
    override def connectionStream(using Dispatcher[IO]): Resource[IO, Stream[IO, Boolean]] =
      Resource.pure(Stream.emit(true) ++ Stream.never[IO])
    override def eventStream(using
      Dispatcher[IO],
      Concurrent[IO]
    ): Resource[IO, Stream[IO, StreamEvent[A]]]                                            =
      Resource.pure(v.discrete.map(StreamEvent.ValueChanged(_)))

    override def connect: IO[Unit]                          = IO.unit
    override def connect(timeout: FiniteDuration): IO[Unit] = IO.unit
    override def disconnect: IO[Unit]                       = IO.unit
    override def getName: String                            = name
    override def getConnectionState: IO[ConnectionState]    = IO.pure(ConnectionState.CONNECTED)
    override def getAccessRights: IO[AccessRights]          = IO.pure(AccessRights.READ_WRITE)
  }

  final case class Env(
    cmd:    GeminiApplyCommand[IO],
    log:    Ref[IO, Log],
    tt:     TelltaleChannel[IO],
    p1:     Channel[IO, String],
    p2:     Channel[IO, String],
    carVal: SignallingRef[IO, CarState]
  ) {

    /** The way the TCS builds a command: the parameters are handed to post. */
    def command(value: String, timeout: FiniteDuration): IO[ApplyCommandResult] = {
      val params: ParameterList.ParameterList[IO] = List(
        writeChannel(tt, p1)(value.pure[IO]),
        writeChannel(tt, p2)(value.pure[IO])
      )
      cmd.post(timeout, params.compile).run
    }
  }

  /**
   * A minimal IOC. A START on the apply record bumps apply.VAL and car.CLID. If the CAR was IDLE it
   * also runs a short BUSY-IDLE cycle. If it was already BUSY, whoever set it BUSY is responsible
   * for setting it back to IDLE.
   */
  def withEnv[A](
    p2Hook:         String => IO[Unit],
    car0:           CarState = CarState.IDLE,
    apply0:         Int = 0,
    clid0:          Int = 0,
    iocAcks:        Boolean = true,
    rejectWith:     Option[(Int, String)] = None,
    ackTimeout:     FiniteDuration = GeminiApplyCommand.DefaultAckTimeout,
    triggerTimeout: FiniteDuration = GeminiApplyCommand.DefaultTriggerTimeout
  )(f: Env => IO[A]): IO[A] =
    Dispatcher.parallel[IO].use { d =>
      given Dispatcher[IO] = d
      for {
        log      <- Ref.of[IO, Log](Vector.empty)
        applyVal <- SignallingRef[IO, Int](apply0)
        clid     <- SignallingRef[IO, Int](clid0)
        carVal   <- SignallingRef[IO, CarState](car0)
        dirRef   <- SignallingRef[IO, CadDirective](CadDirective.CLEAR)
        messRef  <- SignallingRef[IO, String]("")
        logPut    = (n: String) => (s: String) => log.update(_ :+ (n -> s))
        dir       = new FakeChannel[CadDirective](
                      "dir",
                      dirRef,
                      {
                        case CadDirective.START if !iocAcks             => logPut("dir")("START")
                        case CadDirective.START if rejectWith.isDefined =>
                          logPut("dir")("START") *>
                            rejectWith.traverse_ { case (code, msg) =>
                              messRef.set(msg) *> applyVal.set(code)
                            }
                        case CadDirective.START                         =>
                          logPut("dir")("START") *>
                            applyVal.updateAndGet(_ + 1).flatMap(clid.set) *>
                            carVal.get.flatMap {
                              case CarState.IDLE =>
                                (carVal.set(CarState.BUSY) *> IO.sleep(10.millis) *>
                                  carVal.set(CarState.IDLE)).start.void
                              case _             => IO.unit
                            }
                        case x                                          => logPut("dir")(x.toString.toUpperCase)
                      }
                    )
        p1       <- SignallingRef[IO, String]("").map(new FakeChannel[String]("p1", _, logPut("p1")))
        p2       <- SignallingRef[IO, String]("")
                      .map(new FakeChannel[String]("p2", _, s => p2Hook(s) *> logPut("p2")(s)))
        mess      = new FakeChannel[String]("mess", messRef, _ => IO.unit)
        omss     <- SignallingRef[IO, String]("").map(new FakeChannel[String]("omss", _, _ => IO.unit))
        apply     = ApplyRecord[IO]("apply",
                                    dir,
                                    new FakeChannel[Int]("apply.VAL", applyVal, _ => IO.unit),
                                    mess
                    )
        car       = CarRecord[IO]("car",
                                  new FakeChannel[Int]("car.CLID", clid, _ => IO.unit),
                                  new FakeChannel[CarState]("car.VAL", carVal, _ => IO.unit),
                                  omss
                    )
        tt        = TelltaleChannel[IO]("test", dir)
        mutex    <- Mutex[IO]
        r        <- f(
                      Env(new GeminiApplyCommand.GeminiApplyCommandImpl[IO](tt,
                                                                            apply,
                                                                            car,
                                                                            mutex,
                                                                            ackTimeout,
                                                                            triggerTimeout
                          ),
                          log,
                          tt,
                          p1,
                          p2,
                          carVal
                      )
                    )
      } yield r
    }

}
