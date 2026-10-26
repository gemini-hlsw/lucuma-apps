// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.Eq
import cats.MonadThrow
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.effect.std.Mutex
import cats.effect.syntax.temporal.*
import cats.syntax.all.*
import fs2.RaiseThrowable.*
import fs2.Stream
import mouse.all.booleanSyntaxMouse
import navigate.epics.Channel
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.RemoteChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*

trait GeminiApplyCommand[F[_]] {

  /**
   * Given a pair of apply and CAR records, this function produces a program that will write the
   * command parameters, trigger the apply record and then monitor the apply and CAR record to
   * finally produce a command result. If the command takes longer than the <code>timeout</code>, it
   * will produce an error. The program is contained in a <code>VerifiedEpics</code> that checks the
   * connection to the channels.
   *
   * Several commands can be in progress at the same time on the same apply record, but writing the
   * parameters and triggering the apply record must be atomic with respect to other commands. This
   * is why the parameters are received here: they are written while holding a lock on the apply
   * record, which is released as soon as the apply record acknowledges the trigger.
   * @param timeout
   *   The timeout for running the command
   * @param params
   *   The program that writes the command parameters.
   * @return
   */
  def post(
    timeout: FiniteDuration,
    params:  VerifiedEpics[F, F, Unit]
  ): VerifiedEpics[F, F, ApplyCommandResult]

  /**
   * Clears the apply record if the CAR is not busy. It takes the same lock used by
   * <code>post</code>.
   */
  def clearIfNotBusy: VerifiedEpics[F, F, Unit]
}

object GeminiApplyCommand {

  sealed trait CmdPhase extends Product with Serializable

  case object WaitingBusy extends CmdPhase
  case object WaitingIdle extends CmdPhase

  // Delay reading error messages by this amount after an error is triggered
  private val MessageReadDelay: FiniteDuration = 100.milliseconds

  // Maximum time to wait for the apply record to acknowledge a trigger
  val DefaultAckTimeout: FiniteDuration     = 2.seconds
  // Maximum time to hold the apply record lock while writing parameters and triggering
  val DefaultTriggerTimeout: FiniteDuration = 5.seconds

  /**
   * Failures of the apply record handling that have a message meant for the operator.
   */
  sealed abstract class ApplyCommandException(msg: String) extends Exception(msg)

  final class ApplyTriggerTimeout(applyName: String, timeout: FiniteDuration)
      extends ApplyCommandException(
        s"Apply record $applyName did not accept the command in $timeout (writing parameters or START)"
      )

  final class ApplyAckTimeout(applyName: String, timeout: FiniteDuration)
      extends ApplyCommandException(
        s"Apply record $applyName did not acknowledge the command in $timeout"
      )

  final class ApplyRejected(applyName: String, message: String)
      extends ApplyCommandException(s"Apply record $applyName failed with error: $message")

  final class ApplyTriggeredExternally(applyName: String)
      extends ApplyCommandException(
        s"Apply record $applyName triggered externally while processing."
      )

  final class CarRecordError(carName: String, message: String)
      extends ApplyCommandException(s"CAR record $carName has error: $message")

  final class ApplyLockTimeout(applyName: String, timeout: FiniteDuration)
      extends ApplyCommandException(
        s"Timed out after $timeout waiting for another command on apply record $applyName to be triggered"
      )

  final class GeminiApplyCommandImpl[F[_]: {Dispatcher, Temporal}](
    telltaleChannel: TelltaleChannel[F],
    apply:           ApplyRecord[F],
    car:             CarRecord[F],
    mutex:           Mutex[F],
    ackTimeout:      FiniteDuration = DefaultAckTimeout,
    triggerTimeout:  FiniteDuration = DefaultTriggerTimeout
  ) extends GeminiApplyCommand[F] {

    override def post(
      timeout: FiniteDuration,
      params:  VerifiedEpics[F, F, Unit]
    ): VerifiedEpics[F, F, ApplyCommandResult] = {
      val prelude   = clearUnlocked *> params
      val applyVals = eventStream(telltaleChannel, apply.oval)
      val carVals   = eventStream(telltaleChannel, car.oval)
      val dw        = writeChannel(telltaleChannel, apply.dir)(CadDirective.START.pure[F])
      val msr       =
        readChannel(telltaleChannel, apply.mess).map(Temporal[F].delayBy(_, MessageReadDelay))
      val omsr      =
        readChannel(telltaleChannel, car.omss).map(Temporal[F].delayBy(_, MessageReadDelay))
      val clidr     = readChannel(telltaleChannel, car.clid)
      val avr       = readChannel(telltaleChannel, apply.oval)

      // The command is done in two phases. The first one writes the parameters and triggers the
      // apply record, and runs holding the apply record lock. It ends when apply.VAL acknowledges
      // the trigger, and produces the id of the command. The second one follows the CAR record
      // until the command ends, and does not need the lock.
      //
      // The CAR monitor is created after the lock is released. A CA monitor always starts with the
      // current value, and the CAR stays BUSY for at least a second for each command, so the BUSY
      // of this command, or of another one in progress, cannot be missed.
      val trigger: F[Int] =
        (prelude.run *> applyVals.run.use { acks =>
          for {
            av0 <- avr.run.attempt.map(_.toOption)
            _   <- dw.run
            v   <- awaitAck(acks, av0, msr.run)
          } yield v
        }).timeoutTo(triggerTimeout, new ApplyTriggerTimeout(apply.name, triggerTimeout).raiseError)

      new ChannelTracker[F, F[ApplyCommandResult]] {
        override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] =
          List(prelude.systems,
               applyVals.systems,
               carVals.systems,
               dw.systems,
               msr.systems,
               omsr.systems,
               clidr.systems,
               avr.systems
          ).reduce(merge)

        override val run: F[ApplyCommandResult] =
          Ref.of[F, Boolean](false).flatMap { gotLock =>
            mutex.lock
              .surround(gotLock.set(true) *> trigger)
              .flatMap(v => carVals.run.use(processCommand(v, _, omsr.run, clidr.run)))
              .timeout(timeout)
              .handleErrorWith {
                // A timeout before getting the lock means that the command was waiting for others
                case e: TimeoutException =>
                  gotLock.get.flatMap(
                    _.fold(e, new ApplyLockTimeout(apply.name, timeout)).raiseError
                  )
                case e                   => e.raiseError
              }
          }
      }
    }

    /**
     * Waits for apply.VAL to change from its baseline value, which is the acknowledge of the
     * trigger. Because the apply record lock is held, nobody else can trigger it meanwhile, so the
     * new value is the id of this command, if it is positive. A negative value is an error of the
     * apply record, with the message in apply.MESS, and zero means that it was not triggered by
     * this command.
     */
    private def awaitAck(
      acks: Stream[F, StreamEvent[Int]],
      av0:  Option[Int],
      msr:  F[String]
    ): F[Int] = {
      val noAck = new ApplyAckTimeout(apply.name, ackTimeout)

      removeRepeated(acks, av0, s"Apply record ${apply.name} disconnected")
        .take(1)
        .compile
        .last
        .timeoutTo(ackTimeout, noAck.raiseError)
        .flatMap(_.liftTo[F](noAck))
        .flatMap {
          case v if v > 0 => v.pure[F]
          case v if v < 0 => msr.flatMap(m => new ApplyRejected(apply.name, m).raiseError[F, Int])
          case _          => new ApplyTriggeredExternally(apply.name).raiseError[F, Int]
        }
    }

    /**
     * Follows the CAR record until the command ends. The inputs are:
     *
     * @param v:
     *   Value of apply.VAL after the trigger, the id of the command
     * @param cvs:
     *   Stream of values from car.VAL
     * @param omsr:
     *   Effect to read car.OMSS
     * @param clidr:
     *   Effect to read car.CLID
     *
     * The monitors created by the CA library always give the current value as the first event (if
     * there is a current value). That holds true for the Streams created from those monitors. As
     * there is no baseline for car.VAL, its first event is the current state, which can be BUSY if
     * another command is in progress.
     */
    private def processCommand(
      v:     Int,
      cvs:   Stream[F, StreamEvent[CarState]],
      omsr:  F[String],
      clidr: F[Int]
    ): F[ApplyCommandResult] =
      commandStateMachine(car.name,
                          v,
                          omsr,
                          clidr,
                          removeRepeated(cvs, none, s"CAR record ${car.name} disconnected")
      )

    private def removeRepeated[U: Eq](
      strm:          Stream[F, StreamEvent[U]],
      v0:            Option[U],
      disconnectMsg: String
    ): Stream[F, U] =
      strm
        .flatMap {
          case StreamEvent.ValueChanged(v) => Stream(v)
          case StreamEvent.Disconnected    => Stream.raiseError[F](new Throwable(disconnectMsg))
          case _                           => Stream.empty
        }
        .mapAccumulate(v0) { case (acc, v) => (v.some, acc.forall(_ =!= v).option(v)) }
        .map(_._2)
        .unNone

    private val ClearDelay = 50.milliseconds

    override def clearIfNotBusy: VerifiedEpics[F, F, Unit] = clearUnlocked.locked(mutex)

    private def clearUnlocked: VerifiedEpics[F, F, Unit] =
      readChannel(telltaleChannel, car.oval).flatMap { g =>
        VerifiedEpics.ifF(g.map(_ != CarState.BUSY))(
          writeChannel(telltaleChannel, apply.dir)(
            CadDirective.CLEAR.pure[F]
          ) *> VerifiedEpics.liftF(Temporal[F].sleep(ClearDelay))
        )(
          VerifiedEpics.unit
        )
      }
  }

  /**
   * The state machine that follows the algorithm described in Gemini ICD 1b, section 6.1. The
   * events are the changes of car.VAL. The CAR is shared by all the commands on the apply record,
   * it reports the id of the last command that it received in car.CLID, and a transition of the CAR
   * ends this command only if the CAR has already received it, that is, if CLID is not less than
   * the command id.
   *
   * It is not enough to wait for the CAR to be IDLE, because it can still be IDLE for a short time
   * after the apply record acknowledges the trigger, so the command has to see the CAR going BUSY
   * first. Then, the transition to IDLE completes the command, and PAUSED or ERROR end it too.
   */
  private[acm] def commandStateMachine[F[_]: Concurrent](
    carName: String,
    id:      Int,
    omsr:    F[String],
    clidr:   F[Int],
    cvs:     Stream[F, CarState]
  ): F[ApplyCommandResult] = {
    val noResult = none[ApplyCommandResult].pure[F]

    def ifReceived(r: ApplyCommandResult): F[Option[ApplyCommandResult]] =
      clidr.map(c => (c >= id).option(r))

    cvs
      .mapAccumulate[CmdPhase, F[Option[ApplyCommandResult]]](WaitingBusy) {
        case (phase, CarState.ERROR)      =>
          (phase,
           clidr.flatMap(c =>
             (c >= id).fold(
               omsr.flatMap(msg => new CarRecordError(carName, msg).raiseError),
               noResult
             )
           )
          )
        case (WaitingBusy, CarState.BUSY) => (WaitingIdle, noResult)
        case (WaitingIdle, CarState.IDLE) => (WaitingBusy, ifReceived(ApplyCommandResult.Completed))
        case (_, CarState.PAUSED)         => (WaitingBusy, ifReceived(ApplyCommandResult.Paused))
        case (phase, _)                   => (phase, noResult)
      }
      .evalMap(_._2)
      .unNone
      .take(1)
      .compile
      .lastOrError
  }

  def build[F[_]: {Dispatcher, Temporal}](
    srv:             EpicsService[F],
    telltaleChannel: TelltaleChannel[F],
    applyName:       String,
    carName:         String
  ): Resource[F, GeminiApplyCommand[F]] = for {
    apply <- ApplyRecord.build(srv, applyName)
    car   <- CarRecord.build(srv, carName)
    mutex <- Resource.eval(Mutex[F])
  } yield new GeminiApplyCommandImpl[F](telltaleChannel, apply, car, mutex)

  def smartSetParam[F[_]: MonadThrow, A, B](
    tt:  TelltaleChannel[F],
    st:  Channel[F, A],
    pr:  Channel[F, B],
    cmp: (A, B) => Boolean
  )(value: B): VerifiedEpics[F, F, Unit] = {
    val statusRead = readChannel(tt, st)
    val paramWrite = writeChannel(tt, pr)(value.pure[F])

    statusRead.flatMap(fa => ifF(fa.map(cmp(_, value)))(VerifiedEpics.unit[F, F])(paramWrite))
  }
}
