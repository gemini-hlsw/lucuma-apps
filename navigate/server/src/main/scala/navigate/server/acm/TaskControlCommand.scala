// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.effect.std.Mutex
import cats.effect.syntax.temporal.*
import cats.syntax.all.*
import fs2.Stream
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult

import scala.concurrent.duration.*

/**
 * A command executed by a TaskControl or an assemblyControl record, which behave the same way. The
 * parameters are written to the record, the directive is set to START, and the record answers with
 * ACCEPT (zero) or REJECT (non-zero) in VAL. Then it goes BUSY, and the command ends when it goes
 * back to IDLE. This is the same as the ACM library, used in Seqexec, does for TaskControl.
 */
trait TaskControlCommand[F[_]] {

  /**
   * Writes the parameters, starts the record and waits for the command to end. If the command takes
   * longer than the <code>timeout</code>, it will produce an error. The program is contained in a
   * <code>VerifiedEpics</code> that checks the connection to the channels. Commands on the same
   * record are executed one at a time.
   */
  def post(
    timeout: FiniteDuration,
    params:  VerifiedEpics[F, F, Unit]
  ): VerifiedEpics[F, F, ApplyCommandResult]
}

object TaskControlCommand {

  final class TaskControlRejected(recordName: String, message: String)
      extends GeminiApplyCommand.ApplyCommandException(
        s"TaskControl record $recordName failed with error: $message"
      )

  // Delay reading error messages by this amount after an error is triggered
  private val MessageReadDelay: FiniteDuration = 100.milliseconds

  /** What the record tells us: a new VAL or a new BUSY */
  private[acm] type Event = Either[Int, CarState]

  private enum Phase {
    // Waiting for the record to acknowledge the directive. It keeps if BUSY was seen, because it
    // can be seen before VAL.
    case WaitPreset(busySeen: Boolean)
    case WaitStart
    case WaitCompletion
  }

  private enum Step {
    case Continue(phase: Phase)
    case Succeed
    case Fail
  }

  private def step(phase: Phase, event: Event): Step = {
    import Phase.*
    (phase, event) match {
      case (_, Left(v)) if v =!= 0                => Step.Fail
      case (_, Right(CarState.ERROR))             => Step.Fail
      case (WaitPreset(true), Left(_))            => Step.Continue(WaitCompletion)
      case (WaitPreset(false), Left(_))           => Step.Continue(WaitStart)
      case (WaitPreset(_), Right(s))              => Step.Continue(WaitPreset(s === CarState.BUSY))
      case (WaitStart, Right(CarState.BUSY))      => Step.Continue(WaitCompletion)
      case (WaitCompletion, Right(CarState.IDLE)) => Step.Succeed
      case (p, _)                                 => Step.Continue(p)
    }
  }

  /**
   * Follows the record until the command ends, with the algorithm of the ACM library: the record
   * acknowledges the directive by writing 0 to VAL (any other value is an error), then goes BUSY,
   * and the command is done when it goes back to IDLE. Going to ERROR at any time ends the command
   * with an error.
   */
  private[acm] def stateMachine[F[_]: Concurrent](
    recordName: String,
    mess:       F[String],
    events:     Stream[F, Event]
  ): F[ApplyCommandResult] =
    events
      .mapAccumulate[Phase, Step](Phase.WaitPreset(false)) { (phase, event) =>
        val s = step(phase, event)
        (
          s match {
            case Step.Continue(p) => p
            case _                => phase
          },
          s
        )
      }
      .map(_._2)
      .collectFirst {
        case Step.Succeed => true
        case Step.Fail    => false
      }
      .compile
      .lastOrError
      .flatMap {
        case true  => ApplyCommandResult.Completed.pure[F]
        case false =>
          mess
            .flatMap(m => new TaskControlRejected(recordName, m).raiseError[F, ApplyCommandResult])
      }

  private[acm] def values[F[_]: Concurrent, A](
    recordName: String,
    strm:       Stream[F, StreamEvent[A]]
  ): Stream[F, A] =
    strm.flatMap {
      case StreamEvent.ValueChanged(v) => Stream(v)
      case StreamEvent.Disconnected    =>
        Stream.raiseError[F](new Throwable(s"Record $recordName disconnected"))
      case _                           => Stream.empty
    }

  final class TaskControlCommandImpl[F[_]: {Dispatcher, Temporal}](
    telltaleChannel: TelltaleChannel[F],
    record:          TaskControlRecord[F],
    mutex:           Mutex[F]
  ) extends TaskControlCommand[F] {

    override def post(
      timeout: FiniteDuration,
      params:  VerifiedEpics[F, F, Unit]
    ): VerifiedEpics[F, F, ApplyCommandResult] = {
      val vals  = eventStream(telltaleChannel, record.oval)
      val state = eventStream(telltaleChannel, record.state)
      val start = writeChannel(telltaleChannel, record.dir)(CadDirective.START.pure[F])
      val mess  =
        readChannel(telltaleChannel, record.mess).map(Temporal[F].delayBy(_, MessageReadDelay))

      new ChannelTracker[F, F[ApplyCommandResult]] {
        override val systems =
          List(params.systems, vals.systems, state.systems, start.systems, mess.systems).reduce(
            merge
          )

        override val run: F[ApplyCommandResult] =
          mutex.lock
            .surround {
              (vals.run, state.run).tupled.use { case (vs, ss) =>
                // The monitors start with the current values, which are not an answer to this
                // command. Only the new values of VAL count. The monitors are created before the
                // directive is written, so no event is missed.
                val events: Stream[F, Event] =
                  values(record.name, vs)
                    .drop(1)
                    .map(_.asLeft[CarState])
                    .merge(values(record.name, ss).map(_.asRight[Int]))
                params.run *> Concurrent[F]
                  .both(start.run, stateMachine(record.name, mess.run, events))
                  .map(_._2)
              }
            }
            .timeout(timeout)
      }
    }
  }

  def build[F[_]: {Dispatcher, Temporal}](
    srv:             EpicsService[F],
    telltaleChannel: TelltaleChannel[F],
    recordName:      String
  ): Resource[F, TaskControlCommand[F]] = for {
    record <- TaskControlRecord.build(srv, recordName)
    mutex  <- Resource.eval(Mutex[F])
  } yield new TaskControlCommandImpl[F](telltaleChannel, record, mutex)

}
