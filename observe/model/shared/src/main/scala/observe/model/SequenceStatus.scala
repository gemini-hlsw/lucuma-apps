// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.util.Display
import lucuma.core.util.NewBoolean
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

enum SequenceStatus(val name: String) derives Eq, Encoder, Decoder:
  case Idle                extends SequenceStatus("Idle")
  case Running(
    userStop:          SequenceStatus.HasUserStop,
    internalStop:      SequenceStatus.HasInternalStop,
    waitingUserPrompt: SequenceStatus.IsWaitingUserPrompt,
    waitingNextStep:   SequenceStatus.IsWaitingNextStep,
    starting:          SequenceStatus.IsStarting
  )                        extends SequenceStatus("Running")
  case Completed           extends SequenceStatus("Completed")
  case Failed(msg: String) extends SequenceStatus("Failed")
  case Aborted             extends SequenceStatus("Aborted")

  def isUserStopRequested: Boolean =
    this match
      case SequenceStatus.Running(b, _, _, _, _) => b
      case _                                     => false

  def isInternalStopRequested: Boolean =
    this match
      case SequenceStatus.Running(_, b, _, _, _) => b
      case _                                     => false

  def isStopRequested: Boolean =
    isUserStopRequested || isInternalStopRequested

  def isError: Boolean =
    this match
      case Failed(_) => true
      case _         => false

  def isInProcess: Boolean =
    this =!= SequenceStatus.Idle

  def isRunning: Boolean =
    this match
      case SequenceStatus.Running(_, _, _, _, _) => true
      case _                                     => false

  def isWaitingUserPrompt: Boolean =
    this match
      case SequenceStatus.Running(_, _, waitingUserPrompt, _, _) => waitingUserPrompt
      case _                                                     => false

  // A sequence can be unloaded if it's not running or if it's running but waiting for user prompt.
  def canUnload: Boolean =
    !isRunning || isWaitingUserPrompt

  def isStarting: Boolean =
    this match
      case SequenceStatus.Running(_, _, _, _, starting) => starting
      case _                                            => false

  def isCompleted: Boolean =
    this === SequenceStatus.Completed

  def isIdle: Boolean =
    this === SequenceStatus.Idle || this === SequenceStatus.Aborted

object SequenceStatus:
  given Display[SequenceStatus] = Display.byShortName(_.name)

  val running: Prism[SequenceStatus, SequenceStatus.Running] =
    GenPrism[SequenceStatus, SequenceStatus.Running]

  object HasUserStop extends NewBoolean { val Yes = True; val No = False }
  type HasUserStop = HasUserStop.Type

  object HasInternalStop extends NewBoolean { val Yes = True; val No = False }
  type HasInternalStop = HasInternalStop.Type

  object IsWaitingUserPrompt extends NewBoolean { val Yes = True; val No = False }
  type IsWaitingUserPrompt = IsWaitingUserPrompt.Type

  object IsWaitingNextStep extends NewBoolean { val Yes = True; val No = False }
  type IsWaitingNextStep = IsWaitingNextStep.Type

  object IsStarting extends NewBoolean { val Yes = True; val No = False }
  type IsStarting = IsStarting.Type

  object Running:
    val Init: Running =
      SequenceStatus.Running(
        userStop = HasUserStop.No,
        internalStop = HasInternalStop.No,
        waitingUserPrompt = IsWaitingUserPrompt.No,
        waitingNextStep = IsWaitingNextStep.No,
        starting = IsStarting.No
      )

    val userStop: Lens[SequenceStatus.Running, HasUserStop] =
      Focus[SequenceStatus.Running](_.userStop)

    val internalStop: Lens[SequenceStatus.Running, HasInternalStop] =
      Focus[SequenceStatus.Running](_.internalStop)

    val waitingUserPrompt: Lens[SequenceStatus.Running, IsWaitingUserPrompt] =
      Focus[SequenceStatus.Running](_.waitingUserPrompt)

    val waitingNextStep: Lens[SequenceStatus.Running, IsWaitingNextStep] =
      Focus[SequenceStatus.Running](_.waitingNextStep)

    val starting: Lens[SequenceStatus.Running, IsStarting] =
      Focus[SequenceStatus.Running](_.starting)
