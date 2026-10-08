// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  case Idle extends SequenceStatus("Idle")

  /**
   * The sequence is running.
   *
   *   - `sequenceHoldRequested`: the user asked to hold the sequence after the current step
   *     completes. Set by the request-hold action and cleared by the cancel-hold-request action.
   *   - `stepInterruptRequested`: the current step is being interrupted by a stop, abort or
   *     pause-exposure, or by a rewind-step (all of them through the engine's `actionStop`). The
   *     sequence goes Idle at the next execution-group boundary.
   */
  case Running(
    sequenceHoldRequested:  SequenceStatus.IsSequenceHoldRequested,
    stepInterruptRequested: SequenceStatus.IsStepInterruptRequested,
    waitingUserPrompt:      SequenceStatus.IsWaitingUserPrompt,
    waitingNextStep:        SequenceStatus.IsWaitingNextStep,
    starting:               SequenceStatus.IsStarting
  )                        extends SequenceStatus("Running")
  case Completed           extends SequenceStatus("Completed")
  case Failed(msg: String) extends SequenceStatus("Failed")
  case Aborted             extends SequenceStatus("Aborted")

  def isSequenceHoldRequested: Boolean =
    this match
      case SequenceStatus.Running(b, _, _, _, _) => b
      case _                                     => false

  def isStepInterruptRequested: Boolean =
    this match
      case SequenceStatus.Running(_, b, _, _, _) => b
      case _                                     => false

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

  def isAborted: Boolean =
    this === SequenceStatus.Aborted

  def withWaitingUserPrompt(value: Boolean): SequenceStatus =
    this match
      case r @ SequenceStatus.Running(_, _, _, _, _) =>
        r.copy(waitingUserPrompt = SequenceStatus.IsWaitingUserPrompt(value))
      case other                                     => other

  def withWaitingNextStep(value: Boolean): SequenceStatus =
    this match
      case r @ SequenceStatus.Running(_, _, _, _, _) =>
        r.copy(waitingNextStep = SequenceStatus.IsWaitingNextStep(value))
      case other                                     => other

  def withStarting(value: Boolean): SequenceStatus =
    this match
      case r @ SequenceStatus.Running(_, _, _, _, _) =>
        r.copy(starting = SequenceStatus.IsStarting(value))
      case other                                     => other

object SequenceStatus:
  given Display[SequenceStatus] = Display.byShortName(_.name)

  val running: Prism[SequenceStatus, SequenceStatus.Running] =
    GenPrism[SequenceStatus, SequenceStatus.Running]

  object IsSequenceHoldRequested extends NewBoolean { val Yes = True; val No = False }
  type IsSequenceHoldRequested = IsSequenceHoldRequested.Type

  object IsStepInterruptRequested extends NewBoolean { val Yes = True; val No = False }
  type IsStepInterruptRequested = IsStepInterruptRequested.Type

  object IsWaitingUserPrompt extends NewBoolean { val Yes = True; val No = False }
  type IsWaitingUserPrompt = IsWaitingUserPrompt.Type

  object IsWaitingNextStep extends NewBoolean { val Yes = True; val No = False }
  type IsWaitingNextStep = IsWaitingNextStep.Type

  object IsStarting extends NewBoolean { val Yes = True; val No = False }
  type IsStarting = IsStarting.Type

  object Running:
    val Init: Running =
      SequenceStatus.Running(
        sequenceHoldRequested = IsSequenceHoldRequested.No,
        stepInterruptRequested = IsStepInterruptRequested.No,
        waitingUserPrompt = IsWaitingUserPrompt.No,
        waitingNextStep = IsWaitingNextStep.No,
        starting = IsStarting.No
      )

    val Starting: SequenceStatus = Init.withStarting(true)

    val sequenceHoldRequested: Lens[SequenceStatus.Running, IsSequenceHoldRequested] =
      Focus[SequenceStatus.Running](_.sequenceHoldRequested)

    val stepInterruptRequested: Lens[SequenceStatus.Running, IsStepInterruptRequested] =
      Focus[SequenceStatus.Running](_.stepInterruptRequested)

    val waitingUserPrompt: Lens[SequenceStatus.Running, IsWaitingUserPrompt] =
      Focus[SequenceStatus.Running](_.waitingUserPrompt)

    val waitingNextStep: Lens[SequenceStatus.Running, IsWaitingNextStep] =
      Focus[SequenceStatus.Running](_.waitingNextStep)

    val starting: Lens[SequenceStatus.Running, IsStarting] =
      Focus[SequenceStatus.Running](_.starting)
