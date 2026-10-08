// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.MonadThrow
import cats.effect.IO
import japgolly.scalajs.react.React
import japgolly.scalajs.react.feature.Context
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import observe.model.Subsystem
import observe.model.enums.RunOverride

import scala.annotation.unused

trait SequenceApi[F[_]: MonadThrow]:
  /** Load a sequence in the server */
  def loadObservation(@unused obsId: Observation.Id, @unused instrument: Instrument): F[Unit] =
    NotAuthorized

  /** Set or unset a single breakpoint */
  def setBreakpoint(
    @unused obsId:  Observation.Id,
    @unused stepId: Step.Id,
    @unused value:  Breakpoint
  ): F[Unit] =
    NotAuthorized

  /** Set or unset multiple breakpoints */
  def setBreakpoints(
    @unused obsId:   Observation.Id,
    @unused stepIds: List[Step.Id],
    @unused value:   Breakpoint
  ): F[Unit] =
    NotAuthorized

  /** Start the sequence from the next pending step */
  def startSequence(
    @unused obsId:       Observation.Id,
    @unused runOverride: RunOverride = RunOverride.Default
  ): F[Unit] =
    NotAuthorized

  /** Start the sequence from the specified step */
  def startSequenceFrom(
    @unused obsId:       Observation.Id,
    @unused stepId:      Step.Id,
    @unused runOverride: RunOverride = RunOverride.Default
  ): F[Unit] = NotAuthorized

  /** Request holding the sequence after the current step completes */
  def requestSequenceHold(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Cancel a requested sequence hold */
  def cancelSequenceHoldRequest(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Stop the current exposure */
  def stopExposure(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** N&S: Stop the exposure after the current nod(?) */
  def stopExposureGracefully(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Abort the current exposure immediately */
  def abortExposure(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Pause the current exposure immediately */
  def pauseExposure(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Stop before the exposure starts and go idle */
  def rewindStep(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** N&S: Pause the exposure after the current nod(?) */
  def pauseExposureGracefully(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Resume the current exposure if it was paused mid-exposure */
  def resumeExposure(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Runs a resource or instrument */
  def execute(
    @unused obsId:     Observation.Id,
    @unused stepId:    Step.Id,
    @unused subsystem: Subsystem
  ): F[Unit] =
    NotAuthorized

  /** Loads next atom of specified sequence type and resumes execution */
  def proceedAfterPrompt(
    @unused obsId:        Observation.Id,
    @unused sequenceType: SequenceType
  ): F[Unit] =
    NotAuthorized

object SequenceApi:
  // Default value is NotAuthorized implementations
  val ctx: Context[SequenceApi[IO]] = React.createContext(new SequenceApi[IO] {})
