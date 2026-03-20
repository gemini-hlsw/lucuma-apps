// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.*
import observe.model.ExecutionState
import observe.model.ObserveStep
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.steps.*
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode

// Helper to build Props classes for instrument sequence tables.
private trait SequenceTable[S, D](
  protected[sequence] val instrument: Instrument
):
  def clientMode: ClientMode
  def obsId: Observation.Id
  def config: ExecutionConfig[S, D]
  def visits: View[Option[ExecutionVisits]]
  def executionState: ExecutionState
  def currentRecordedVisit: Option[RecordedVisit]
  def progress: Option[StepProgress]
  def selectedRowId: Option[SelectedRowId]
  def setSelectedRowId: SelectedRowId => Callback
  def requests: ObservationRequests
  def isPreview: Boolean
  def onBreakpointFlip: (Observation.Id, Step.Id) => Callback

  def signalToNoise: SequenceType => D => Option[SignalToNoise]
  def toInstrumentVisits: PartialFunction[ExecutionVisits, NonEmptyList[Visit[D]]]

  protected[sequence] lazy val instrumentVisits: List[Visit[D]] =
    visits.get
      .collect:
        toInstrumentVisits.andThen(_.toList)
      .orEmpty

  private lazy val lastVisitStepId: Option[Step.Id] =
    instrumentVisits.lastOption
      .flatMap(_.atoms.lastOption)
      .flatMap(_.steps.lastOption)
      .map(_.id)

  private lazy val activeStepId: Option[Step.Id] =
    executionState.runningStep.filter(_.isActive).map(_.id)

  // Obtain the id of the last recorded step only if its step id is the same
  // as the currently executing step. This will be filtered out from the visit steps.
  protected[sequence] lazy val currentRecordedStepId: Option[Step.Id] =
    lastVisitStepId.filter(activeStepId.contains_(_))

  private def futureSteps(
    atoms:   List[Atom[D]],
    seqType: SequenceType
  ): List[SequenceRow.FutureStep[D]] =
    SequenceRow.FutureStep.fromAtoms(atoms, signalToNoise(seqType), seqType)

  // TODO Remove CurrentAtomStepRow and have the renderer handle the runningStep?
  protected[sequence] def runningStepToRow(
    runningStep:  Option[ObserveStep],
    sequenceType: SequenceType
  ): Option[CurrentAtomStepRow[D]] =
    runningStep.map: step =>
      CurrentAtomStepRow(
        step,
        breakpoint =
          if (executionState.breakpoints.contains_(step.id)) Breakpoint.Enabled
          else Breakpoint.Disabled,
        // isFirstOfAtom = currentSteps.headOption.exists(_.id === step.id),
        isFirstOfAtom = false, // See how to determine this... If we keep using this model.
        step.signalToNoise.filter: _ =>
          sequenceType === SequenceType.Science || step.instConfig.config.shouldShowAcquisitionSn
      )

  // protected[sequence] lazy val (currentAcquisitionRows, currentScienceRows)
  //   : (List[SequenceRow[D]], List[SequenceRow[D]]) =
  //   executionState.sequenceType match
  //     case SequenceType.Acquisition =>
  //       (currentStepsToRows(currentAtomPendingSteps, SequenceType.Acquisition),
  //        config.science.map(s => futureSteps(List(s.nextAtom), SequenceType.Science)).orEmpty
  //       )
  //     case SequenceType.Science     =>
  //       (config.acquisition
  //          .map(a => futureSteps(List(a.nextAtom), SequenceType.Acquisition))
  //          .orEmpty,
  //        currentStepsToRows(currentAtomPendingSteps, SequenceType.Science)
  //       )

  protected[sequence] lazy val scienceRows: List[SequenceRow[D]] =
    // currentScienceRows ++
    config.science
      .map(s => futureSteps(s.nextAtom +: s.possibleFuture, SequenceType.Science))
      .orEmpty

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] =
    // If initial acquisition atom is complete, then nextAtom already shows the next potential step. We want to hide that.
    // We also hide acquisition if the sequence is complete
    if executionState.isWaitingAcquisitionPrompt || executionState.sequenceType === SequenceType.Science || scienceRows.isEmpty
    then List.empty
    else
      config.acquisition
        .map(a => futureSteps(List(a.nextAtom), SequenceType.Acquisition))
        .orEmpty
    // else currentAcquisitionRows

  // Alert position is right after currently executing atom.
  protected[sequence] lazy val alertPosition: NonNegInt =
    // TODO
    NonNegInt.unsafeFrom(acquisitionRows.length)
    // NonNegInt.unsafeFrom(currentAtomPendingSteps.length)

  protected[sequence] lazy val runningStepId: Option[Step.Id] = executionState.runningStep.map(_.id)

  protected[sequence] lazy val nextStepId: Option[Step.Id] =
    // TODO Compute the first unexecuted step.
    acquisitionRows.headOption
      .orElse(scienceRows.headOption)
      .flatMap(_.id.toOption)
    // currentAtomPendingSteps.headOption.map(_.id)

  println(pprint(executionState))

  // println(
  //   s"Acquisition rows: ${acquisitionRows.map(_.id)}, Science rows: ${scienceRows.map(_.id)}, Running step id: $runningStepId, Next step id: $nextStepId"
  // )
