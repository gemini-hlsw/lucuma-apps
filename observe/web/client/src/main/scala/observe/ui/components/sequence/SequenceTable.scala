// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.ui.sequence.*
import observe.model.ExecutionState
import observe.model.SequenceStatus
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
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

  protected[sequence] lazy val loadedStepId: Option[Step.Id] =
    executionState.loadedStep.map(_.id)

  // Obtain the id of the last recorded step only if its step id is the same
  // as the currently executing step. This will be filtered out from the visit steps.
  protected[sequence] lazy val currentRecordedStepId: Option[Step.Id] =
    lastVisitStepId.filter(loadedStepId.contains_(_))

  // There's a temporary situation where the loadedStep has moved on to the next one,
  // but the visits and sequence haven't caught up yet. Therefore, if the loadedStep id
  // is the 2nd in the future sequence, and the last visit step id is the same as the
  // 1st in the future sequence, we remove the 1st future step.
  private def shouldHideFirstFutureStep(secondStepId: Option[Step.Id]): Boolean =
    (loadedStepId, secondStepId, executionState.sequenceStatus) match
      case (Some(runStepId), Some(secStepId), _)               => runStepId === secStepId
      case (None, None, _)                                     => true
      case (None, _, status) if status =!= SequenceStatus.Idle => true // Avoid glitch
      case _                                                   => false

  private def futureSteps(
    atoms:   List[Atom[D]],
    seqType: SequenceType
  ): List[SequenceRow.FutureStep[D]] =
    SequenceRow.FutureStep.fromAtoms(atoms, signalToNoise(seqType), seqType) match
      case head :: tail if shouldHideFirstFutureStep(tail.headOption.flatMap(_.id.toOption)) =>
        head.some.filterNot(row => lastVisitStepId === row.id.toOption).toList ++ tail
      case other                                                                             => other

  protected[sequence] lazy val scienceRows: List[SequenceRow[D]] =
    config.science
      .map(s => futureSteps(s.nextAtom +: s.possibleFuture, SequenceType.Science))
      .orEmpty

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] =
    // If initial acquisition atom is complete, then nextAtom already shows the next potential step. We want to hide that.
    // We also hide acquisition if the sequence is complete.
    if executionState.isWaitingAcquisitionPrompt || executionState.sequenceType === SequenceType.Science || scienceRows.isEmpty
    then List.empty
    else
      config.acquisition
        .map(a => futureSteps(List(a.nextAtom), SequenceType.Acquisition))
        .orEmpty

  // Alert position is right after currently executing atom.
  protected[sequence] lazy val alertPosition: NonNegInt =
    NonNegInt.unsafeFrom(acquisitionRows.length)

  protected[sequence] lazy val nextStepId: Option[Step.Id] =
    acquisitionRows.headOption
      .orElse(scienceRows.headOption)
      .flatMap(_.id.toOption)
