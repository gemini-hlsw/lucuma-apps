// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.PeakPixel
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
  def config: View[ExecutionConfig[S, D]]
  def visits: View[Option[ExecutionVisits]]
  def executionState: ExecutionState
  def currentRecordedVisit: Option[RecordedVisit]
  def progress: Option[StepProgress]
  def selectedRowId: Option[SelectedRowId]
  def setSelectedRowId: SelectedRowId => Callback
  def requests: ObservationRequests
  def isPreview: Boolean
  def columnProfile: SequenceColumnProfile
  def onBreakpointFlip: (Observation.Id, Step.Id) => Callback
  def sequenceControls: VdomNode
  def isEditEnabled: IsEditEnabled
  def isEditingAcquisition: View[IsEditing]
  def isEditingScience: View[IsEditing]
  def remoteReplace: SequenceType => List[Atom[D]] => IO[List[Atom[D]]]

  // The mask name for a GMOS custom mask attachment, when known. Only GMOS sequence
  // tables have a custom mask FPU, so other instruments keep the default.
  def maskName(attachmentId: Attachment.Id): Option[NonEmptyString] = None

  // Only GHOST sequence tables have a resolution mode.
  def ghostResolutionMode: Option[GhostResolutionMode] = None

  // Only GNIRS sequence tables have a well depth.
  def gnirsWellDepth: Option[GnirsWellDepth] = None

  def signalToNoise: SequenceType => Option[NonEmptyString] => D => Option[SignalToNoiseValue]
  def peakPixel: SequenceType => Option[NonEmptyString] => D => Option[PeakPixel]
  def toInstrumentVisits: PartialFunction[ExecutionVisits, NonEmptyList[Visit[D]]]

  protected[sequence] lazy val instrumentVisits: List[Visit[D]] =
    visits.get
      .collect:
        toInstrumentVisits.andThen(_.toList)
      .orEmpty

  // All step ids recorded in the current (last) visit. The currently executing step is not
  // necessarily the last one recorded: after a re-acquisition, a regenerated science step is
  // recorded in the science atom that precedes the new acquisition atoms.
  private lazy val currentVisitStepIds: Set[Step.Id] =
    instrumentVisits.lastOption
      .foldMap(_.atoms.flatMap(_.steps.map(_.id)))
      .toSet

  protected[sequence] lazy val loadedStepId: Option[Step.Id] =
    executionState.loadedStep.map(_.id)

  // The currently executing step, if it's already recorded in the current visit. This will be
  // filtered out from the visit steps, so that it isn't shown twice.
  protected[sequence] lazy val currentRecordedStepId: Option[Step.Id] =
    loadedStepId.filter(currentVisitStepIds.contains)

  // There's a temporary situation where the loadedStep has moved on to the next one,
  // but the visits and sequence haven't caught up yet. Therefore, if the loadedStep id
  // is the 2nd in the future sequence, and the 1st in the future sequence is already recorded
  // in the current visit, we remove the 1st future step.
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
    SequenceRow.FutureStep.fromAtoms(
      atoms,
      signalToNoise(seqType),
      peakPixel(seqType),
      seqType
    ) match
      case head :: tail if shouldHideFirstFutureStep(tail.headOption.flatMap(_.id.toOption)) =>
        head.some
          .filterNot(_.id.toOption.exists(currentVisitStepIds.contains))
          .toList ++ tail
      case other                                                                             => other

  // The whole remaining sequences, as saved. Never display-filtered, since they are what is edited.
  protected[sequence] lazy val acquisition: View[List[Atom[D]]] = config.flatAcquisition
  protected[sequence] lazy val science: View[List[Atom[D]]]     = config.flatScience

  // A step executing, or paused mid-exposure, keeps the sequence from being replaced.
  protected[sequence] lazy val isStepOngoing: Boolean =
    executionState.sequenceStatus.isRunning || executionState.pausedStep.isDefined

  /** The rows shown for the given acquisition and science sequences (saved or being edited). */
  protected[sequence] def buildRows(
    resolvedAcquisition: List[Atom[D]],
    resolvedScience:     List[Atom[D]]
  ): (List[SequenceRow[D]], List[SequenceRow[D]]) =
    val scienceRows: List[SequenceRow[D]] =
      futureSteps(resolvedScience, SequenceType.Science)

    // If initial acquisition atom is complete, then nextAtom already shows the next potential step. We want to hide that.
    // We also hide acquisition if the sequence is complete.
    val acquisitionRows: List[SequenceRow[D]] =
      if executionState.isWaitingAcquisitionPrompt || executionState.sequenceType === SequenceType.Science || scienceRows.isEmpty
      then List.empty
      else futureSteps(resolvedAcquisition.headOption.toList, SequenceType.Acquisition)

    (acquisitionRows, scienceRows)

  // Rows of the saved sequences, e.g. for column presence or the step to auto-select.
  private lazy val savedRows: (List[SequenceRow[D]], List[SequenceRow[D]]) =
    buildRows(acquisition.get, science.get)

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] = savedRows._1
  protected[sequence] lazy val scienceRows: List[SequenceRow[D]]     = savedRows._2

  protected[sequence] lazy val nextStepId: Option[Step.Id] =
    acquisitionRows.headOption
      .orElse(scienceRows.headOption)
      .flatMap(_.id.toOption)
