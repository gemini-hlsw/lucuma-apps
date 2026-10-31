// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enums.WavelengthUnits
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Attachment
import lucuma.core.model.sequence.*
import lucuma.react.table.ColumnId
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.PeakPixel
import lucuma.schemas.model.Visit
import lucuma.schemas.model.enums.AtomExecutionState
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.sequence.*

private trait SequenceTable[S, D]:
  def visits: View[Option[ExecutionVisits]]
  def staticConfig: S
  def acquisition: View[List[Atom[D]]]
  def science: View[List[Atom[D]]]
  def signalToNoise: SequenceType => Option[NonEmptyString] => D => Option[SignalToNoise]
  def peakPixel: SequenceType => Option[NonEmptyString] => D => Option[PeakPixel]
  def isEditEnabled: IsEditEnabled
  def isEditingAcquisition: View[IsEditing]
  def isEditingScience: View[IsEditing]
  def isUserStaffOrAdmin: Boolean
  def wavelengthUnits: WavelengthUnits
  def remoteReplace: SequenceType => List[Atom[D]] => IO[List[Atom[D]]]

  // Columns hidden for this observation, e.g. offsets and ITC for daytime calibrations.
  def hiddenColumns: Set[ColumnId]

  // The mask name for a custom mask attachment, when known.
  def maskName(attachmentId: Attachment.Id): Option[NonEmptyString] = None

  def toInstrumentVisits: PartialFunction[ExecutionVisits, NonEmptyList[Visit[D]]]

  lazy val isEditing: Boolean = isEditingAcquisition.get || isEditingScience.get

  protected[sequence] lazy val instrumentVisits: List[Visit[D]] =
    visits.get
      .collect:
        toInstrumentVisits.andThen(_.toList)
      .orEmpty

  private def futureSteps(
    seqType:        SequenceType,
    currentSeqType: Option[SequenceType]
  )(sequence: List[Atom[D]]): List[SequenceRow.FutureStep[D]] =
    val allSteps: List[SequenceRow.FutureStep[D]] =
      SequenceRow.FutureStep
        .fromAtoms(sequence, signalToNoise(seqType), peakPixel(seqType), seqType)
    if (currentSeqType.contains_(seqType)) allSteps.drop(1)
    else allSteps
  end futureSteps

  private lazy val currentVisitData: Option[(Visit.Id, SequenceType, Boolean)] =
    // If the last atom of the last visit is Ongoing, the sequence is executing.
    instrumentVisits.lastOption
      .filter:
        _.atoms.lastOption.exists:
          _.executionState === AtomExecutionState.Ongoing
      .map(visit =>
        (visit.id,
         visit.atoms.last.sequenceType,
         // The sequence is executing if the last step of the last visit is Ongoing.
         // Note: The last atom could be Ongoing but be paused, in which case there's no executing step.
         visit.atoms.lastOption
           .flatMap(_.steps.lastOption)
           .exists(_.executionState === StepExecutionState.Ongoing)
        )
      )

  protected[sequence] lazy val currentVisitId: Option[Visit.Id] =
    currentVisitData.map(_._1)
  private lazy val currentSequenceType: Option[SequenceType]    =
    currentVisitData.map(_._2)
  private lazy val isExecuting: Boolean                         =
    currentVisitData.exists(_._3)

  protected[sequence] def buildRows(
    resolvedAcquisition: List[Atom[D]],
    resolvedScience:     List[Atom[D]]
  ): (List[SequenceRow[D]], List[SequenceRow[D]]) = {
    val scienceRows: List[SequenceRow[D]] =
      futureSteps(SequenceType.Science, currentSequenceType.filter(_ => isExecuting))(
        resolvedScience
      )

    // Acquisition atoms can be of two types: the initial acquisition, or a repeat offer (used for
    // fine adjustments). We want to tell the types apart since we don't want to generally show
    // pending repeat offer atoms, unless they are executing. The only way to tell them apart is by
    // their description. They have different descriptions but there's no standard for them: they
    // can vary among modes and instruments. Therefore, we compare the description of the pending
    // acquisition atom with the description of the first atom of the last executed acquisition.
    // If the descriptions differ, it is considered a repeat offer. (Observe resets the acquisition
    // whenever it loads the sequence, so a pending initial atom means a new acquisition is due.)
    val isAcquisitionRepeatOffer: Boolean =
      (
        instrumentVisits.reverse.collectFirstSome(_.acquisitionAtoms.headOption),
        resolvedAcquisition.headOption
      ).mapN: (executed, pending) =>
        executed.description.map(_.value) =!= pending.description.map(_.value)
      .getOrElse(false)

    // Hide acquisition when science is executing or when sequence is complete.
    val isAcquisitionDisplayed: Boolean =
      scienceRows.nonEmpty && (currentSequenceType match
        case Some(SequenceType.Science)     => false
        case Some(SequenceType.Acquisition) => true
        case None                           => !isAcquisitionRepeatOffer)

    val acquisitionRows: List[SequenceRow[D]] =
      resolvedAcquisition.headOption // If we are executing Science, don't show any future acquisition rows.
        .filter(_ => isAcquisitionDisplayed)
        .map(List(_))
        .map(futureSteps(SequenceType.Acquisition, currentSequenceType.filter(_ => isExecuting)))
        .orEmpty

    (acquisitionRows, scienceRows)
  }
