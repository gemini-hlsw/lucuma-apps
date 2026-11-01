// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import crystal.react.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.all.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.core.util.time.format.UtcFormatter
import lucuma.react.primereact.Button
import lucuma.react.primereact.TooltipOptions
import lucuma.react.table.Expandable
import lucuma.react.table.Expanded
import lucuma.react.table.RowId
import lucuma.react.table.Table
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.AtomRecord
import lucuma.schemas.model.Dataset
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.StepRecord
import lucuma.schemas.model.Visit
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.LucumaIcons
import lucuma.ui.display.given
import lucuma.ui.format.DurationPaddedFormatter
import lucuma.ui.primereact.*
import lucuma.ui.sequence.*
import lucuma.ui.syntax.render.*
import lucuma.ui.table.*
import lucuma.ui.undo.UndoButtons
import org.typelevel.log4cats.Logger

import scala.collection.immutable.HashSet

// Methods for building visits rows on the sequence table
trait SequenceRowBuilder[D] extends SequenceQaEditHelper:
  protected type SequenceTableRowType =
    Expandable[HeaderOrRow[SequenceHeaderContext[D], SequenceIndexedRow[D]]]

  protected def getRowId(row: SequenceTableRowType): RowId =
    row.value match
      case Left(HeaderRow(rowId = rowId)) => rowId
      case Right(stepRow)                 => stepRow.step.rowId

  /**
   * Row id of the group holding the live sections, whether it's the current visit, a visit that
   * just started or the remaining sequence: one stable id, so that switching between those doesn't
   * recreate the row (which would show it collapsed until expanded again).
   */
  protected val CurrentGroupRowId: RowId = RowId("current")

  /** The rows expanded from the start: the live group and its two sections. */
  protected val CurrentExpandedRowIds: List[RowId] =
    List(
      CurrentGroupRowId,
      RowId(SequenceType.Acquisition.toString),
      RowId(SequenceType.Science.toString)
    )

  protected val CurrentExpandedState: Expanded =
    Expanded.fromExpandedRows(CurrentExpandedRowIds*)

  /** Expands or collapses `rowIds`, leaving the other rows as they are. */
  protected def setRowsExpanded[T, TM, CM, TF](
    table:    Table[T, TM, CM, TF],
    rowIds:   List[RowId],
    expanded: Boolean
  ): Callback =
    table.modExpanded:
      case Expanded.AllRows    => Expanded.AllRows
      case Expanded.Rows(rows) => Expanded.Rows(rows ++ rowIds.map(_ -> expanded))

  /**
   * Expands the rows among `rowIds` the table has no state for yet (e.g. rows that just appeared).
   */
  protected def expandNewRows[T, TM, CM, TF](
    table:  Table[T, TM, CM, TF],
    rowIds: List[RowId]
  ): Callback =
    table.modExpanded:
      case Expanded.AllRows    => Expanded.AllRows
      case Expanded.Rows(rows) =>
        Expanded.Rows(rows ++ rowIds.filterNot(rows.contains).map(_ -> true))

  /**
   * A run of consecutive steps of one sequence type within a visit.
   *
   *   - `datasetRange` is the lowest and highest file number (`Dataset.Filename.index`) over the
   *     run's steps' datasets; the visit header shows it aggregated over all runs as "Files min -
   *     max".
   *   - `interval` spans the run's atom records, from the first one's first event to the last one's
   *     last event, as recorded by the ODB.
   *   - `runIndex` is the run's position within the visit, which only serves to give each run
   *     separator row a distinct row id.
   */
  protected case class VisitRun(
    sequenceType: SequenceType,
    stepRows:     NonEmptyList[SequenceIndexedRow[D]],
    datasetRange: Option[(Int, Int)],
    interval:     Option[TimestampInterval],
    runIndex:     Int
  )

  /**
   * A visit's executed steps, as runs in execution order. Step numbering is on each row
   * (`SequenceIndexedRow.index`), assigned when the runs are built. `interval` spans the visit's
   * first event to its last, as recorded by the ODB.
   */
  protected case class VisitData(
    visitId:       Visit.Id,
    effectiveTime: Timestamp,
    interval:      Option[TimestampInterval],
    runs:          List[VisitRun] // Empty only for the current visit, before a step completes
  ):
    val rowId: RowId = RowId(visitId.toString)

    def runRowId(run: VisitRun): RowId = VisitRunRowId(visitId, run.runIndex, run.sequenceType)

    lazy val stepRows: List[SequenceIndexedRow[D]] = runs.flatMap(_.stepRows.toList)

    lazy val datasetRange: Option[(Int, Int)] =
      runs
        .flatMap(_.datasetRange)
        .reduceOption: (a, b) =>
          (a._1.min(b._1), a._2.max(b._2))

  private def renderFiles(datasetRange: Option[(Int, Int)]): String =
    "Files: " + datasetRange.map((min, max) => s"$min - $max").getOrElse("---")

  // The bulk QA button of a visit or run header, when the table supports it. It's disabled when
  // the header has no datasets to target.
  private def renderBulkQaButton(
    bulkEdit: Option[DatasetQaBulkEdit],
    stepRows: List[SequenceIndexedRow[D]]
  ): TagMod =
    bulkEdit
      .map: bulkEdit =>
        val targets: List[Dataset] =
          DatasetQaBulkEdit.targets:
            stepRows
              .map(_.step)
              .collect:
                case step: SequenceRow.Executed.ExecutedStep[D] =>
                  step.executionState -> step.datasets

        QaBulkEditor(
          targets,
          inFlight = targets.exists(dataset => bulkEdit.datasetIdsInFlight.contains(dataset.id)),
          bulkEdit.onChange
        )
      .whenDefined

  protected def renderVisitHeader(
    visit:     VisitData,
    isCurrent: Boolean,
    ctx:       SequenceHeaderContext[D]
  ): VdomNode =
    val visitTime: String = UtcFormatter.format(visit.effectiveTime.toInstant)

    <.div(SequenceStyles.VisitHeader)(
      <.span(s"Visit on $visitTime" + (if isCurrent then " (Current)" else "")),
      <.span(s"Steps: ${visit.stepRows.size}"),
      <.span(renderFiles(visit.datasetRange)),
      renderBulkQaButton(ctx.datasetQaBulkEdit, visit.stepRows),
      ctx.visitRunButtons,
      // The visit's wall-clock duration, once it's over.
      visit.interval
        .filterNot(_ => isCurrent)
        .map: interval =>
          <.span(SequenceStyles.VisitDuration)(
            DurationPaddedFormatter(interval.timeSpan.toDuration)
          )
    )

  // Heads the runs within a visit. Steps is non-empty => head and last are safe.
  protected def renderVisitRunHeader(run: VisitRun, ctx: SequenceHeaderContext[D]): VdomNode =
    <.div(SequenceStyles.VisitRunHeader)(
      <.span(run.sequenceType.shortName),
      <.span(s"Steps: ${run.stepRows.head.index} - ${run.stepRows.last.index}"),
      <.span(renderFiles(run.datasetRange)),
      renderBulkQaButton(ctx.datasetQaBulkEdit, run.stepRows.toList),
      // The run's wall-clock duration.
      run.interval.map: interval =>
        <.span(SequenceStyles.VisitDuration)(
          DurationPaddedFormatter(interval.timeSpan.toDuration)
        )
    )

  /** The row ids of every run within `visits`, e.g. to expand them. */
  protected def visitRunRowIds(visits: List[VisitData]): List[RowId] =
    visits.flatMap(visit => visit.runs.map(visit.runRowId))

  /**
   * The row ids of the current visit's runs shown within its group (see `currentVisitRuns`). Runs
   * start collapsed, except for the current visit's, so the table expands these as they appear.
   */
  protected def currentVisitRowIds(
    visits:                List[VisitData],
    currentVisitId:        Option[Visit.Id],
    hasPendingAcquisition: Boolean
  ): List[RowId] =
    currentVisitRuns(visits, currentVisitId, hasPendingAcquisition).earlier
      .foldMap(visit => visit.runs.map(visit.runRowId))

  /**
   * The row ids of the runs of every visit but the current one. Collapsing them when the current
   * visit changes keeps the runs expanded while it was current from staying so once it's past.
   */
  protected def pastVisitRunRowIds(
    visits:         List[VisitData],
    currentVisitId: Option[Visit.Id]
  ): List[RowId] =
    visitRunRowIds(visits.filterNot(visit => currentVisitId.contains_(visit.visitId)))

  /**
   * The row ids of the last visit's group and runs, e.g. to show it whole once the sequence ends.
   */
  protected def lastVisitRowIds(visits: List[VisitData]): List[RowId] =
    visits.lastOption.foldMap(visit => visit.rowId :: visit.runs.map(visit.runRowId))

  protected def renderRemainingHeader: VdomNode =
    <.div(SequenceStyles.RemainingHeader)("Remaining sequence")

  // Shown for the current visit until the ODB reports it.
  protected def renderStartingVisitHeader: VdomNode =
    <.div(SequenceStyles.RemainingHeader)("Visit starting...")

  /**
   * The `sequenceType` editor's button strip: undo buttons and Edit while idle, Cancel and Accept
   * while editing. Shared by the section header and the "Sequence complete" header.
   */
  protected def renderEditorButtons(
    sequenceType: SequenceType,
    ctxs:         SequenceEditContexts[D]
  )(using Logger[IO]): VdomNode =
    val ctx: SequenceEditContext[D] = ctxs.forSequenceType(sequenceType)

    React.Fragment(
      <.span(
        UndoButtons(ctx.undoCtx, size = PlSize.Mini, loading = ctx.isEditInFlight)
          .when(ctx.isEditing.get)
      ),
      <.span(SequenceStyles.EditorButtons)(
        Button(
          onClickE = _.stopPropagationCB >> ctx.isEditing.set(IsEditing.True),
          label = "Edit",
          icon = SequenceIcons.Pencil,
          tooltip = "Enter sequence editing mode",
          tooltipOptions = TooltipOptions.Top
        ).mini.compact
          .when:
            ctxs.isEditEnabled && !ctx.isEditing.get
        ,
        React
          .Fragment(
            Button(
              onClickE = _.stopPropagationCB >> ctx.onCancel,
              label = "Cancel",
              icon = SequenceIcons.XMark,
              tooltip = "Cancel sequence editing",
              tooltipOptions = TooltipOptions.Top,
              severity = Button.Severity.Danger,
              loading = ctx.isEditInFlight
            ).mini.compact,
            Button( // commit
              onClickE = _.stopPropagationCB >> ctx.onAccept.runAsync,
              label = "Accept",
              icon = SequenceIcons.Check,
              tooltip = "Accept sequence modifications",
              tooltipOptions = TooltipOptions.Top,
              severity = Button.Severity.Success,
              loading = ctx.isEditInFlight
            ).mini.compact
          )
          .when(ctx.isEditing.get)
      )
    )

  protected def renderCurrentHeader(
    sequenceType: SequenceType
  )(using Logger[IO]): SequenceHeaderContext[D] => VdomNode =
    headerCtx =>
      <.span(
        SequenceStyles.CurrentHeader,
        sequenceType.toString,
        renderEditorButtons(sequenceType, headerCtx.editContexts)
      )

  // Every step has run. The science editor's buttons live here until a step is added, since
  // the Science section (and its header) only appears once there are pending steps.
  protected def renderSequenceCompleteHeader(using
    Logger[IO]
  ): SequenceHeaderContext[D] => VdomNode =
    headerCtx =>
      <.div(
        SequenceStyles.RemainingHeader,
        SequenceStyles.CurrentHeader,
        "Sequence complete",
        renderEditorButtons(SequenceType.Science, headerCtx.editContexts)
      )

  // private val ArchiveBaseUrl = "https://archive.gemini.edu/preview" // In case they want the image instead
  private val ArchiveBaseUrl = "https://archive.gemini.edu/fullheader"

  protected def renderVisitExtraRow(
    step:               SequenceRow.Executed.ExecutedStep[D],
    showOngoingLabel:   Boolean,
    enableQaEditor:     Boolean,
    allVisits:          View[Option[ExecutionVisits]],
    datasetIdsInFlight: View[HashSet[Dataset.Id]]
  )(using
    FetchClient[IO, ObservationDB],
    ToastCtx[IO],
    Logger[IO]
  ) =
    <.div(SequenceStyles.VisitStepExtra)(
      <.span(SequenceStyles.VisitStepExtraDatetime)(
        step.interval
          .map(_.start.toInstant)
          .fold("---")(start => UtcFormatter.format(start))
      ),
      <.span(SequenceStyles.VisitStepExtraStatus)(
        step.executionState.renderVdom
          .unless(!showOngoingLabel && step.executionState === StepExecutionState.Ongoing)
      ),
      <.span(SequenceStyles.VisitStepExtraDatasets)(
        step.datasets
          .map: dataset =>
            val datasetName: String = dataset.filename.format

            <.span(^.key := dataset.id.toString)(SequenceStyles.VisitStepExtraDatasetItem)(
              if (dataset.isWritten)
                <.a(^.href := s"$ArchiveBaseUrl/$datasetName", ^.target.blank)(
                  datasetName
                )
              else datasetName,
              <.span(SequenceStyles.VisitStepExtraDatasetQAStatus)(
                if datasetIdsInFlight.get.contains(dataset.id)
                then LucumaIcons.CircleNotch
                else
                  val qaIcon: VdomNode = renderQaIcon(dataset.qaState, dataset.comment)
                  if enableQaEditor then
                    QaEditor(
                      dataset,
                      qaIcon,
                      onDatasetQaChange(allVisits, datasetIdsInFlight)
                    )
                  else qaIcon
              )
            )
          .toVdomArray
      )
    )

  private def buildVisitRun(
    visitId:       Visit.Id,
    atoms:         List[AtomRecord[D]],
    sequenceType:  SequenceType,
    currentStepId: Option[Step.Id], // Will be removed from visit rows
    startIndex:    StepIndex,
    runIndex:      Int
  ): (Option[VisitRun], StepIndex) =
    val steps: List[StepRecord[D]] =
      atoms.flatMap(_.steps).filterNot(step => currentStepId.contains_(step.id))

    val (rows: List[(SequenceRow.Executed.ExecutedStep[D], StepIndex)], nextIndex: StepIndex) =
      steps
        .map(SequenceRow.Executed.ExecutedStep(visitId, _, none, none))
        .zipWithStepIndex(startIndex)

    val datasetIndices: List[Int] = steps.flatMap(_.datasets).map(_.filename.index.value)

    val run: Option[VisitRun] =
      NonEmptyList
        .fromList(rows.map(SequenceIndexedRow(_, _)))
        .map: stepRows =>
          VisitRun(
            sequenceType,
            stepRows,
            datasetIndices.minOption.map(min => (min, datasetIndices.max)),
            atoms.flatMap(_.interval).reduceOption(_.span(_)),
            runIndex
          )

    (run, nextIndex)

  // Groups consecutive atoms of the same sequence type, keeping their order.
  private def consecutiveRuns(
    atoms: List[AtomRecord[D]]
  ): List[(SequenceType, List[AtomRecord[D]])] =
    atoms.foldRight(List.empty[(SequenceType, List[AtomRecord[D]])]):
      case (atom, (seqType, run) :: rest) if seqType === atom.sequenceType =>
        (seqType, atom :: run) :: rest
      case (atom, runs)                                                    =>
        (atom.sequenceType, List(atom)) :: runs

  /**
   * Builds the visit rows: one `VisitData` per visit, holding one run per group of consecutive
   * atoms of the same sequence type, in execution order (so that an acquisition reinserted in the
   * middle of science is shown where it ran).
   *
   * Acquisition indices restart at 1 in each visit. Science indices continue from one visit to the
   * next.
   */
  def visitsSequences(
    visits:         List[Visit[D]],
    currentStepId:  Option[Step.Id], // Will be removed from visits
    currentVisitId: Option[Visit.Id] // Kept even before any of its steps completes
  ): (List[VisitData], StepIndex) =
    visits
      .foldLeft((List.empty[VisitData], StepIndex.One)): (accum, visit) =>
        val (seqs, scienceIndex) = accum

        val (runs, _, nextScienceIndex, _) =
          consecutiveRuns(visit.atoms)
            .foldLeft((List.empty[VisitRun], StepIndex.One, scienceIndex, 0)):
              case ((acc, acquisitionIndex, scienceIndex, runIndex), (seqType, atoms)) =>
                val startIndex: StepIndex = seqType match
                  case SequenceType.Acquisition => acquisitionIndex
                  case SequenceType.Science     => scienceIndex

                val (run, nextIndex) =
                  buildVisitRun(visit.id, atoms, seqType, currentStepId, startIndex, runIndex)

                seqType match
                  case SequenceType.Acquisition =>
                    (acc ++ run.toList, nextIndex, scienceIndex, runIndex + 1)
                  case SequenceType.Science     =>
                    (acc ++ run.toList, acquisitionIndex, nextIndex, runIndex + 1)

        // The current visit is kept even without completed steps, so that it's shown as soon as
        // it starts (its running step is not among the visit rows).
        val visitData: Option[VisitData] =
          Option.when(runs.nonEmpty || currentVisitId.contains_(visit.id)):
            VisitData(visit.id, visit.effectiveTime, visit.interval, runs)

        (seqs ++ visitData.toList, nextScienceIndex)

  protected val AlertRowId: RowId = RowId("alert")

  /**
   * How the current visit's runs are shown: `earlier` ones (if any) as a visit of their own, in
   * execution order; the others merged into the live Acquisition and Science sections.
   */
  protected case class CurrentVisitRuns(
    earlier:         Option[VisitData],
    liveAcquisition: Option[VisitRun],
    liveScience:     Option[VisitRun]
  )

  /**
   * The live Science section takes the trailing science run, unless pending acquisition steps come
   * between them. The live Acquisition section takes the trailing acquisition run left after that.
   * Everything earlier goes to a visit of its own, so that an acquisition reinserted in the middle
   * of science (possibly more than once) reads in the order it ran. In the usual flow (acquisition,
   * then science) nothing is moved.
   */
  protected def currentVisitRuns(
    visits:                List[VisitData],
    currentVisitId:        Option[Visit.Id],
    hasPendingAcquisition: Boolean
  ): CurrentVisitRuns =
    val currentVisit: Option[VisitData] =
      visits.find(visit => currentVisitId.contains_(visit.visitId))
    val runs: List[VisitRun]            = currentVisit.foldMap(_.runs)

    val (liveScience: Option[VisitRun], rest: List[VisitRun]) =
      if !hasPendingAcquisition && runs.lastOption.exists(_.sequenceType === SequenceType.Science)
      then (runs.lastOption, runs.dropRight(1))
      else (none, runs)

    val (liveAcquisition: Option[VisitRun], earlier: List[VisitRun]) =
      if rest.lastOption.exists(_.sequenceType === SequenceType.Acquisition)
      then (rest.lastOption, rest.dropRight(1))
      else (none, rest)

    CurrentVisitRuns(
      (currentVisit, NonEmptyList.fromList(earlier))
        .mapN((visit, runs) => visit.copy(runs = runs.toList)),
      liveAcquisition,
      liveScience
    )

  protected case class AlertRow(sequenceType: SequenceType, position: NonNegInt, content: VdomNode)

  def stitchSequence(
    visits:                List[VisitData],
    currentVisitId:        Option[Visit.Id],     // Used to move current visit steps to current sequences
    nextScienceIndex:      StepIndex,            // Used to continue numbering from visits
    acquisitionRows:       List[SequenceRow[D]], // Should have completed steps already removed
    scienceRows:           List[SequenceRow[D]], // Should have completed steps already removed
    alertRow:              Option[AlertRow] = none,
    showSequenceLevelEdit: Boolean = false       // Every step has run and the user may add more
  )(using Logger[IO]): List[SequenceTableRowType] = {
    val (pastVisits: List[VisitData], currentVisits: List[VisitData]) =
      visits.partition: visitData =>
        !currentVisitId.contains_(visitData.visitId)

    val currentRuns: CurrentVisitRuns =
      currentVisitRuns(visits, currentVisitId, acquisitionRows.nonEmpty)

    // A visit is an expandable group of expandable runs. Runs start collapsed, except for the
    // current visit's (see `currentVisitRowIds`).
    def runRows(visit: VisitData): List[SequenceTableRowType] =
      visit.runs.map: run =>
        Expandable(
          HeaderRow(
            visit.runRowId(run),
            (ctx: SequenceHeaderContext[D]) => renderVisitRunHeader(run, ctx)
          ).toHeaderOrRow,
          run.stepRows.toList.map(step => Expandable(step.toHeaderOrRow))
        )

    def visitGroup(
      visit:     VisitData,
      isCurrent: Boolean,
      children:  List[SequenceTableRowType]
    ): SequenceTableRowType =
      Expandable(
        HeaderRow(
          if isCurrent then CurrentGroupRowId else visit.rowId,
          (ctx: SequenceHeaderContext[D]) => renderVisitHeader(visit, isCurrent, ctx),
          collapsible = !isCurrent // The current visit holds the live sections.
        ).toHeaderOrRow,
        children
      )

    val pastVisitsRows: List[SequenceTableRowType] =
      pastVisits.map(visit => visitGroup(visit, false, runRows(visit)))

    def liveRows(run: Option[VisitRun]): List[SequenceTableRowType] =
      run.toList.flatMap(_.stepRows.toList).map(step => Expandable(step.toHeaderOrRow))

    val currentVisitAcquisitionRows: List[SequenceTableRowType] =
      liveRows(currentRuns.liveAcquisition)

    val currentVisitScienceRows: List[SequenceTableRowType] =
      liveRows(currentRuns.liveScience)

    // Acquisition numbering continues after every acquisition step executed in the current visit.
    val executedAcquisitionCount: Int =
      currentVisits
        .flatMap(_.runs)
        .filter(_.sequenceType === SequenceType.Acquisition)
        .map(_.stepRows.size)
        .sum

    def insertAlertRow(
      sequenceType: SequenceType,
      stepRows:     List[SequenceTableRowType]
    ): List[SequenceTableRowType] =
      alertRow
        .filter(_.sequenceType === sequenceType)
        .fold(stepRows): alert =>
          val (before, after) = stepRows.splitAt(alert.position.value)
          before ++ List(Expandable(HeaderRow(AlertRowId, alert.content).toHeaderOrRow)) ++ after

    def buildSequenceRows(
      sequenceType:     SequenceType,
      currentVisitRows: List[SequenceTableRowType],
      steps:            List[SequenceRow[D]],
      nextIndex:        StepIndex
    )(using Logger[IO]): List[SequenceTableRowType] =
      Option
        .when(currentVisitRows.nonEmpty || steps.nonEmpty):
          Expandable(
            HeaderRow(
              RowId(sequenceType.toString),
              renderCurrentHeader(sequenceType)
            ).toHeaderOrRow,
            currentVisitRows ++
              insertAlertRow(
                sequenceType,
                steps
                  .zipWithStepIndex(nextIndex)
                  ._1
                  .map: (step, index) =>
                    Expandable(SequenceIndexedRow(step, index).toHeaderOrRow)
              )
          )
        .toList

    val nextAcquisitionIndex: StepIndex =
      StepIndex(PosInt.unsafeFrom(executedAcquisitionCount + 1))

    val acquisitionTableRows: List[SequenceTableRowType] =
      buildSequenceRows(
        SequenceType.Acquisition,
        currentVisitAcquisitionRows,
        acquisitionRows,
        nextAcquisitionIndex
      )

    val scienceTableRows: List[SequenceTableRowType] =
      buildSequenceRows(
        SequenceType.Science,
        currentVisitScienceRows,
        scienceRows,
        nextScienceIndex
      )

    // The live sections (trailing executed runs and pending steps) are nested in the current
    // visit's group, after its earlier runs, so that the group doesn't come and go as the
    // acquisition is skipped or reinserted. Before a visit starts (or between visits), they are
    // nested in a "Remaining sequence" group instead.
    val liveSections: List[SequenceTableRowType] = acquisitionTableRows ++ scienceTableRows

    def liveGroup(
      header:       SequenceHeaderContext[D] => VdomNode,
      iconOverride: Option[VdomNode] = none
    ): SequenceTableRowType =
      Expandable(
        HeaderRow(CurrentGroupRowId, header, collapsible = false, iconOverride).toHeaderOrRow,
        liveSections
      )

    val currentGroupRows: List[SequenceTableRowType] =
      (currentVisits.headOption, currentVisitId) match
        case (Some(visit), _)                         =>
          List(
            visitGroup(visit, true, currentRuns.earlier.toList.flatMap(runRows) ++ liveSections)
          )
        // The visit has started but the ODB hasn't reported it yet.
        case (None, Some(_)) if liveSections.nonEmpty =>
          List(liveGroup(_ => renderStartingVisitHeader))
        case (None, None) if liveSections.nonEmpty    =>
          List(liveGroup(_ => renderRemainingHeader))
        case (None, None) if showSequenceLevelEdit    =>
          List(liveGroup(renderSequenceCompleteHeader, Some[VdomNode](SequenceIcons.Check)))
        case _                                        => Nil

    pastVisitsRows ++ currentGroupRows
  }
