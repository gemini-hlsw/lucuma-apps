// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.syntax.effect.*
import eu.timepit.refined.types.numeric.PosInt
import japgolly.scalajs.react.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.react.pragmaticdnd.Edge
import lucuma.react.table.*
import lucuma.ui.table.*
import monocle.Focus
import monocle.Lens

trait SequenceEditRowHelpers[D, T, R <: SequenceRow[D], TM <: SequenceTableMeta[D], CM, TF](
  getStepFromRow: T => Option[R]
) extends SequenceEditOptics[D]:

  protected type CellContextType[A] =
    CellContext[Expandable[HeaderOrRow[SequenceEditContexts[D], T]], A, TM, ?, TF, ?, ?]

  extension (row: Expandable[HeaderOrRow[SequenceEditContexts[D], T]])
    protected def getStep: Option[R] =
      row.value.toOption.flatMap(row => getStepFromRow(row))

  extension [A](cellContext: CellContextType[A])
    protected def getStep: Option[R] =
      cellContext.row.original.getStep

    protected def getFutureStep: Option[SequenceRow.FutureStep[D]] =
      cellContext.getStep.collect:
        case SequenceRow.futureStep(fs) => fs

    protected def isRowEditing: Boolean =
      (for
        seqType <- cellContext.getStep.flatMap(_.sequenceType)
        ctx     <- cellContext.table.options.meta.map(_.editContexts.forSequenceType(seqType))
      yield ctx.isEditing.get.value).getOrElse(false)

  protected def handleSeqTypeRowEditAsync[A](
    c:       CellContextType[A],
    seqType: SequenceType
  )(rowEdit: IO[Endo[List[Atom[D]]]]): Callback =
    c.table.options.meta.foldMap: meta =>
      rowEdit
        .flatMap: mod =>
          meta.editContexts.seqTypeMod(seqType)(mod).toAsync
        .runAsyncAndForget

  protected def handleAllSeqTypesRowEditAsync[A](c: CellContextType[A])(
    rowEdit: SequenceType => IO[Endo[List[Atom[D]]]]
  ): Callback =
    handleSeqTypeRowEditAsync(c, SequenceType.Acquisition)(rowEdit(SequenceType.Acquisition)) >>
      handleSeqTypeRowEditAsync(c, SequenceType.Science)(rowEdit(SequenceType.Science))

  protected def handleSeqTypeRowEdit[A](c: CellContextType[A], seqType: SequenceType)(
    rowEdit: Endo[List[Atom[D]]]
  ): Callback =
    c.table.options.meta.foldMap:
      _.editContexts.seqTypeMod(seqType)(rowEdit)

  protected def handleRowEdit[A](c: CellContextType[A])(rowEdit: Endo[List[Atom[D]]]): Callback =
    c.getFutureStep.foldMap: futureStep =>
      handleSeqTypeRowEdit(c, futureStep.seqType)(rowEdit)

  protected def handleRowValueEdit[A, B](
    c: CellContextType[A]
  )(rowEdit: Step.Id => B => Endo[List[Atom[D]]])(value: Option[B]): Callback =
    (c.getFutureStep, value).tupled.foldMap: (futureStep, v) =>
      handleSeqTypeRowEdit(c, futureStep.seqType)(rowEdit(futureStep.stepId)(v))

  // Follow this template for other fields
  protected val exposureReplace: Step.Id => TimeSpan => Endo[List[Atom[D]]] =
    modifyStep:
      combineOptionalsReplace(
        gmosNorth.andThen(gmos.DynamicConfig.GmosNorth.exposure),
        gmosSouth.andThen(gmos.DynamicConfig.GmosSouth.exposure),
        flamingos2.andThen(Flamingos2DynamicConfig.exposure),
        igrins2.andThen(Igrins2DynamicConfig.exposure),
        gnirs.andThen(GnirsDynamicConfig.exposure)
      )

  private val detectorExposure: Lens[GhostDetector, TimeSpan] =
    Focus[GhostDetector](_.exposureTime)

  private val detectorExposureCount: Lens[GhostDetector, PosInt] =
    Focus[GhostDetector](_.exposureCount)

  protected val ghostRedExposureReplace: Step.Id => TimeSpan => Endo[List[Atom[D]]] =
    modifyStep(ghostRed.andThen(detectorExposure).replace)

  protected val ghostBlueExposureReplace: Step.Id => TimeSpan => Endo[List[Atom[D]]] =
    modifyStep(ghostBlue.andThen(detectorExposure).replace)

  protected val ghostRedExposureCountReplace: Step.Id => PosInt => Endo[List[Atom[D]]] =
    modifyStep(ghostRed.andThen(detectorExposureCount).replace)

  protected val ghostBlueExposureCountReplace: Step.Id => PosInt => Endo[List[Atom[D]]] =
    modifyStep(ghostBlue.andThen(detectorExposureCount).replace)

  private val detectorReadMode: Lens[GhostDetector, GhostReadMode] =
    Focus[GhostDetector](_.readMode)

  private val detectorBinning: Lens[GhostDetector, GhostBinning] =
    Focus[GhostDetector](_.binning)

  protected val ghostRedReadModeReplace: Step.Id => GhostReadMode => Endo[List[Atom[D]]] =
    modifyStep(ghostRed.andThen(detectorReadMode).replace)

  protected val ghostBlueReadModeReplace: Step.Id => GhostReadMode => Endo[List[Atom[D]]] =
    modifyStep(ghostBlue.andThen(detectorReadMode).replace)

  protected val ghostRedBinningReplace: Step.Id => GhostBinning => Endo[List[Atom[D]]] =
    modifyStep(ghostRed.andThen(detectorBinning).replace)

  protected val ghostBlueBinningReplace: Step.Id => GhostBinning => Endo[List[Atom[D]]] =
    modifyStep(ghostBlue.andThen(detectorBinning).replace)

  protected val deleteRow: Step.Id => Endo[List[Atom[D]]] =
    stepId => atoms => extractStep(stepId)(atoms)._1

  // Inserts a copy of the row's step, with `modStep` applied to it, right below the row.
  // For executed rows, which have no place in the future sequence, the copy goes at the head.
  private def insertCopyOfRow(row: SequenceRow[D], modStep: Endo[Step[D]])(
    targetSeqType: SequenceType
  ): IO[Endo[List[Atom[D]]]] =
    IO.randomUUID
      .map(Step.Id.fromUuid(_))
      .map: newId =>
        val copyStep: Endo[Step[D]] = Step.id[D].replace(newId).andThen(modStep)

        row match
          case SequenceRow.futureStep(fs)   =>
            insertStep(
              copyStep(fs.step),
              nextTo = fs.stepId,
              position = Edge.Bottom
            )
          case SequenceRow.executedStep(es) =>
            val newStep: Step[D] = copyStep(
              Step(
                newId,
                es.stepRecord.instrumentConfig,
                es.stepRecord.stepConfig,
                es.stepRecord.telescopeConfig,
                // TODO IS THIS WHAT WE ACTUALLY WANT?? // es.interval.foldMap(_.timeSpan),
                StepEstimate.fromMax(List.empty, List.empty),
                es.stepRecord.observeClass,
                Breakpoint.Disabled
              )
            )
            {
              case Nil          => Nil // There's no sequence... What to do?
              case head :: tail =>
                val newHead: Atom[D] =
                  if row.sequenceType.contains_(targetSeqType)
                  then Atom.steps[D].modify(newStep :: _)(head)
                  else head
                newHead :: tail
            }
          case _                            => identity

  protected def cloneRow(row: SequenceRow[D]): SequenceType => IO[Endo[List[Atom[D]]]] =
    insertCopyOfRow(row, identity[Step[D]])

  // A dark doesn't guide and has no offsets.
  private val DarkTelescopeConfig: TelescopeConfig =
    TelescopeConfig(Offset.Zero, StepGuideState.Disabled)

  protected def insertDarkRow(row: SequenceRow[D]): SequenceType => IO[Endo[List[Atom[D]]]] =
    insertCopyOfRow(
      row,
      Step
        .stepConfig[D]
        .replace(StepConfig.Dark)
        .andThen(Step.observeClass[D].replace(ObserveClass.DayCal))
        .andThen(Step.telescopeConfig[D].replace(DarkTelescopeConfig))
    )
