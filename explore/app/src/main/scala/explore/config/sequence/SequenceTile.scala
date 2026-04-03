// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import explore.*
import explore.components.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.sequence.byInstrument.*
import explore.model.AppContext
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.ui.sequence.*
import lucuma.ui.sequence.IsEditing
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.scalajs.dom.HTMLElement

import scala.collection.immutable.SortedSet

final case class SequenceTile(
  obsId:                Observation.Id,
  obsExecution:         Execution,
  asterismIds:          SortedSet[Target.Id],
  customSedTimestamps:  List[Timestamp],
  calibrationRole:      Option[CalibrationRole],
  sequenceChanged:      View[Pot[Unit]],
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean
) extends Tile[SequenceTile](
      ObsTabTileIds.SequenceId.id,
      "Sequence",
      canMinimize = !isEditingAcquisition.get && !isEditingScience.get
    )(
      SequenceTile // TODO Move isEditing state here, but we need to be able to change tile state from within tile
    )

object SequenceTile
    extends TileComponent[SequenceTile]((props, sizeState) =>
      import SequenceTileHelper.*

      for
        ctx          <- useContext(AppContext.ctx)
        liveSequence <- useLiveSequence(
                          props.obsId,
                          props.asterismIds.toList,
                          props.customSedTimestamps,
                          props.calibrationRole
                        )
        _            <-
          useEffectWithDeps(liveSequence): newLiveSequence =>
            props.sequenceChanged.set: // Notify caller of change
              (newLiveSequence.visits, newLiveSequence.sequence.map(_.get)).tupled.void
      yield
        val execution: Execution           = props.obsExecution
        val staleCss: TagMod               = execution.digest.staleClass
        val staleTooltip: Option[VdomNode] = execution.digest.staleTooltip
        val programTimeCharge: TimeSpan    = execution.programTimeCharge.value
        val executed: TagOf[HTMLElement]   = timeDisplay("Executed", programTimeCharge)

        val isEditEnabled: IsEditEnabled =
          IsEditEnabled(sizeState.isMaximized && liveSequence.isReady)

        val title =
          <.span(
            execution.digest.programTimeEstimate.value
              .map: plannedTime =>
                val total: TimeSpan             = programTimeCharge +| plannedTime
                val pending: TagOf[HTMLElement] =
                  timeDisplay(
                    "Pending",
                    plannedTime,
                    timeClass = staleCss,
                    timeTooltip = staleTooltip
                  )
                val planned: TagOf[HTMLElement] =
                  timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)

                <.span(ExploreStyles.SequenceTileTitle)(
                  <.span(ExploreStyles.SequenceTileTitleSide, ExploreStyles.SequenceTileTitleUndo)(
                    // UndoButtons(undoCtx, size = PlSize.Mini).when(props.isEditing.get).unless(isEditInFlight.get)
                  ),
                  <.span(ExploreStyles.SequenceTileTitleSummary)(
                    HelpIcon("target/main/sequence-times.md".refined),
                    planned,
                    executed,
                    pending
                  )
                )
              .getOrElse(executed)
          )

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        // TODO Test the Iso?
        def flatExecutionSequence[D]: Iso[Option[ExecutionSequence[D]], List[Atom[D]]] =
          Iso[Option[ExecutionSequence[D]], List[Atom[D]]](
            _.foldMap(s => s.nextAtom +: s.possibleFuture)
          )(l => if (l.isEmpty) none else ExecutionSequence(l.head, l.tail, false).some)

        val body =
          props.sequenceChanged.get
            .flatMap: _ =>
              (liveSequence.visits, liveSequence.sequence).tupled
            .renderPot(
              (visitsViewOpt, sequenceViewOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceViewOpt.toOptionView
                  .flatMap { sequnceView =>
                    sequnceView.get match
                      case SequenceData(
                            InstrumentExecutionConfig.GmosNorth(config),
                            signalToNoise
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.gmosNorth)
                              .andThen(InstrumentExecutionConfig.GmosNorth.executionConfig)
                          )
                          .toOptionView
                          .map: gmosNorthExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                                GmosNorthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosNorthExecutionView.zoom:
                                    ExecutionConfig.acquisition.andThen(flatExecutionSequence)
                                  ,
                                  gmosNorthExecutionView.zoom:
                                    ExecutionConfig.science.andThen(flatExecutionSequence)
                                  ,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosNorthSequence(props.obsId, seqType, atoms)
                                )
                              case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                                GmosNorthImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosNorthExecutionView.zoom:
                                    ExecutionConfig.acquisition.andThen(flatExecutionSequence)
                                  ,
                                  gmosNorthExecutionView.zoom:
                                    ExecutionConfig.science.andThen(flatExecutionSequence)
                                  ,
                                  snPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosNorthSequence(props.obsId, seqType, atoms)
                                )
                              case _                                                        => mismatchError
                      case SequenceData(
                            InstrumentExecutionConfig.GmosSouth(config),
                            signalToNoise
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.gmosSouth)
                              .andThen(InstrumentExecutionConfig.GmosSouth.executionConfig)
                          )
                          .toOptionView
                          .map: gmosSouthExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                                GmosSouthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosSouthExecutionView.zoom:
                                    ExecutionConfig.acquisition.andThen(flatExecutionSequence)
                                  ,
                                  gmosSouthExecutionView.zoom:
                                    ExecutionConfig.science.andThen(flatExecutionSequence)
                                  ,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosSouthSequence(props.obsId, seqType, atoms)
                                )
                              case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                                GmosSouthImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosSouthExecutionView.zoom:
                                    ExecutionConfig.acquisition.andThen(flatExecutionSequence)
                                  ,
                                  gmosSouthExecutionView.zoom:
                                    ExecutionConfig.science.andThen(flatExecutionSequence)
                                  ,
                                  snPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosSouthSequence(props.obsId, seqType, atoms)
                                )
                              case _                                                        => mismatchError
                      case SequenceData(
                            InstrumentExecutionConfig.Flamingos2(config),
                            ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.flamingos2)
                              .andThen(InstrumentExecutionConfig.Flamingos2.executionConfig)
                          )
                          .toOptionView
                          .map: flamingos2ExecutionView =>
                            Flamingos2SequenceTable(
                              visitsViewOpt,
                              config.static,
                              flamingos2ExecutionView.zoom:
                                ExecutionConfig.acquisition.andThen(flatExecutionSequence)
                              ,
                              flamingos2ExecutionView.zoom:
                                ExecutionConfig.science.andThen(flatExecutionSequence)
                              ,
                              acquisitionSn,
                              scienceSn,
                              isEditEnabled,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              seqType =>
                                atoms =>
                                  ctx.odbApi
                                    .replaceFlamingos2Sequence(props.obsId, seqType, atoms)
                            )
                      case SequenceData(
                            InstrumentExecutionConfig.Igrins2(config),
                            ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.igrins2)
                              .andThen(InstrumentExecutionConfig.Igrins2.executionConfig)
                          )
                          .toOptionView
                          .map: igrins2ExecutionView =>
                            Igrins2SequenceTable(
                              visitsViewOpt,
                              config.static,
                              igrins2ExecutionView.zoom:
                                ExecutionConfig.science.andThen(flatExecutionSequence)
                              ,
                              scienceSn,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              _ => atoms => IO(atoms)
                            )
                      case _ => mismatchError.some
                  }
                  .getOrElse:
                    Message(
                      text = "Empty or incomplete sequence data returned by server",
                      severity = Message.Severity.Error
                    )
              ,
              errorRender = m =>
                <.div(ExploreStyles.SequencesPanelError)(
                  Message(
                    text = m.getMessage,
                    severity = Message.Severity.Warning,
                    icon = Icons.ExclamationTriangle
                  )
                )
            )

        TileContents(title, body)
    )
