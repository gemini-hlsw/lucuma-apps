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

        extension [D](execution: View[Option[ExecutionSequence[D]]])
          def flatExecutionSequence: View[List[Atom[D]]] =
            execution.zoom(_.foldMap(s => s.nextAtom +: s.possibleFuture))(modList =>
              optExSeq =>
                val list: List[Atom[D]]    = optExSeq.foldMap(s => s.nextAtom +: s.possibleFuture)
                val newList: List[Atom[D]] = modList(list)
                if (newList.isEmpty) none
                else ExecutionSequence(newList.head, newList.tail, optExSeq.exists(_.hasMore)).some
            )

        extension [S, D](execution: View[ExecutionConfig[S, D]])
          def flatAcquisition: View[List[Atom[D]]] =
            execution.zoom(ExecutionConfig.acquisition).flatExecutionSequence
          def flatScience: View[List[Atom[D]]]     =
            execution.zoom(ExecutionConfig.science).flatExecutionSequence

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
                                  gmosNorthExecutionView.flatAcquisition,
                                  gmosNorthExecutionView.flatScience,
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
                                  gmosNorthExecutionView.flatAcquisition,
                                  gmosNorthExecutionView.flatScience,
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
                                  gmosSouthExecutionView.flatAcquisition,
                                  gmosSouthExecutionView.flatScience,
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
                                  gmosSouthExecutionView.flatAcquisition,
                                  gmosSouthExecutionView.flatScience,
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
                              flamingos2ExecutionView.flatAcquisition,
                              flamingos2ExecutionView.flatScience,
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
                              igrins2ExecutionView.flatScience,
                              scienceSn,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              _ => atoms => IO(atoms)
                            )
                      case SequenceData(InstrumentExecutionConfig.Ghost(config), _) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.ghost)
                              .andThen(InstrumentExecutionConfig.Ghost.executionConfig)
                          )
                          .toOptionView
                          .map: ghostExecutionView =>
                            GhostSequenceTable(
                              visitsViewOpt,
                              config.static,
                              ghostExecutionView.flatScience,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              _ => atoms => IO(atoms)
                            )
                      case _                                                        => mismatchError.some
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
