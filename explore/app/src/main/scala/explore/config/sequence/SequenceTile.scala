// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.given
import explore.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.sequence.byInstrument.*
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.TooltipOptions
import lucuma.refined.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.model.Visit
import lucuma.ui.primereact.*
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SequenceData
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

object SequenceTile extends SequenceTileHelper:
  def apply(
    obsId:               Observation.Id,
    obsExecution:        Execution,
    asterismIds:         SortedSet[Target.Id],
    customSedTimestamps: List[Timestamp],
    calibrationRole:     Option[CalibrationRole],
    sequenceChanged:     View[Pot[Unit]],
    isEditing:           View[IsEditing]
  ) =
    // TODO DISABLE CLOSE IF IT'S EDITING! (AND HIDE EDIT BUTTON IF CLOSED)
    Tile(
      ObsTabTileIds.SequenceId.id,
      "Sequence",
      0, // TODO This is a temporary mechanism for demo purposes
      canMinimize = !isEditing.get
    )(
      i =>
        Body(
          obsId,
          asterismIds.toList,
          customSedTimestamps,
          calibrationRole,
          sequenceChanged,
          isEditing.get,
          i.get
        ),
      (i, sizeState) => Title(obsExecution, isEditing, i.mod(_ + 1), sizeState.isMinimized)
    )

  private case class Body(
    obsId:               Observation.Id,
    targetIds:           List[Target.Id],
    customSedTimestamps: List[Timestamp],
    calibrationRole:     Option[CalibrationRole],
    sequenceChanged:     View[Pot[Unit]],
    isEditing:           IsEditing,
    i:                   Int // TODO This is a temporary mechanism for demo purposes
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        for
          liveSequence <- useLiveSequence(
                            props.obsId,
                            props.targetIds,
                            props.customSedTimestamps,
                            props.calibrationRole
                          )
          _            <- useEffectWithDeps(liveSequence.data): dataPot =>
                            props.sequenceChanged.set(dataPot.void)
        yield
          val mismatchError = Message(
            text = "ERROR: Sequence and S/N are inconsistent.",
            severity = Message.Severity.Error
          )

          props.sequenceChanged.get
            .flatMap(_ => liveSequence.data)
            .renderPot(
              (visitsOpt, sequenceDataOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceDataOpt
                  .fold[VdomNode](
                    Message(
                      text = "Empty or incomplete sequence data returned by server",
                      severity = Message.Severity.Error
                    )
                  ) {
                    case SequenceData(InstrumentExecutionConfig.GmosNorth(config), signalToNoise) =>
                      val visits: List[Visit.GmosNorth] =
                        visitsOpt
                          .collect:
                            case ExecutionVisits.GmosNorth(vs) => vs.toList
                          .orEmpty

                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosNorthSpectroscopySequenceTable(
                            visits,
                            config,
                            acquisitionSn,
                            scienceSn,
                            props.isEditing,
                            props.i
                          )
                        case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                          GmosNorthImagingSequenceTable(
                            visits,
                            config,
                            snPerFilter,
                            props.isEditing,
                            props.i
                          )
                        case _                                                        => mismatchError
                    case SequenceData(InstrumentExecutionConfig.GmosSouth(config), signalToNoise) =>
                      val visits: List[Visit.GmosSouth] =
                        visitsOpt
                          .collect:
                            case ExecutionVisits.GmosSouth(vs) => vs.toList
                          .orEmpty

                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosSouthSpectroscopySequenceTable(
                            visits,
                            config,
                            acquisitionSn,
                            scienceSn,
                            props.isEditing,
                            props.i
                          )
                        case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                          GmosSouthImagingSequenceTable(
                            visits,
                            config,
                            snPerFilter,
                            props.isEditing,
                            props.i
                          )
                        case _                                                        => mismatchError
                    case SequenceData(
                          InstrumentExecutionConfig.Flamingos2(config),
                          ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                        ) =>
                      Flamingos2SequenceTable(
                        visitsOpt
                          .collect:
                            case ExecutionVisits.Flamingos2(visits) => visits.toList
                          .orEmpty,
                        config,
                        acquisitionSn,
                        scienceSn,
                        props.isEditing,
                        props.i
                      )
                    case _                                                                        => mismatchError
                  },
              errorRender = m =>
                <.div(ExploreStyles.SequencesPanelError)(
                  Message(
                    text = m.getMessage,
                    severity = Message.Severity.Warning,
                    icon = Icons.ExclamationTriangle
                  )
                )
            )
      )

  private case class Title(
    obsExecution: Execution,
    isEditing:    View[IsEditing],
    commitEdit:   Callback,
    isMinimized:  Boolean
  ) extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>

        val execution         = props.obsExecution
        val staleCss          = execution.digest.staleClass
        val staleTooltip      = execution.digest.staleTooltip
        val programTimeCharge = execution.programTimeCharge.value

        val executed = timeDisplay("Executed", programTimeCharge)

        <.span(
          execution.digest.programTimeEstimate.value
            .map: plannedTime =>
              val total   = programTimeCharge +| plannedTime
              val pending = timeDisplay(
                "Pending",
                plannedTime,
                timeClass = staleCss,
                timeTooltip = staleTooltip
              )
              val planned =
                timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)

              <.span(ExploreStyles.SequenceTileTitle)(
                <.span(ExploreStyles.SequenceTileTitleSide),
                <.span(ExploreStyles.SequenceTileTitleSummary)(
                  HelpIcon("target/main/sequence-times.md".refined),
                  planned,
                  executed,
                  pending
                ),
                <.span(
                  ExploreStyles.SequenceTileTitleSide,
                  ExploreStyles.SequenceTileTitleEdit
                )(
                  Button(
                    onClick = props.isEditing.set(IsEditing.True),
                    label = "Edit",
                    icon = Icons.Pencil,
                    tooltip = "Enter sequence editing mode",
                    tooltipOptions = TooltipOptions.Top
                  ).mini.compact.when(!props.isEditing.get && !props.isMinimized),
                  React
                    .Fragment(
                      Button(
                        onClick = props.isEditing.set(IsEditing.False),
                        label = "Cancel",
                        icon = Icons.Close,
                        tooltip = "Cancel sequence editing",
                        tooltipOptions = TooltipOptions.Top,
                        severity = Button.Severity.Danger
                      ).mini.compact,
                      Button(
                        onClick = props.isEditing.set(IsEditing.False) >> props.commitEdit,
                        label = "Accept",
                        icon = Icons.Checkmark,
                        tooltip = "Accept sequence modifications",
                        tooltipOptions = TooltipOptions.Top,
                        severity = Button.Severity.Success
                      ).mini.compact
                    )
                    .when(props.isEditing.get)
                )
              )
            .getOrElse(executed)
        )
      )
