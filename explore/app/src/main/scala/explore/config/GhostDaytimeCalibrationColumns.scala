// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.cats.given
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.DaytimeCalibrationSets
import explore.model.GhostDaytimeSetup
import explore.model.inputCodecs
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.react.common.Css
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.ui.LucumaStyles
import lucuma.ui.display.given
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sequence.SequenceStyles
import monocle.Lens

private[config] object GhostDaytimeCalibrationColumns:
  def apply(
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean
  ): List[ColDef.Type] =
    val setup: Int => View[GhostDynamicConfig] =
      setupView(setsView, GhostDaytimeSetup.setup, GhostDaytimeSetup.default)

    // Smart GCAL supplies a smart row's exposure time and count for both detectors.
    def detectorColumns(
      name:     String,
      groupId:  ColumnId,
      color:    Css,
      detector: Lens[GhostDynamicConfig, GhostDetector]
    ): ColDef.Type =
      def colId(field: String): ColumnId = ColumnId(s"$name-$field")
      def view(idx: Int): View[GhostDetector] = setup(idx).zoom(detector)

      ColDef.group(
        groupId,
        header = _ => <.span(color)(name.capitalize),
        columns = List(
          ColDef(
            colId("exposure"),
            header = "Exp (s)",
            cell = c =>
              val idx: Int = c.row.original._2
              if DaytimeCalibrationSets.usesExposure(c.row.original._1.kind) then
                FormInputTextView(
                  id = cellId(colId("exposure"), idx),
                  value = view(idx).zoom(GhostDaytimeSetup.exposureTime),
                  codec = inputCodecs.GhostCalibrationExposure,
                  groupClass = SequenceStyles.SequenceInput,
                  disabled = !editable
                )
              else EmptyVdom
          ).withSize(70.toPx),
          ColDef(
            colId("count"),
            header = "#",
            cell = c =>
              val idx: Int = c.row.original._2
              if c.row.original._1.kind.isSmart then EmptyVdom
              else
                FormInputTextView(
                  id = cellId(colId("count"), idx),
                  value = view(idx).zoom(GhostDaytimeSetup.exposureCount),
                  codec = inputCodecs.PositiveInt,
                  groupClass = SequenceStyles.SequenceInput,
                  disabled = !editable
                )
          ).withSize(50.toPx),
          ColDef(
            colId("readMode"),
            header = "Read",
            cell = c =>
              val idx: Int = c.row.original._2
              dropdown(colId("readMode"), idx, view(idx).zoom(GhostDaytimeSetup.readMode), editable)
          ).withSize(80.toPx),
          ColDef(
            colId("binning"),
            header = "Bin",
            cell = c =>
              val idx: Int = c.row.original._2
              dropdown(colId("binning"), idx, view(idx).zoom(GhostDaytimeSetup.binning), editable)
          ).withSize(70.toPx)
        )
      )

    columns(
      Instrument.Ghost,
      setsView,
      editable,
      None,
      List(
        detectorColumns("red", RedGroupColumnId, LucumaStyles.GhostRed, GhostDaytimeSetup.red),
        detectorColumns("blue", BlueGroupColumnId, LucumaStyles.GhostBlue, GhostDaytimeSetup.blue)
      )
    )
