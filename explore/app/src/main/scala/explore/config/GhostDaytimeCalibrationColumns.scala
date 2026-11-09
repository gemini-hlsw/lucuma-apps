// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.cats.given
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.GhostDaytimeSetup
import explore.model.inputCodecs
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.react.common.Css
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.ui.LucumaStyles
import lucuma.ui.display.given
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
      def colId(field: String): ColumnId         = ColumnId(s"$name-$field")
      def view(idx:    Int): View[GhostDetector] = setup(idx).zoom(detector)

      ColDef.group(
        groupId,
        header = _ => <.span(color)(name.capitalize),
        columns = List(
          inputColumn(
            colId("exposure"),
            "Exp (s)",
            70,
            editable,
            inputCodecs.GhostCalibrationExposure,
            withExposure
          )(view(_).zoom(GhostDaytimeSetup.exposureTime)),
          inputColumn(
            colId("count"),
            "#",
            50,
            editable,
            inputCodecs.PositiveInt,
            r => !r._1.kind.isSmart
          )(view(_).zoom(GhostDaytimeSetup.exposureCount)),
          dropdownColumn(colId("readMode"), "Read", 80, editable)(
            view(_).zoom(GhostDaytimeSetup.readMode)
          ),
          dropdownColumn(colId("binning"), "Bin", 70, editable)(
            view(_).zoom(GhostDaytimeSetup.binning)
          )
        )
      )

    columns(
      Instrument.Ghost,
      setsView,
      editable,
      List(
        detectorColumns("red", RedGroupColumnId, LucumaStyles.GhostRed, GhostDaytimeSetup.red),
        detectorColumns("blue", BlueGroupColumnId, LucumaStyles.GhostBlue, GhostDaytimeSetup.blue)
      )
    )
