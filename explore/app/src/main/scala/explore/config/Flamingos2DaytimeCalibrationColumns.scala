// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.Flamingos2DaytimeSetup
import explore.model.display.given
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.ui.display.given

private[config] object Flamingos2DaytimeCalibrationColumns:
  private val DeckerColumnId: ColumnId      = ColumnId("decker")
  private val ReadsColumnId: ColumnId       = ColumnId("reads")
  private val WindowCoverColumnId: ColumnId = ColumnId("windowCover")

  def apply(
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean
  ): List[ColDef.Type] =
    val setup: Int => View[Flamingos2DynamicConfig] =
      setupView(setsView, Flamingos2DaytimeSetup.setup, Flamingos2DaytimeSetup.default)

    columns(
      Instrument.Flamingos2,
      setsView,
      editable,
      setup(_).zoom(Flamingos2DynamicConfig.exposure),
      List(
        ColDef(
          FilterColumnId,
          header = "Filter",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                FilterColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.filter),
                editable
              )
        ).withSize(90.toPx),
        ColDef(
          GratingColumnId,
          header = "Grating",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                GratingColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.disperser),
                "None",
                editable
              )
        ).withSize(100.toPx),
        ColDef(
          FpuColumnId,
          header = "FPU",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FpuColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DaytimeSetup.fpu),
                if Flamingos2FpuMask.custom.nonEmpty(setup(c.row.original._2).get.fpu) then
                  "Custom Mask"
                else "Imaging",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          DeckerColumnId,
          header = "Decker",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                DeckerColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(Flamingos2DynamicConfig.decker),
                editable
              )
        ).withSize(90.toPx),
        ColDef(
          ReadModeColumnId,
          header = "Read Mode",
          cell = c =>
            dropdown(
              ReadModeColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DaytimeSetup.readMode),
              editable
            )
        ).withSize(90.toPx),
        ColDef(
          ReadsColumnId,
          header = "Reads",
          cell = c =>
            dropdown(
              ReadsColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DynamicConfig.reads),
              editable
            )
        ).withSize(70.toPx),
        ColDef(
          WindowCoverColumnId,
          header = "Window Cover",
          cell = c =>
            optionalDropdown(
              WindowCoverColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(Flamingos2DynamicConfig.windowCover),
              "Auto",
              editable
            )
        ).withSize(90.toPx)
      )
    )
