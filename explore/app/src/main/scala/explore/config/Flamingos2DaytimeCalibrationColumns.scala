// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.DaytimeCalibrationSets
import explore.model.Flamingos2DaytimeSetup
import explore.model.display.given
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet

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
      List(
        exposureColumn(editable)(setup(_).zoom(Flamingos2DynamicConfig.exposure)),
        dropdownColumn(FilterColumnId, "Filter", 90, editable, withOptics)(
          setup(_).zoom(Flamingos2DynamicConfig.filter)
        ),
        optionalDropdownColumn(
          GratingColumnId,
          "Grating",
          100,
          editable,
          _ => "None",
          showClear = r => !DaytimeCalibrationSets.needsDisperser(r._1.kind),
          show = withOptics
        )(setup(_).zoom(Flamingos2DynamicConfig.disperser)),
        optionalDropdownColumn(
          FpuColumnId,
          "FPU",
          110,
          editable,
          r =>
            if Flamingos2FpuMask.custom.nonEmpty(setup(r._2).get.fpu) then "Custom Mask"
            else "Imaging",
          show = withOptics
        )(setup(_).zoom(Flamingos2DaytimeSetup.fpu)),
        dropdownColumn(DeckerColumnId, "Decker", 90, editable, withOptics)(
          setup(_).zoom(Flamingos2DynamicConfig.decker)
        ),
        dropdownColumn(ReadModeColumnId, "Read Mode", 90, editable)(
          setup(_).zoom(Flamingos2DaytimeSetup.readMode)
        ),
        dropdownColumn(ReadsColumnId, "Reads", 70, editable)(
          setup(_).zoom(Flamingos2DynamicConfig.reads)
        ),
        optionalDropdownColumn(WindowCoverColumnId, "Window Cover", 90, editable, _ => "Auto")(
          setup(_).zoom(Flamingos2DynamicConfig.windowCover)
        )
      )
    )
