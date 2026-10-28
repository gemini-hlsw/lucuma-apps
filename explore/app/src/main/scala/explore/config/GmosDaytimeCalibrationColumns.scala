// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.DaytimeCalibrationSets.*
import explore.model.GmosDaytimeSetup
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.inputCodecs
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sequence.SequenceStyles
import lucuma.ui.syntax.all.given

private[config] object GmosDaytimeCalibrationColumns:
  private val WavelengthColumnId: ColumnId = ColumnId("wavelength")
  private val BinningColumnId: ColumnId    = ColumnId("binning")
  private val RoiColumnId: ColumnId        = ColumnId("roi")

  /** The columns of one GMOS site, generic in its grating, filter and FPU types. */
  def apply[
    D,
    G: Enumerated: Display,
    F: Enumerated: Display,
    U: Enumerated: Display
  ](
    site:     GmosDaytimeSetup[D, G, F, U],
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean,
    units:    WavelengthUnits
  ): List[ColDef.Type] =
    val setup: Int => View[D] = setupView(setsView, site.setup, site.default)

    columns(
      site.instrument,
      setsView,
      editable,
      setup(_).zoom(site.exposure),
      List(
        ColDef(
          FpuColumnId,
          header = "FPU",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FpuColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.fpu),
                "None",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          GratingColumnId,
          header = "Grating",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                GratingColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.grating),
                "Mirror",
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          WavelengthColumnId,
          header = s"λ (${units.symbol})",
          cell = c =>
            val idx: Int = c.row.original._2
            val d: D     = setup(idx).get
            // Editable with a disperser; otherwise the filter's wavelength, read-only.
            opticsCell(c.row.original):
              site.grating.get(d) match
                case Some(_) =>
                  FormInputTextView(
                    id = cellId(WavelengthColumnId, idx),
                    value = setup(idx).zoom(d =>
                      site.wavelength.get(d).getOrElse(DefaultWavelength)
                    )(f => site.wavelength.modify(_.map(f))),
                    codec = inputCodecs.wavelength(units),
                    groupClass = SequenceStyles.SequenceInput,
                    disabled = !editable
                  )
                case None    =>
                  site
                    .centralWavelength(d)
                    .fold(EmptyVdom)(w =>
                      inputCodecs.wavelengthValid(units).reverseGet(w): VdomNode
                    )
        ).withSize(80.toPx),
        ColDef(
          FilterColumnId,
          header = "Filter",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                FilterColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(site.filter),
                "None",
                editable
              )
        ).withSize(100.toPx),
        ColDef(
          ReadModeColumnId,
          header = "Read Mode",
          cell = c =>
            dropdown(
              ReadModeColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.readout.andThen(ReadMode.fromCcdMode)),
              editable
            )
        ).withSize(110.toPx),
        ColDef(
          BinningColumnId,
          header = "Binning",
          cell = c =>
            dropdown(
              BinningColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.readout.andThen(Binning.fromCcdMode)),
              editable
            )
        ).withSize(70.toPx),
        ColDef(
          RoiColumnId,
          header = "ROI",
          cell = c =>
            dropdown(
              RoiColumnId,
              c.row.original._2,
              setup(c.row.original._2).zoom(site.roi),
              editable
            )
        ).withSize(90.toPx)
      )
    )
