// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.DaytimeCalibrationSets.*
import explore.model.GmosDaytimeSetup
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet

private[config] object GmosDaytimeCalibrationColumns:
  private val BinningColumnId: ColumnId = ColumnId("binning")
  private val RoiColumnId: ColumnId     = ColumnId("roi")

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
      List(
        exposureColumn(editable)(setup(_).zoom(site.exposure)),
        optionalDropdownColumn(FpuColumnId, "FPU", 110, editable, _ => "None", show = withOptics)(
          setup(_).zoom(site.fpu)
        ),
        optionalDropdownColumn(
          GratingColumnId,
          "Grating",
          110,
          editable,
          _ => "Mirror",
          showClear = r => !needsDisperser(r._1.kind),
          show = withOptics
        )(setup(_).zoom(site.grating)),
        wavelengthColumn(units, editable)(
          idx =>
            site.grating
              .get(setup(idx).get)
              .as(
                setup(idx).zoom(d => site.wavelength.get(d).getOrElse(DefaultWavelength))(f =>
                  site.wavelength.modify(_.map(f))
                )
              ),
          idx => site.centralWavelength(setup(idx).get)
        ),
        optionalDropdownColumn(
          FilterColumnId,
          "Filter",
          100,
          editable,
          _ => "None",
          show = withOptics
        )(setup(_).zoom(site.filter)),
        dropdownColumn(ReadModeColumnId, "Read Mode", 110, editable)(
          setup(_).zoom(site.readout.andThen(ReadMode.fromCcdMode))
        ),
        dropdownColumn(BinningColumnId, "Binning", 70, editable)(
          setup(_).zoom(site.readout.andThen(Binning.fromCcdMode))
        ),
        dropdownColumn(RoiColumnId, "ROI", 90, editable)(setup(_).zoom(site.roi))
      )
    )
