// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.cats.given
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.GnirsDaytimeSetup
import explore.model.GnirsDaytimeSetup.given
import explore.model.enums.WavelengthUnits
import explore.model.inputCodecs
import japgolly.scalajs.react.ReactCats.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet

private[config] object GnirsDaytimeCalibrationColumns:
  private val CameraColumnId: ColumnId = ColumnId("camera")
  private val PrismColumnId: ColumnId  = ColumnId("prism")
  private val DeckerColumnId: ColumnId = ColumnId("decker")
  private val CoaddsColumnId: ColumnId = ColumnId("coadds")

  def apply(
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean,
    units:    WavelengthUnits
  ): List[ColDef.Type] =
    val setup: Int => View[GnirsDynamicConfig] =
      setupView(setsView, GnirsDaytimeSetup.setup, GnirsDaytimeSetup.default)

    def mirrorOut(idx: Int): Boolean = GnirsDaytimeSetup.grating.get(setup(idx).get).isDefined

    columns(
      Instrument.Gnirs,
      setsView,
      editable,
      List(
        exposureColumn(editable)(setup(_).zoom(GnirsDaytimeSetup.exposure)),
        // Smart GCAL supplies a smart row's coadds, like its exposure.
        inputColumn(CoaddsColumnId, "Coadds", 60, editable, inputCodecs.PositiveInt, withExposure)(
          setup(_).zoom(GnirsDynamicConfig.coadds)
        ),
        dropdownColumn(CameraColumnId, "Camera", 100, editable, withOptics)(
          setup(_).zoom(GnirsDaytimeSetup.camera)
        ),
        optionalDropdownColumn(
          GratingColumnId,
          "Grating",
          100,
          editable,
          _ => "Mirror",
          showClear = r => !GnirsDaytimeSetup.needsMirrorOut(r._1.kind),
          show = withOptics
        )(setup(_).zoom(GnirsDaytimeSetup.grating)),
        dropdownColumn(
          PrismColumnId,
          "Prism",
          90,
          editable,
          r => withOptics(r) && mirrorOut(r._2)
        )(setup(_).zoom(GnirsDaytimeSetup.prism)),
        wavelengthColumn(units, editable)(
          setup(_).zoom(GnirsDaytimeSetup.wavelength).asView,
          idx => Some(setup(idx).get.centralWavelength)
        ),
        dropdownColumn(FilterColumnId, "Filter", 110, editable, withOptics)(
          setup(_).zoom(GnirsDaytimeSetup.filter)
        ),
        dropdownColumn(FpuColumnId, "FPU", 110, editable, withOptics)(
          setup(_).zoom(GnirsDaytimeSetup.fpu)
        ),
        dropdownColumn(DeckerColumnId, "Decker", 100, editable, withOptics)(
          setup(_).zoom(GnirsDynamicConfig.decker)
        ),
        dropdownColumn(ReadModeColumnId, "Read Mode", 100, editable)(
          setup(_).zoom(GnirsDynamicConfig.readMode)
        )
      )
    )
