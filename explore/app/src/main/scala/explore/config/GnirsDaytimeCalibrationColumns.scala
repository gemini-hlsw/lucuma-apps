// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.cats.given
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.DaytimeCalibrationSets
import explore.model.GnirsDaytimeSetup
import explore.model.GnirsDaytimeSetup.given
import explore.model.enums.WavelengthUnits
import explore.model.inputCodecs
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sequence.SequenceStyles
import lucuma.ui.syntax.all.given

private[config] object GnirsDaytimeCalibrationColumns:
  private val CameraColumnId: ColumnId     = ColumnId("camera")
  private val PrismColumnId: ColumnId      = ColumnId("prism")
  private val WavelengthColumnId: ColumnId = ColumnId("wavelength")
  private val DeckerColumnId: ColumnId     = ColumnId("decker")
  private val CoaddsColumnId: ColumnId     = ColumnId("coadds")

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
      setup(_).zoom(GnirsDaytimeSetup.exposure),
      List(
        ColDef(
          CoaddsColumnId,
          header = "Coadds",
          // Smart GCAL supplies a smart row's coadds, like its exposure.
          cell = c =>
            if DaytimeCalibrationSets.usesExposure(c.row.original._1.kind) then
              FormInputTextView(
                id = cellId(CoaddsColumnId, c.row.original._2),
                value = setup(c.row.original._2).zoom(GnirsDynamicConfig.coadds),
                codec = inputCodecs.PositiveInt,
                groupClass = SequenceStyles.SequenceInput,
                disabled = !editable
              )
            else EmptyVdom
        ).withSize(60.toPx),
        ColDef(
          CameraColumnId,
          header = "Camera",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                CameraColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(GnirsDaytimeSetup.camera),
                editable
              )
        ).withSize(100.toPx),
        ColDef(
          GratingColumnId,
          header = "Grating",
          cell = c =>
            opticsCell(c.row.original):
              optionalDropdown(
                GratingColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(GnirsDaytimeSetup.grating),
                "Mirror",
                editable,
                showClear = !GnirsDaytimeSetup.needsMirrorOut(c.row.original._1.kind)
              )
        ).withSize(100.toPx),
        ColDef(
          PrismColumnId,
          header = "Prism",
          cell = c =>
            val idx: Int = c.row.original._2
            opticsCell(c.row.original):
              if mirrorOut(idx) then
                dropdown(PrismColumnId, idx, setup(idx).zoom(GnirsDaytimeSetup.prism), editable)
              else EmptyVdom
        ).withSize(90.toPx),
        ColDef(
          WavelengthColumnId,
          header = s"λ (${units.symbol})",
          cell = c =>
            val idx: Int = c.row.original._2
            // Editable with the mirror out; otherwise the filter's wavelength, read-only.
            opticsCell(c.row.original):
              setup(idx)
                .zoom(GnirsDaytimeSetup.wavelength)
                .asView
                .fold[VdomNode](
                  inputCodecs.wavelengthValid(units).reverseGet(setup(idx).get.centralWavelength)
                ): wavelength =>
                  FormInputTextView(
                    id = cellId(WavelengthColumnId, idx),
                    value = wavelength,
                    codec = inputCodecs.wavelength(units),
                    groupClass = SequenceStyles.SequenceInput,
                    disabled = !editable
                  )
        ).withSize(80.toPx),
        ColDef(
          FilterColumnId,
          header = "Filter",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                FilterColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(GnirsDaytimeSetup.filter),
                editable
              )
        ).withSize(110.toPx),
        ColDef(
          FpuColumnId,
          header = "FPU",
          cell = c =>
            opticsCell(c.row.original):
              dropdown(
                FpuColumnId,
                c.row.original._2,
                setup(c.row.original._2).zoom(GnirsDaytimeSetup.fpu),
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
                setup(c.row.original._2).zoom(GnirsDynamicConfig.decker),
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
              setup(c.row.original._2).zoom(GnirsDynamicConfig.readMode),
              editable
            )
        ).withSize(100.toPx)
      )
    )
