// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import clue.data.syntax.*
import crystal.react.View
import explore.common.Aligner
import explore.config.offsets.PresettableTelescopeConfigsEditor
import explore.model.Help
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.sequence.gmos.ifu.northIfuTelescopeConfigPresets
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

case class GmosNorthIfuPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosNorthIfu, GmosNorthIfuInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosNorthIfuPanel](GmosNorthIfuPanel.component)
    with GmosSpectroscopyPanelProps[GmosNorthGrating, GmosNorthFilter, GmosNorthIfuFpu]:

  private val Mode = ObservingMode.GmosNorthIfu

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosNorthGrating                         = Mode.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosNorthFilter]                   = Mode.initialFilter.get(observingMode.get)
  def initialFpu: GmosNorthIfuFpu                              = Mode.initialFpu.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    Mode.initialCentralWavelength.andThen(CentralWavelength.Value).get(observingMode.get)
  def defaultXBinning: GmosXBinning                            = Mode.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            = Mode.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (Mode.defaultAmpReadMode.get(observingMode.get), Mode.defaultAmpGain.get(observingMode.get))
  def defaultRoi: GmosRoi                                      = Mode.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    Mode.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    (Mode.explicitAmpReadMode
       .get(observingMode.get)
       .getOrElse(Mode.defaultAmpReadMode.get(observingMode.get)),
     Mode.explicitAmpGain
       .get(observingMode.get)
       .getOrElse(Mode.defaultAmpGain.get(observingMode.get))
    )

  // Every aperture is offered; the mode has no unavailable ones.
  def excludedFpus: Set[GmosNorthIfuFpu] = Set.empty
  def fpuLabel: String                   = "IFU"
  def fpuHelpId: Option[Help.Id]         = Some("configuration/gmos/ifu.md".refined)

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(Mode.centralWavelength.andThen(CentralWavelength.Value),
            GmosNorthIfuInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosNorthGrating] =
    observingMode.zoom(Mode.grating, GmosNorthIfuInput.grating.modify).view(_.assign)

  def filterView: View[Option[GmosNorthFilter]] =
    observingMode.zoom(Mode.filter, GmosNorthIfuInput.filter.modify).view(_.orUnassign)

  def fpuView: View[GmosNorthIfuFpu] =
    observingMode.zoom(Mode.fpu, GmosNorthIfuInput.fpu.modify).view(_.assign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(Mode.explicitXBin, GmosNorthIfuInput.explicitXBin.modify)
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(Mode.explicitYBin, GmosNorthIfuInput.explicitYBin.modify)
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner: Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthIfuInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(Mode.explicitAmpReadMode, Mode.explicitAmpGain),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosNorthIfuInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosNorthIfuInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode.zoom(Mode.explicitRoi, GmosNorthIfuInput.explicitRoi.modify).view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(Mode.explicitWavelengthDithers, GmosNorthIfuInput.explicitWavelengthDithers.modify)
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(Mode.exposureTimeMode, GmosNorthIfuInput.exposureTimeMode.modify)
      .view(_.toInput.assign)

  // The IFU has its own sky bundle 60" away, so it does not nod: the positions are a plain list.
  // The presets are keyed by plain name and depend on the FPU (one slit vs two slits).
  def offsetsControl(disabled: Boolean): VdomNode =
    PresettableTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(Mode.explicitTelescopeConfigs, GmosNorthIfuInput.explicitTelescopeConfigs.modify)
        .view(_.map(_.toList.map(_.toInput)).orUnassign)
        .removeOptionality(Mode.defaultTelescopeConfigs.get(observingMode.get)),
      presets = northIfuTelescopeConfigPresets(Mode.fpu.get(observingMode.get)),
      helpId = "configuration/mos-spatial-offsets.md".refined,
      defaultConfigs = Mode.defaultTelescopeConfigs.get(observingMode.get),
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  def maskControl: VdomNode = EmptyVdom

  // How the ITC samples the field: unique to the IFU, so it goes in the shared form's mode slot.
  override def modeSpecificFields(disabled: Boolean): VdomNode =
    GmosIfuAnalysisEditor(
      analysis = observingMode
        .zoom(Mode.explicitIfuAnalysis, GmosNorthIfuInput.explicitIfuAnalysis.modify)
        .view(_.map(_.toInput).orUnassign)
        .removeOptionality(Mode.defaultIfuAnalysis.get(observingMode.get)),
      default = Mode.defaultIfuAnalysis.get(observingMode.get),
      readonly = disabled,
      showCustomization = showCustomization,
      allowRevertCustomization = allowRevertCustomization
    )

  private def acquisition
    : Aligner[ObservingMode.GmosNorthIfu.Acquisition, GmosNorthIfuAcquisitionInput] =
    observingMode.zoom(
      Mode.acquisition,
      forceAssign(GmosNorthIfuInput.acquisition.modify)(GmosNorthIfuAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val ModeAcq                                                     = ObservingMode.GmosNorthIfu.Acquisition
    val defaultAcquisitionFilter                                    =
      Mode.acquisition.andThen(ModeAcq.defaultFilter).get(observingMode.get)
    val defaultAcquisitionRoi                                       = Mode.acquisition.andThen(ModeAcq.defaultRoi).get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosNorthFilter]            =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosNorthFilter]]    =
      acquisition
        .zoom(ModeAcq.explicitFilter, GmosNorthIfuAcquisitionInput.explicitFilter.modify)
        .view(_.orUnassign)
    val explicitAcquisitionRoi: View[Option[GmosIfuAcquisitionRoi]] =
      acquisition
        .zoom(ModeAcq.explicitRoi, GmosNorthIfuAcquisitionInput.explicitRoi.modify)
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]         =
      acquisition
        .zoom(ModeAcq.exposureTimeMode, GmosNorthIfuAcquisitionInput.exposureTimeMode.modify)
        .view(_.toInput.assign)
    gmosAcqPanel(
      this,
      disabled,
      explicitAcquisitionRoi.withDefault(defaultAcquisitionRoi),
      defaultAcquisitionRoi,
      explicitAcquisitionFilter.withDefault(defaultAcquisitionFilter),
      defaultAcquisitionFilter,
      excludedAcquisitionFilters,
      acquisitionExposureTimeMode
    )

object GmosNorthIfuPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosNorthGrating,
      GmosNorthFilter,
      GmosNorthIfuFpu,
      GmosNorthIfuPanel
    ]
