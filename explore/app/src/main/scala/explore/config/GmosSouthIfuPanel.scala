// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.sequence.gmos.ifu.southIfuTelescopeConfigPresets
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

case class GmosSouthIfuPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosSouthIfu, GmosSouthIfuInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosSouthIfuPanel](GmosSouthIfuPanel.component)
    with GmosSpectroscopyPanelProps[GmosSouthGrating, GmosSouthFilter, GmosSouthIfuFpu]:

  private val Mode = ObservingMode.GmosSouthIfu

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosSouthGrating                         = Mode.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosSouthFilter]                   = Mode.initialFilter.get(observingMode.get)
  def initialFpu: GmosSouthIfuFpu                              = Mode.initialFpu.get(observingMode.get)
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
  def excludedFpus: Set[GmosSouthIfuFpu] = Set.empty
  def fpuLabel: String                   = "IFU"
  def fpuHelpId: Option[Help.Id]         = Some("configuration/gmos/ifu.md".refined)

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(
        Mode.centralWavelength.andThen(CentralWavelength.Value),
        GmosSouthIfuInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosSouthGrating] =
    observingMode.zoom(Mode.grating, GmosSouthIfuInput.grating.modify).view(_.assign)

  def filterView: View[Option[GmosSouthFilter]] =
    observingMode.zoom(Mode.filter, GmosSouthIfuInput.filter.modify).view(_.orUnassign)

  def fpuView: View[GmosSouthIfuFpu] =
    observingMode.zoom(Mode.fpu, GmosSouthIfuInput.fpu.modify).view(_.assign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(Mode.explicitXBin, GmosSouthIfuInput.explicitXBin.modify)
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(Mode.explicitYBin, GmosSouthIfuInput.explicitYBin.modify)
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner: Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthIfuInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(Mode.explicitAmpReadMode, Mode.explicitAmpGain),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosSouthIfuInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosSouthIfuInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode.zoom(Mode.explicitRoi, GmosSouthIfuInput.explicitRoi.modify).view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(Mode.explicitWavelengthDithers, GmosSouthIfuInput.explicitWavelengthDithers.modify)
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(Mode.exposureTimeMode, GmosSouthIfuInput.exposureTimeMode.modify)
      .view(_.toInput.assign)

  // The IFU has its own sky bundle 60" away, so it does not nod: the positions are a plain list.
  // The presets are keyed by plain name and depend on the FPU (one slit vs two slits).
  def offsetsControl(disabled: Boolean): VdomNode =
    PresettableTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(Mode.explicitTelescopeConfigs, GmosSouthIfuInput.explicitTelescopeConfigs.modify)
        .view(_.map(_.toList.map(_.toInput)).orUnassign)
        .removeOptionality(Mode.defaultTelescopeConfigs.get(observingMode.get)),
      presets = southIfuTelescopeConfigPresets(Mode.fpu.get(observingMode.get)),
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
        .zoom(Mode.explicitIfuAnalysis, GmosSouthIfuInput.explicitIfuAnalysis.modify)
        .view(_.map(_.toInput).orUnassign)
        .removeOptionality(Mode.defaultIfuAnalysis.get(observingMode.get)),
      default = Mode.defaultIfuAnalysis.get(observingMode.get),
      readonly = disabled,
      showCustomization = showCustomization,
      allowRevertCustomization = allowRevertCustomization
    )

  private def acquisition
    : Aligner[ObservingMode.GmosSouthIfu.Acquisition, GmosSouthIfuAcquisitionInput] =
    observingMode.zoom(
      Mode.acquisition,
      forceAssign(GmosSouthIfuInput.acquisition.modify)(GmosSouthIfuAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val ModeAcq                                                     = ObservingMode.GmosSouthIfu.Acquisition
    val defaultAcquisitionFilter                                    =
      Mode.acquisition.andThen(ModeAcq.defaultFilter).get(observingMode.get)
    val defaultAcquisitionRoi                                       = Mode.acquisition.andThen(ModeAcq.defaultRoi).get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosSouthFilter]            =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosSouthFilter]]    =
      acquisition
        .zoom(ModeAcq.explicitFilter, GmosSouthIfuAcquisitionInput.explicitFilter.modify)
        .view(_.orUnassign)
    val explicitAcquisitionRoi: View[Option[GmosIfuAcquisitionRoi]] =
      acquisition
        .zoom(ModeAcq.explicitRoi, GmosSouthIfuAcquisitionInput.explicitRoi.modify)
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]         =
      acquisition
        .zoom(ModeAcq.exposureTimeMode, GmosSouthIfuAcquisitionInput.exposureTimeMode.modify)
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

object GmosSouthIfuPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosSouthGrating,
      GmosSouthFilter,
      GmosSouthIfuFpu,
      GmosSouthIfuPanel
    ]
