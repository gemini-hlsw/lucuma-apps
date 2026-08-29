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

  private val M = ObservingMode.GmosNorthIfu

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosNorthGrating                         = M.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosNorthFilter]                   = M.initialFilter.get(observingMode.get)
  def initialFpu: GmosNorthIfuFpu                              = M.initialFpu.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    M.initialCentralWavelength.andThen(CentralWavelength.Value).get(observingMode.get)
  def defaultXBinning: GmosXBinning                            = M.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            = M.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (M.defaultAmpReadMode.get(observingMode.get), M.defaultAmpGain.get(observingMode.get))
  def defaultRoi: GmosRoi                                      = M.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    M.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    (M.explicitAmpReadMode
       .get(observingMode.get)
       .getOrElse(M.defaultAmpReadMode.get(observingMode.get)),
     M.explicitAmpGain.get(observingMode.get).getOrElse(M.defaultAmpGain.get(observingMode.get))
    )

  // Every aperture is offered; the mode has no unavailable ones.
  def excludedFpus: Set[GmosNorthIfuFpu] = Set.empty
  def fpuLabel: String                   = "IFU"
  def fpuHelpId: Option[Help.Id]         = None

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(M.centralWavelength.andThen(CentralWavelength.Value),
            GmosNorthIfuInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosNorthGrating] =
    observingMode.zoom(M.grating, GmosNorthIfuInput.grating.modify).view(_.assign)

  def filterView: View[Option[GmosNorthFilter]] =
    observingMode.zoom(M.filter, GmosNorthIfuInput.filter.modify).view(_.orUnassign)

  def fpuView: View[GmosNorthIfuFpu] =
    observingMode.zoom(M.fpu, GmosNorthIfuInput.fpu.modify).view(_.assign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(M.explicitXBin, GmosNorthIfuInput.explicitXBin.modify)
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(M.explicitYBin, GmosNorthIfuInput.explicitYBin.modify)
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner: Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthIfuInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(M.explicitAmpReadMode, M.explicitAmpGain),
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
    observingMode.zoom(M.explicitRoi, GmosNorthIfuInput.explicitRoi.modify).view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(M.explicitWavelengthDithers, GmosNorthIfuInput.explicitWavelengthDithers.modify)
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(M.exposureTimeMode, GmosNorthIfuInput.exposureTimeMode.modify)
      .view(_.toInput.assign)

  // The IFU has its own sky bundle 60" away, so it does not nod: the positions are a plain list
  // and the only preset is the default.
  def offsetsControl(disabled: Boolean): VdomNode =
    PresettableTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(M.explicitTelescopeConfigs, GmosNorthIfuInput.explicitTelescopeConfigs.modify)
        .view(_.map(_.toList.map(_.toInput)).orUnassign)
        .removeOptionality(M.defaultTelescopeConfigs.get(observingMode.get)),
      presets = NonEmptyList.one("Default" -> M.defaultTelescopeConfigs.get(observingMode.get)),
      helpId = "configuration/mos-spatial-offsets.md".refined,
      defaultConfigs = M.defaultTelescopeConfigs.get(observingMode.get),
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  def maskControl: VdomNode = EmptyVdom

  private def acquisition
    : Aligner[ObservingMode.GmosNorthIfu.Acquisition, GmosNorthIfuAcquisitionInput] =
    observingMode.zoom(
      M.acquisition,
      forceAssign(GmosNorthIfuInput.acquisition.modify)(GmosNorthIfuAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val A                                                           = ObservingMode.GmosNorthIfu.Acquisition
    val defaultAcquisitionFilter                                    = M.acquisition.andThen(A.defaultFilter).get(observingMode.get)
    val defaultAcquisitionRoi                                       = M.acquisition.andThen(A.defaultRoi).get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosNorthFilter]            =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosNorthFilter]]    =
      acquisition
        .zoom(A.explicitFilter, GmosNorthIfuAcquisitionInput.explicitFilter.modify)
        .view(_.orUnassign)
    val explicitAcquisitionRoi: View[Option[GmosIfuAcquisitionRoi]] =
      acquisition
        .zoom(A.explicitRoi, GmosNorthIfuAcquisitionInput.explicitRoi.modify)
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]         =
      acquisition
        .zoom(A.exposureTimeMode, GmosNorthIfuAcquisitionInput.exposureTimeMode.modify)
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
