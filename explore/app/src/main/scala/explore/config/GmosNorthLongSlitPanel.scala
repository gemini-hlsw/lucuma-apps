// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import explore.common.Aligner
import explore.config.offsets.SlitTelescopeConfigsEditor
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
import lucuma.core.model.sequence.gmos.longslit.defaultSlitTelescopeConfigs
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

case class GmosNorthLongSlitPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosNorthLongSlitPanel](GmosNorthLongSlitPanel.component)
    with GmosLongSlitPanelProps[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosNorthGrating                         =
    ObservingMode.GmosNorthLongSlit.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosNorthFilter]                   =
    ObservingMode.GmosNorthLongSlit.initialFilter.get(observingMode.get)
  def initialFpu: GmosNorthFpu                                 =
    ObservingMode.GmosNorthLongSlit.initialFpu.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    ObservingMode.GmosNorthLongSlit.initialCentralWavelength
      .andThen(CentralWavelength.Value)
      .get(observingMode.get)
  def defaultXBinning: GmosXBinning                            =
    ObservingMode.GmosNorthLongSlit.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            =
    ObservingMode.GmosNorthLongSlit.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosNorthLongSlit.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosNorthLongSlit.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      =
    ObservingMode.GmosNorthLongSlit.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    ObservingMode.GmosNorthLongSlit.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    val readMode = ObservingMode.GmosNorthLongSlit.explicitAmpReadMode
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpReadMode.get(observingMode.get))
    val ampGain  = ObservingMode.GmosNorthLongSlit.explicitAmpGain
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpGain.get(observingMode.get))
    (readMode, ampGain)

  def excludedFpus: Set[GmosNorthFpu] =
    Enumerated[GmosNorthFpu].all.filter(_.fpuType =!= GmosFpuType.LongSlit).toSet

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(
        ObservingMode.GmosNorthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
        GmosNorthLongSlitInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosNorthGrating] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.grating, GmosNorthLongSlitInput.grating.modify)
      .view(_.assign)

  def filterView: View[Option[GmosNorthFilter]] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.filter, GmosNorthLongSlitInput.filter.modify)
      .view(_.orUnassign)

  def fpuView: View[GmosNorthFpu] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.fpu, GmosNorthLongSlitInput.fpu.modify)
      .view(_.assign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.explicitXBin,
            GmosNorthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.explicitYBin,
            GmosNorthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner
    : Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthLongSlitInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(
        ObservingMode.GmosNorthLongSlit.explicitAmpReadMode,
        ObservingMode.GmosNorthLongSlit.explicitAmpGain
      ),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosNorthLongSlitInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosNorthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode
      .zoom(ObservingMode.GmosNorthLongSlit.explicitRoi, GmosNorthLongSlitInput.explicitRoi.modify)
      .view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitWavelengthDithers,
        GmosNorthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(
        ObservingMode.GmosNorthLongSlit.exposureTimeMode,
        GmosNorthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

  def offsetsControl(disabled: Boolean): VdomNode =
    SlitTelescopeConfigsEditor[GmosSlitOffsetPreset](
      explicitValue = observingMode
        .zoom(
          ObservingMode.GmosNorthLongSlit.explicitTelescopeConfigs,
          GmosNorthLongSlitInput.explicitTelescopeConfigs.modify
        )
        .view(_.map(_.toInput).orUnassign),
      defaultValue = ObservingMode.GmosNorthLongSlit.defaultTelescopeConfigs.get(observingMode.get),
      defaultForPreset = defaultSlitTelescopeConfigs,
      helpId = "configuration/slit-spatial-offsets.md".refined,
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  private def acquisition
    : Aligner[ObservingMode.GmosNorthLongSlit.Acquisition, GmosNorthLongSlitAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GmosNorthLongSlit.acquisition,
      forceAssign(GmosNorthLongSlitInput.acquisition.modify)(GmosNorthLongSlitAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val defaultAcquisitionFilter                                         =
      ObservingMode.GmosNorthLongSlit.acquisition
        .andThen(ObservingMode.GmosNorthLongSlit.Acquisition.defaultFilter)
        .get(observingMode.get)
    val defaultAcquisitionRoi                                            =
      ObservingMode.GmosNorthLongSlit.acquisition
        .andThen(ObservingMode.GmosNorthLongSlit.Acquisition.defaultRoi)
        .get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosNorthFilter]                 =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosNorthFilter]]         =
      acquisition
        .zoom(
          ObservingMode.GmosNorthLongSlit.Acquisition.explicitFilter,
          GmosNorthLongSlitAcquisitionInput.explicitFilter.modify
        )
        .view(_.orUnassign)
    val explicitAcquisitionRoi: View[Option[GmosLongSlitAcquisitionRoi]] =
      acquisition
        .zoom(
          ObservingMode.GmosNorthLongSlit.Acquisition.explicitRoi,
          GmosNorthLongSlitAcquisitionInput.explicitRoi.modify
        )
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]              =
      acquisition
        .zoom(
          ObservingMode.GmosNorthLongSlit.Acquisition.exposureTimeMode,
          GmosNorthLongSlitAcquisitionInput.exposureTimeMode.modify
        )
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

object GmosNorthLongSlitPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosNorthGrating,
      GmosNorthFilter,
      GmosNorthFpu,
      GmosNorthLongSlitPanel
    ]
