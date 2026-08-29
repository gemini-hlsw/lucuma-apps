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

case class GmosSouthLongSlitPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosSouthLongSlitPanel](GmosSouthLongSlitPanel.component)
    with GmosLongSlitPanelProps[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosSouthGrating                         =
    ObservingMode.GmosSouthLongSlit.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosSouthFilter]                   =
    ObservingMode.GmosSouthLongSlit.initialFilter.get(observingMode.get)
  def initialFpu: GmosSouthFpu                                 =
    ObservingMode.GmosSouthLongSlit.initialFpu.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    ObservingMode.GmosSouthLongSlit.initialCentralWavelength
      .andThen(CentralWavelength.Value)
      .get(observingMode.get)
  def defaultXBinning: GmosXBinning                            =
    ObservingMode.GmosSouthLongSlit.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            =
    ObservingMode.GmosSouthLongSlit.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosSouthLongSlit.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosSouthLongSlit.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      =
    ObservingMode.GmosSouthLongSlit.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    ObservingMode.GmosSouthLongSlit.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    val readMode = ObservingMode.GmosSouthLongSlit.explicitAmpReadMode
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpReadMode.get(observingMode.get))
    val ampGain  = ObservingMode.GmosSouthLongSlit.explicitAmpGain
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpGain.get(observingMode.get))
    (readMode, ampGain)

  def excludedFpus: Set[GmosSouthFpu] =
    Enumerated[GmosSouthFpu].all.filter(_.fpuType =!= GmosFpuType.LongSlit).toSet

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(
        ObservingMode.GmosSouthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
        GmosSouthLongSlitInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosSouthGrating] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.grating, GmosSouthLongSlitInput.grating.modify)
      .view(_.assign)

  def filterView: View[Option[GmosSouthFilter]] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.filter, GmosSouthLongSlitInput.filter.modify)
      .view(_.orUnassign)

  def fpuView: View[GmosSouthFpu] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.fpu, GmosSouthLongSlitInput.fpu.modify)
      .view(_.assign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.explicitXBin,
            GmosSouthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.explicitYBin,
            GmosSouthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner
    : Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthLongSlitInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(
        ObservingMode.GmosSouthLongSlit.explicitAmpReadMode,
        ObservingMode.GmosSouthLongSlit.explicitAmpGain
      ),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosSouthLongSlitInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosSouthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode
      .zoom(ObservingMode.GmosSouthLongSlit.explicitRoi, GmosSouthLongSlitInput.explicitRoi.modify)
      .view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitWavelengthDithers,
        GmosSouthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(
        ObservingMode.GmosSouthLongSlit.exposureTimeMode,
        GmosSouthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

  def offsetsControl(disabled: Boolean): VdomNode =
    SlitTelescopeConfigsEditor[GmosSlitOffsetPreset](
      explicitValue = observingMode
        .zoom(
          ObservingMode.GmosSouthLongSlit.explicitTelescopeConfigs,
          GmosSouthLongSlitInput.explicitTelescopeConfigs.modify
        )
        .view(_.map(_.toInput).orUnassign),
      defaultValue = ObservingMode.GmosSouthLongSlit.defaultTelescopeConfigs.get(observingMode.get),
      defaultForPreset = defaultSlitTelescopeConfigs,
      helpId = "configuration/slit-spatial-offsets.md".refined,
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  private def acquisition
    : Aligner[ObservingMode.GmosSouthLongSlit.Acquisition, GmosSouthLongSlitAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GmosSouthLongSlit.acquisition,
      forceAssign(GmosSouthLongSlitInput.acquisition.modify)(GmosSouthLongSlitAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val defaultAcquisitionFilter                                         =
      ObservingMode.GmosSouthLongSlit.acquisition
        .andThen(ObservingMode.GmosSouthLongSlit.Acquisition.defaultFilter)
        .get(observingMode.get)
    val defaultAcquisitionRoi                                            =
      ObservingMode.GmosSouthLongSlit.acquisition
        .andThen(ObservingMode.GmosSouthLongSlit.Acquisition.defaultRoi)
        .get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosSouthFilter]                 =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosSouthFilter]]         =
      acquisition
        .zoom(
          ObservingMode.GmosSouthLongSlit.Acquisition.explicitFilter,
          GmosSouthLongSlitAcquisitionInput.explicitFilter.modify
        )
        .view(_.orUnassign)
    val explicitAcquisitionRoi: View[Option[GmosLongSlitAcquisitionRoi]] =
      acquisition
        .zoom(
          ObservingMode.GmosSouthLongSlit.Acquisition.explicitRoi,
          GmosSouthLongSlitAcquisitionInput.explicitRoi.modify
        )
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]              =
      acquisition
        .zoom(
          ObservingMode.GmosSouthLongSlit.Acquisition.exposureTimeMode,
          GmosSouthLongSlitAcquisitionInput.exposureTimeMode.modify
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

object GmosSouthLongSlitPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosSouthGrating,
      GmosSouthFilter,
      GmosSouthFpu,
      GmosSouthLongSlitPanel
    ]
