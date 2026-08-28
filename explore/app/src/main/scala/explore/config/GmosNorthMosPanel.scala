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
import explore.model.Attachment
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

case class GmosNorthMosPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosNorthMos, GmosNorthMosInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits,
  maskContext:     MosMaskContext
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosNorthMosPanel](GmosNorthMosPanel.component)
    with GmosMosPanelProps[GmosNorthGrating, GmosNorthFilter]:

  val maskInstrument: Instrument = Instrument.GmosNorth

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosNorthGrating                         =
    ObservingMode.GmosNorthMos.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosNorthFilter]                   =
    ObservingMode.GmosNorthMos.initialFilter.get(observingMode.get)
  def initialFpu: GmosCustomSlitWidth                          =
    ObservingMode.GmosNorthMos.initialSlitWidth.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    ObservingMode.GmosNorthMos.initialCentralWavelength
      .andThen(CentralWavelength.Value)
      .get(observingMode.get)
  def defaultXBinning: GmosXBinning                            =
    ObservingMode.GmosNorthMos.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            =
    ObservingMode.GmosNorthMos.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosNorthMos.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosNorthMos.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      =
    ObservingMode.GmosNorthMos.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    ObservingMode.GmosNorthMos.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    val readMode = ObservingMode.GmosNorthMos.explicitAmpReadMode
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosNorthMos.defaultAmpReadMode.get(observingMode.get))
    val ampGain  = ObservingMode.GmosNorthMos.explicitAmpGain
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosNorthMos.defaultAmpGain.get(observingMode.get))
    (readMode, ampGain)

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(
        ObservingMode.GmosNorthMos.centralWavelength.andThen(CentralWavelength.Value),
        GmosNorthMosInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosNorthGrating] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.grating, GmosNorthMosInput.grating.modify)
      .view(_.assign)

  def filterView: View[Option[GmosNorthFilter]] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.filter, GmosNorthMosInput.filter.modify)
      .view(_.orUnassign)

  // The Input's customMask is optional and its slitWidth required.
  private def customMask: Aligner[ObservingMode.GmosCustomMask, GmosCustomMaskInput] =
    observingMode.zoom(
      ObservingMode.GmosNorthMos.customMask,
      forceAssign(GmosNorthMosInput.customMask.modify)(observingMode.get.customMask.toInput)
    )

  def fpuView: View[GmosCustomSlitWidth] =
    customMask
      .zoom(ObservingMode.GmosCustomMask.slitWidth, GmosCustomMaskInput.slitWidth.modify)
      .view(identity)

  def customMaskAttachmentIdView: View[Option[Attachment.Id]] =
    customMask
      .zoom(ObservingMode.GmosCustomMask.attachmentId, GmosCustomMaskInput.attachmentId.modify)
      .view(_.orUnassign)

  def explicitXBinningView: View[Option[GmosXBinning]] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.explicitXBin, GmosNorthMosInput.explicitXBin.modify)
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.explicitYBin, GmosNorthMosInput.explicitYBin.modify)
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner: Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthMosInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(
        ObservingMode.GmosNorthMos.explicitAmpReadMode,
        ObservingMode.GmosNorthMos.explicitAmpGain
      ),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosNorthMosInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosNorthMosInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.explicitRoi, GmosNorthMosInput.explicitRoi.modify)
      .view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(
        ObservingMode.GmosNorthMos.explicitWavelengthDithers,
        GmosNorthMosInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.exposureTimeMode, GmosNorthMosInput.exposureTimeMode.modify)
      .view(_.toInput.assign)

  def offsetsControl(disabled: Boolean): VdomNode =
    // A MOS mask has no single slit to nod along, so the positions are a plain list
    // and the only preset is the default.
    PresettableTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(
          ObservingMode.GmosNorthMos.explicitTelescopeConfigs,
          GmosNorthMosInput.explicitTelescopeConfigs.modify
        )
        .view(_.map(_.toList.map(_.toInput)).orUnassign)
        .removeOptionality(
          ObservingMode.GmosNorthMos.defaultTelescopeConfigs.get(observingMode.get)
        ),
      presets = NonEmptyList.one(
        "Default" -> ObservingMode.GmosNorthMos.defaultTelescopeConfigs.get(observingMode.get)
      ),
      defaultConfigs = ObservingMode.GmosNorthMos.defaultTelescopeConfigs.get(observingMode.get),
      helpId = "configuration/mos-spatial-offsets.md".refined,
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  private def acquisitionTypeView: View[GmosMosAcquisitionType] =
    observingMode
      .zoom(ObservingMode.GmosNorthMos.acquisitionType, GmosNorthMosInput.acquisitionType.modify)
      .view(_.assign)

  private def acquisition
    : Aligner[ObservingMode.GmosNorthMos.Acquisition, GmosNorthMosAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GmosNorthMos.acquisition,
      forceAssign(GmosNorthMosInput.acquisition.modify)(GmosNorthMosAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val defaultAcquisitionFilter                                 =
      ObservingMode.GmosNorthMos.acquisition
        .andThen(ObservingMode.GmosNorthMos.Acquisition.defaultFilter)
        .get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosNorthFilter]         =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosNorthFilter]] =
      acquisition
        .zoom(
          ObservingMode.GmosNorthMos.Acquisition.explicitFilter,
          GmosNorthMosAcquisitionInput.explicitFilter.modify
        )
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]      =
      acquisition
        .zoom(
          ObservingMode.GmosNorthMos.Acquisition.exposureTimeMode,
          GmosNorthMosAcquisitionInput.exposureTimeMode.modify
        )
        .view(_.toInput.assign)
    mosAcquisitionPanel(
      this,
      disabled,
      acquisitionTypeView,
      explicitAcquisitionFilter.withDefault(defaultAcquisitionFilter),
      defaultAcquisitionFilter,
      excludedAcquisitionFilters,
      acquisitionExposureTimeMode
    )

object GmosNorthMosPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosNorthGrating,
      GmosNorthFilter,
      GmosCustomSlitWidth,
      GmosNorthMosPanel
    ]
