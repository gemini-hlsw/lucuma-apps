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

case class GmosSouthMosPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GmosSouthMos, GmosSouthMosInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits,
  maskContext:     MosMaskContext
)(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO])
    extends ReactFnProps[GmosSouthMosPanel](GmosSouthMosPanel.component)
    with GmosMosPanelProps[GmosSouthGrating, GmosSouthFilter]:

  val maskInstrument: Instrument = Instrument.GmosSouth

  def mode: ObservingMode   = observingMode.get
  def isCustomized: Boolean = observingMode.get.isCustomized

  def initialGrating: GmosSouthGrating                         =
    ObservingMode.GmosSouthMos.initialGrating.get(observingMode.get)
  def initialFilter: Option[GmosSouthFilter]                   =
    ObservingMode.GmosSouthMos.initialFilter.get(observingMode.get)
  def initialFpu: GmosCustomSlitWidth                          =
    ObservingMode.GmosSouthMos.initialSlitWidth.get(observingMode.get)
  def initialCentralWavelength: Wavelength                     =
    ObservingMode.GmosSouthMos.initialCentralWavelength
      .andThen(CentralWavelength.Value)
      .get(observingMode.get)
  def defaultXBinning: GmosXBinning                            =
    ObservingMode.GmosSouthMos.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            =
    ObservingMode.GmosSouthMos.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosSouthMos.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosSouthMos.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      =
    ObservingMode.GmosSouthMos.defaultRoi.get(observingMode.get)
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither] =
    ObservingMode.GmosSouthMos.defaultWavelengthDithers.get(observingMode.get)

  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain) =
    val readMode = ObservingMode.GmosSouthMos.explicitAmpReadMode
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosSouthMos.defaultAmpReadMode.get(observingMode.get))
    val ampGain  = ObservingMode.GmosSouthMos.explicitAmpGain
      .get(observingMode.get)
      .getOrElse(ObservingMode.GmosSouthMos.defaultAmpGain.get(observingMode.get))
    (readMode, ampGain)

  def revertCustomizations: Callback = observingMode.view(_.toInput).mod(_.revertCustomizations)

  def centralWavelengthView: View[Wavelength] =
    observingMode
      .zoom(
        ObservingMode.GmosSouthMos.centralWavelength.andThen(CentralWavelength.Value),
        GmosSouthMosInput.centralWavelength.modify
      )
      .view(_.toInput.assign)

  def gratingView: View[GmosSouthGrating] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.grating, GmosSouthMosInput.grating.modify)
      .view(_.assign)

  def filterView: View[Option[GmosSouthFilter]] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.filter, GmosSouthMosInput.filter.modify)
      .view(_.orUnassign)

  // See the note on the North equivalent.
  private def customMask: Aligner[ObservingMode.GmosCustomMask, GmosCustomMaskInput] =
    observingMode.zoom(
      ObservingMode.GmosSouthMos.customMask,
      forceAssign(GmosSouthMosInput.customMask.modify)(observingMode.get.customMask.toInput)
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
      .zoom(ObservingMode.GmosSouthMos.explicitXBin, GmosSouthMosInput.explicitXBin.modify)
      .view(_.map(_.value).orUnassign)

  def explicitYBinningView: View[Option[GmosYBinning]] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.explicitYBin, GmosSouthMosInput.explicitYBin.modify)
      .view(_.map(_.value).orUnassign)

  // The explicit return type drives inference of the `f => i => f(i)` mod function.
  private def readGainAligner: Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthMosInput] =
    observingMode.zoom(
      unsafeDisjointOptionZip(
        ObservingMode.GmosSouthMos.explicitAmpReadMode,
        ObservingMode.GmosSouthMos.explicitAmpGain
      ),
      f => i => f(i)
    )

  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
    readGainAligner.viewMod { org =>
      val rg = org.unzip
      GmosSouthMosInput.explicitAmpReadMode
        .replace(rg._1.orUnassign)
        .andThen(GmosSouthMosInput.explicitAmpGain.replace(rg._2.orUnassign))
    }

  def explicitRoiView: View[Option[GmosRoi]] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.explicitRoi, GmosSouthMosInput.explicitRoi.modify)
      .view(_.orUnassign)

  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]] =
    observingMode
      .zoom(
        ObservingMode.GmosSouthMos.explicitWavelengthDithers,
        GmosSouthMosInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

  def exposureTimeModeView: View[ExposureTimeMode] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.exposureTimeMode, GmosSouthMosInput.exposureTimeMode.modify)
      .view(_.toInput.assign)

  def offsetsControl(disabled: Boolean): VdomNode =
    // A MOS mask has no single slit to nod along, so the positions are a plain list
    // and the only preset is the default.
    PresettableTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(
          ObservingMode.GmosSouthMos.explicitTelescopeConfigs,
          GmosSouthMosInput.explicitTelescopeConfigs.modify
        )
        .view(_.map(_.toList.map(_.toInput)).orUnassign)
        .removeOptionality(
          ObservingMode.GmosSouthMos.defaultTelescopeConfigs.get(observingMode.get)
        ),
      presets = NonEmptyList.one(
        "Default" -> ObservingMode.GmosSouthMos.defaultTelescopeConfigs.get(observingMode.get)
      ),
      defaultConfigs = ObservingMode.GmosSouthMos.defaultTelescopeConfigs.get(observingMode.get),
      helpId = "configuration/mos-spatial-offsets.md".refined,
      presetsReadonly = disabled,
      editingReadonly = disabled
    )

  private def acquisitionTypeView: View[GmosMosAcquisitionType] =
    observingMode
      .zoom(ObservingMode.GmosSouthMos.acquisitionType, GmosSouthMosInput.acquisitionType.modify)
      .view(_.assign)

  private def acquisition
    : Aligner[ObservingMode.GmosSouthMos.Acquisition, GmosSouthMosAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GmosSouthMos.acquisition,
      forceAssign(GmosSouthMosInput.acquisition.modify)(GmosSouthMosAcquisitionInput())
    )

  def acquisitionSection(disabled: Boolean): VdomNode =
    val defaultAcquisitionFilter                                 =
      ObservingMode.GmosSouthMos.acquisition
        .andThen(ObservingMode.GmosSouthMos.Acquisition.defaultFilter)
        .get(observingMode.get)
    val excludedAcquisitionFilters: Set[GmosSouthFilter]         =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet
    val explicitAcquisitionFilter: View[Option[GmosSouthFilter]] =
      acquisition
        .zoom(
          ObservingMode.GmosSouthMos.Acquisition.explicitFilter,
          GmosSouthMosAcquisitionInput.explicitFilter.modify
        )
        .view(_.orUnassign)
    val acquisitionExposureTimeMode: View[ExposureTimeMode]      =
      acquisition
        .zoom(
          ObservingMode.GmosSouthMos.Acquisition.exposureTimeMode,
          GmosSouthMosAcquisitionInput.exposureTimeMode.modify
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

object GmosSouthMosPanel
    extends GmosSpectroscopyPanelBuilder[
      GmosSouthGrating,
      GmosSouthFilter,
      GmosCustomSlitWidth,
      GmosSouthMosPanel
    ]
