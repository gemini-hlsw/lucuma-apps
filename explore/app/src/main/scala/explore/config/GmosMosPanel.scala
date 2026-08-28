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
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.PresettableTelescopeConfigsEditor
import explore.model.Attachment
import explore.model.Help
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
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
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

/**
 * The MOS Acquisition panel contents, shared by GN and GS.
 */
private def mosAcquisitionPanel[Filter: Enumerated: Display](
  props:            GmosSpectroscopyPanelProps[?, ?, ?],
  disabled:         Boolean,
  acquisitionType:  View[GmosMosAcquisitionType],
  filterView:       View[Option[Filter]],
  defaultFilter:    Filter,
  excludedFilters:  Set[Filter],
  exposureTimeMode: View[ExposureTimeMode]
): VdomNode =
  <.div(
    ExploreStyles.AcquisitionCustomizationGrid,
    <.div(
      LucumaPrimeStyles.FormColumnCompact,
      FormEnumDropdownView(
        id = "acq-type".refined,
        value = acquisitionType,
        label = "Acquisition Type",
        disabled = props.permissions.isReadonly
      ),
      CustomizableEnumSelectOptional(
        id = "acq-explicit-filter".refined,
        view = filterView,
        defaultValue = defaultFilter.some,
        exclude = excludedFilters,
        label = "Filter".some,
        helpId = None,
        disabled = disabled,
        showCustomization = props.showCustomization,
        allowRevertCustomization =
          props.allowRevertCustomization || props.permissions.isOnlyForOngoing
      )
    ),
    <.div(
      LucumaPrimeStyles.FormColumnCompact,
      // MOS acquisition is always a single exposure and the ODB rejects a
      // signal-to-noise mode, so only the exposure time is offered.
      ExposureTimeModeEditor(
        instrument = props.instrument,
        wavelength = none,
        exposureTimeMode = exposureTimeMode,
        coadds = none,
        scienceMode = ScienceMode.Imaging,
        readonly = props.permissions.isReadonly,
        units = props.units,
        calibrationRole = props.calibrationRole,
        idPrefix = "gmosMosAcq".refined,
        forceCount = Some(1.refined),
        forceModeType = Some(ExposureTimeModeType.TimeAndCount)
      )
    )
  )

/**
 * Props shared by the two GMOS MOS panels. MOS selects its slit width from a custom mask, offers a
 * mask picker, and shows the FPU as read-only once a mask is bound.
 */
trait GmosMosPanelProps[Grating, Filter]
    extends GmosSpectroscopyPanelProps[Grating, Filter, GmosCustomSlitWidth]:
  def maskContext: MosMaskContext
  def maskInstrument: Instrument

  def customMaskAttachmentIdView: View[Option[Attachment.Id]]

  override def excludedFpus: Set[GmosCustomSlitWidth] = Set.empty
  override def fpuLabel: String                       = "Custom Slit Width"
  override def fpuHelpId: Option[Help.Id]             =
    Some("configuration/gmos/mos-slit-width.md".refined)

  private def maskIsBound: Boolean = customMaskAttachmentIdView.get.isDefined

  override def maskControl: VdomNode =
    // Only shown once the proposal is accepted
    if (maskContext.pickerActive)
      MosMaskPicker(
        instrument = maskInstrument,
        attachmentIdView = customMaskAttachmentIdView,
        attachments = maskContext.attachments,
        obsAttachmentIds = maskContext.obsAttachmentIds,
        helpId = "configuration/gmos/mos-mask.md".refined,
        disabled = !permissions.isFullEdit
      )
    else EmptyVdom

  override def fpuControlReadonly: Boolean = maskContext.pickerActive && maskIsBound

// Gmos North MOS
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
  def defaultXBinning: GmosXBinning                            = ObservingMode.GmosNorthMos.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            = ObservingMode.GmosNorthMos.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosNorthMos.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosNorthMos.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      = ObservingMode.GmosNorthMos.defaultRoi.get(observingMode.get)
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

// Gmos South MOS
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
  def defaultXBinning: GmosXBinning                            = ObservingMode.GmosSouthMos.defaultXBin.get(observingMode.get)
  def defaultYBinning: GmosYBinning                            = ObservingMode.GmosSouthMos.defaultYBin.get(observingMode.get)
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)      =
    (ObservingMode.GmosSouthMos.defaultAmpReadMode.get(observingMode.get),
     ObservingMode.GmosSouthMos.defaultAmpGain.get(observingMode.get)
    )
  def defaultRoi: GmosRoi                                      = ObservingMode.GmosSouthMos.defaultRoi.get(observingMode.get)
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
