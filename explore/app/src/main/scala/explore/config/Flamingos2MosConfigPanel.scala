// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.flamingos2
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Flamingos2MosConfigPanel(
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Flamingos2Mos, Flamingos2MosInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits,
  isStaff:         Boolean,
  maskContext:     MosMaskContext
) extends ReactFnProps(Flamingos2MosConfigPanel)

object Flamingos2MosConfigPanel
    extends ReactFnComponent[Flamingos2MosConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        modeData  <- useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val disableAdvancedEdit      =
          editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
        val disableSimpleEdit        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val disableAdvancedAcqEdit   = disableAdvancedEdit && !props.permissions.isOnlyForOngoing
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit
        val showAcquisitionConfig    = props.calibrationRole.needsAcquisitionConfig

        val disperserView: View[Flamingos2Disperser] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.disperser,
            Flamingos2MosInput.disperser.modify
          )
          .view(_.assign)

        val filterView: View[Flamingos2Filter] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.filter,
            Flamingos2MosInput.filter.modify
          )
          .view(_.assign)

        // The whole mask must be resent when either half of it changes, so the delta is
        // seeded from the current value rather than left partially assigned.
        val customMask: Aligner[ObservingMode.Flamingos2CustomMask, Flamingos2CustomMaskInput] =
          props.observingMode.zoom(
            ObservingMode.Flamingos2Mos.customMask,
            forceAssign(Flamingos2MosInput.customMask.modify)(
              props.observingMode.get.customMask.toInput
            )
          )

        val slitWidthView: View[Flamingos2CustomSlitWidth] = customMask
          .zoom(
            ObservingMode.Flamingos2CustomMask.slitWidth,
            Flamingos2CustomMaskInput.slitWidth.modify
          )
          .view(identity)

        val attachmentIdView: View[Option[Attachment.Id]] = customMask
          .zoom(
            ObservingMode.Flamingos2CustomMask.attachmentId,
            Flamingos2CustomMaskInput.attachmentId.modify
          )
          .view(_.orUnassign)

        val readModeView: View[Option[Flamingos2ReadMode]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.explicitReadMode,
            Flamingos2MosInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.explicitTelescopeConfigs,
            Flamingos2MosInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.exposureTimeMode,
            Flamingos2MosInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        val deckerView: View[Option[Flamingos2Decker]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Mos.explicitDecker,
            Flamingos2MosInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val acquisition
          : Aligner[ObservingMode.Flamingos2Mos.Acquisition, Flamingos2MosAcquisitionInput] =
          props.observingMode.zoom(
            ObservingMode.Flamingos2Mos.acquisition,
            forceAssign(Flamingos2MosInput.acquisition.modify)(
              Flamingos2MosAcquisitionInput()
            )
          )

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(ObservingMode.Flamingos2Mos.Acquisition.exposureTimeMode,
                  Flamingos2MosAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        val explicitAcquisitionFilterView: View[Option[Flamingos2Filter]] =
          acquisition
            .zoom(ObservingMode.Flamingos2Mos.Acquisition.explicitFilter,
                  Flamingos2MosAcquisitionInput.explicitFilter.modify
            )
            .view(_.orUnassign)

        val defaultAcquisitionFilter =
          props.observingMode.get.acquisition.defaultFilter

        val excludedAcquisitionFilters =
          Enumerated[Flamingos2Filter].all.toSet -- Flamingos2Filter.acquisition.toList.toSet

        // OTHER is not an accepted slit width for this mode.
        val excludedSlitWidths = Set(Flamingos2CustomSlitWidth.Other)

        val maskIsBound = attachmentIdView.get.isDefined

        // Once a mask is bound the plate defines the slit width, so the value is shown but
        // not editable. Before the proposal is reviewed there is no mask to pick, so the
        // width is stated by hand.
        val slitWidthReadonly = props.maskContext.pickerActive && maskIsBound

        val maskPicker: VdomNode =
          if (props.maskContext.pickerActive)
            MosMaskPicker(
              attachmentIdView = attachmentIdView,
              attachments = props.maskContext.attachments,
              obsAttachmentIds = props.maskContext.obsAttachmentIds,
              helpId = "configuration/f2/mos-mask.md".refined,
              disabled = !props.permissions.isFullEdit
            )
          else EmptyVdom

        val slitWidthControl: VdomNode =
          if (slitWidthReadonly)
            React.Fragment(
              FormLabel(htmlFor = "slit-width".refined)(
                "Custom Slit Width",
                HelpIcon("configuration/f2/mos-slit-width.md".refined)
              ),
              <.label(^.id := "slit-width",
                      ExploreStyles.FormValue |+| ExploreStyles.FormValueRight,
                      slitWidthView.get.shortName
              )
            )
          else
            CustomizableEnumSelect(
              id = "slit-width".refined,
              view = slitWidthView,
              defaultValue = props.observingMode.get.initialSlitWidth,
              label = "Custom Slit Width".some,
              exclude = excludedSlitWidths,
              helpId = Some("configuration/f2/mos-slit-width.md".refined),
              disabled = disableSimpleEdit,
              showCustomization = showCustomization,
              allowRevertCustomization = allowRevertCustomization
            )

        val fpuControl: VdomNode = React.Fragment(maskPicker, slitWidthControl)

        React.Fragment(
          Flamingos2ConfigFields(
            fpuControl = fpuControl,
            deckerView = deckerView,
            defaultDecker = props.observingMode.get.defaultDecker,
            filterView = filterView,
            initialFilter = props.observingMode.get.initialFilter,
            disperserView = disperserView,
            initialDisperser = props.observingMode.get.initialDisperser,
            readModeView = readModeView,
            exposureTimeMode = exposureTimeMode,
            explicitTelescopeConfigsView = explicitTelescopeConfigsView,
            defaultTelescopeConfigs = props.observingMode.get.defaultTelescopeConfigs,
            defaultForPreset = flamingos2.defaultMosTelescopeConfigs,
            offsetsHelpId = "configuration/f2/mos-spatial-offsets.md".refined,
            instrument = props.observingMode.get.instrument,
            modeData = modeData,
            units = props.units,
            calibrationRole = props.calibrationRole,
            etmIdPrefix = "f2Mos".refined,
            isStaff = props.isStaff,
            disableSimpleEdit = disableSimpleEdit,
            disableAdvancedEdit = disableAdvancedEdit,
            showCustomization = showCustomization,
            allowRevertCustomization = allowRevertCustomization,
            etmReadonly = !props.permissions.isFullEdit,
            presetsReadonly = !props.permissions.isFullEdit
          ),
          <.div(
            ExploreStyles.Flamingos2LowerGrid,
            Panel(
              header = <.span("Acquisition",
                              HelpIcon("configuration/f2/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              <.div(
                ExploreStyles.AcquisitionCustomizationGrid,
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  CustomizableEnumSelectOptional(
                    id = "f2-mos-acq-filter".refined,
                    view = explicitAcquisitionFilterView.withDefault(defaultAcquisitionFilter),
                    defaultValue = defaultAcquisitionFilter.some,
                    label = "Filter".some,
                    helpId = Some("configuration/f2/acquisition-filter.md".refined),
                    exclude = excludedAcquisitionFilters,
                    disabled = disableAdvancedAcqEdit,
                    showCustomization = showCustomization,
                    allowRevertCustomization =
                      allowRevertCustomization || props.permissions.isOnlyForOngoing
                  )
                ),
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  // F2 MOS acquisition is always a single exposure and the ODB rejects a
                  // signal-to-noise mode, so only the exposure time is offered.
                  ExposureTimeModeEditor(
                    instrument = props.observingMode.get.instrument,
                    wavelength = none,
                    exposureTimeMode = acquisitionExposureTimeView,
                    coadds = none,
                    scienceMode = ScienceMode.Imaging,
                    readonly = props.permissions.isReadonly,
                    units = props.units,
                    calibrationRole = props.calibrationRole,
                    idPrefix = "f2MosAcq".refined,
                    forceCount = Some(1.refined),
                    forceModeType = Some(ExposureTimeModeType.TimeAndCount)
                  )
                )
              )
            ).when(showAcquisitionConfig),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              !props.permissions.isFullEdit,
              showAdvancedButton = true
            )
          )
        )
    )
