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
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.flamingos2
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
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
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit

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

        // OTHER is not an accepted slit width for this mode.
        val excludedSlitWidths = Set(Flamingos2CustomSlitWidth.Other)

        val maskIsBound = attachmentIdView.get.isDefined

        // Once a mask is bound the plate defines the slit width, so the control goes away.
        // Before the proposal is reviewed there is no mask to pick, so only the width is offered.
        val showSlitWidth = !props.maskContext.pickerActive || !maskIsBound

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
          if (showSlitWidth)
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
          else EmptyVdom

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
