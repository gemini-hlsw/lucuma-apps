// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.Attachment
import explore.model.Help
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

/**
 * The MOS Acquisition panel contents, shared by GN and GS.
 */
private[config] def mosAcquisitionPanel[Filter: Enumerated: Display](
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
