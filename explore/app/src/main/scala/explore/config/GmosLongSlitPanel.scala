// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.Help
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

/**
 * The long slit Acquisition panel contents, shared by GN and GS.
 */
private[config] def longSlitAcqPanel[Filter: Enumerated: Display](
  props:            GmosSpectroscopyPanelProps[?, ?, ?],
  disabled:         Boolean,
  roiView:          View[Option[GmosLongSlitAcquisitionRoi]],
  defaultRoi:       GmosLongSlitAcquisitionRoi,
  filterView:       View[Option[Filter]],
  defaultFilter:    Filter,
  excludedFilters:  Set[Filter],
  exposureTimeMode: View[ExposureTimeMode]
): VdomNode =
  <.div(
    ExploreStyles.AcquisitionCustomizationGrid,
    <.div(
      LucumaPrimeStyles.FormColumnCompact,
      CustomizableEnumSelectOptional(
        id = "acq-explicit-roi".refined,
        view = roiView,
        defaultValue = defaultRoi.some,
        label = "ROI".some,
        helpId = None,
        disabled = disabled,
        showCustomization = props.showCustomization,
        allowRevertCustomization =
          props.allowRevertCustomization || props.permissions.isOnlyForOngoing
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
      ExposureTimeModeEditor(
        instrument = props.instrument,
        wavelength = none,
        exposureTimeMode = exposureTimeMode,
        coadds = none,
        scienceMode = ScienceMode.Imaging,
        readonly = props.permissions.isReadonly,
        units = props.units,
        calibrationRole = props.calibrationRole,
        idPrefix = "gmosAcq".refined,
        forceCount = Some(1.refined)
      )
    )
  )

/** Props shared by the two GMOS long slit panels. Long slit has no MOS mask. */
trait GmosLongSlitPanelProps[Grating, Filter, Fpu]
    extends GmosSpectroscopyPanelProps[Grating, Filter, Fpu]:
  override def maskControl: VdomNode = EmptyVdom

  override def fpuLabel: String           = "FPU"
  override def fpuHelpId: Option[Help.Id] = Some("configuration/gmos/fpu.md".refined)
