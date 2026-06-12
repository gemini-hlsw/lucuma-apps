// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.implicits.*
import crystal.react.View
import explore.components.HelpIcon
import explore.config.ConfigurationFormats.*
import explore.model.BroadBand
import explore.model.Combination
import explore.model.NarrowBand
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ImagingCapability
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class ImagingConfigurationPanel(
  instrument:           Option[Instrument],
  exposureTimeMode:     View[Option[ExposureTimeMode]],
  exposureTimeModeType: View[ExposureTimeModeType],
  options:              View[ScienceRequirements.Imaging],
  capability:           View[Option[ImagingCapability]],
  readonly:             Boolean,
  units:                WavelengthUnits,
  calibrationRole:      Option[CalibrationRole]
) extends ReactFnProps(ImagingConfigurationPanel)

object ImagingConfigurationPanel
    extends ReactFnComponent[ImagingConfigurationPanel](p =>

      val fov         = p.options.zoom(ScienceRequirements.Imaging.minimumFov)
      val narrowBand  =
        p.options.zoom(ScienceRequirements.Imaging.narrowFilters.some.andThen(NarrowBand.Value))
      val broadBand   =
        p.options.zoom(ScienceRequirements.Imaging.broadFilters.some.andThen(BroadBand.Value))
      val combination =
        p.options
          .zoom(ScienceRequirements.Imaging.combinedFilters.some.andThen(Combination.Value))

      React.Fragment(
        FormInputTextView(
          id = "configuration-fov".refined,
          value = fov,
          label = React.Fragment("Minimum FoV", HelpIcon("configuration/fov.md".refined)),
          units = "arcsec",
          validFormat = angleArcsecsFormat,
          changeAuditor = angleArcsecondsChangeAuditor,
          disabled = p.readonly
        ).clearable,
        <.label(
          "Filters",
          HelpIcon("configuration/filter.md".refined),
          LucumaPrimeStyles.FormFieldLabel
        ),
        CheckboxView(
          id = "narrowband-filter".refined,
          value = narrowBand,
          label = "Narrow",
          disabled = p.readonly
        ),
        CheckboxView(
          id = "broadband-filter".refined,
          value = broadBand,
          label = "Broad",
          disabled = p.readonly
        ),
        CheckboxView(
          id = "combination-filter".refined,
          value = combination,
          label = "Combination",
          disabled = p.readonly
        ),
        FormEnumDropdownOptionalView(
          id = "imaging-capability".refined,
          label = React.Fragment("Capability", HelpIcon("configuration/capability.md".refined)),
          value = p.capability,
          exclude = Set(ImagingCapability.WideField),
          placeholder = "Any",
          disabled = p.readonly
        ),
        ExposureTimeModeEditorOptional(
          instrument = p.instrument,
          wavelength = none,
          exposureTimeMode = p.exposureTimeMode,
          exposureTimeModeType = p.exposureTimeModeType,
          coadds = none,
          scienceMode = ScienceMode.Imaging,
          readonly = p.readonly,
          units = p.units,
          calibrationRole = p.calibrationRole,
          idPrefix = "imagingReq".refined
        )
      )
    )
