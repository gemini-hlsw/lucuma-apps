// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SpectroscopyConfigurationPanel(
  instrument:           Option[Instrument],
  exposureTimeMode:     View[Option[ExposureTimeMode]],
  exposureTimeModeType: View[ExposureTimeModeType],
  options:              View[ScienceRequirements.Spectroscopy],
  readonly:             Boolean,
  units:                WavelengthUnits,
  calibrationRole:      Option[CalibrationRole]
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel extends ConfigurationFormats:
  private type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent[Props]: p =>
      val resolution             = p.options.zoom(ScienceRequirements.Spectroscopy.resolution)
      val wv                     = p.options.zoom(ScienceRequirements.Spectroscopy.wavelength)
      val wavelengthDelta        = p.options.zoom(ScienceRequirements.Spectroscopy.wavelengthCoverage)
      val focalPlane             = p.options.zoom(ScienceRequirements.Spectroscopy.focalPlane)
      val spectroscopyCapability =
        p.options.zoom(ScienceRequirements.Spectroscopy.capability)

      val capabililitesSupported = false
      ReactFragment(
        FormInputTextView[View, Option[Wavelength]](
          id = "configuration-wavelength".refined,
          value = wv,
          label = <.div(
            "Wavelength",
            HelpIcon("configuration/wavelength.md".refined)
          ),
          groupClass = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
          postAddons = wv.get.fold(List(p.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
          units = p.units.symbol,
          validFormat = p.units.toInputWedge,
          changeAuditor = p.units.toAuditor,
          disabled = p.readonly
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "configuration-resolution-power".refined,
          value = resolution,
          label = ReactFragment(
            "Min Resolution",
            HelpIcon("configuration/spectral_resolution.md".refined)
          ),
          validFormat = InputValidSplitEpi.posInt.optional,
          changeAuditor = ChangeAuditor.posInt.optional,
          disabled = p.readonly
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "wavelength-range".refined,
          value = wavelengthDelta,
          units = p.units.symbol,
          label = ReactFragment(
            "Min λ Coverage",
            HelpIcon("configuration/wavelength_coverage.md".refined)
          ),
          validFormat = p.units.toDeltaInputWedge,
          changeAuditor = p.units.toAuditor,
          disabled = p.readonly
        ).clearable(^.autoComplete.off),
        ExposureTimeModeEditorOptional(
          instrument = p.instrument,
          wavelength = wv.get,
          exposureTimeMode = p.exposureTimeMode,
          exposureTimeModeType = p.exposureTimeModeType,
          coadds = none,
          scienceMode = ScienceMode.Spectroscopy,
          readonly = p.readonly,
          units = p.units,
          calibrationRole = p.calibrationRole,
          idPrefix = "spectReq".refined
        ),
        FormEnumDropdownOptionalView(
          id = "focal-plane".refined,
          label = ReactFragment("Focal Plane", HelpIcon("configuration/focal_plane.md".refined)),
          placeholder = "All",
          value = focalPlane,
          disabled = p.readonly
        ),
        Option.when(capabililitesSupported)( // Hide until supported
          FormEnumDropdownOptionalView(
            id = "spectroscopy-capability".refined,
            placeholder = "None",
            value = spectroscopyCapability,
            label = ReactFragment(
              "Capability",
              HelpIcon("configuration/capability.md".refined)
            ),
            disabled = true
          )
        )
      )
