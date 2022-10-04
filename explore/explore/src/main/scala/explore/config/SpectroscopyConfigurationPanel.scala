// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import explore.common.ObsQueries.SpectroscopyRequirementsData
import explore.components.HelpIcon
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.ExploreModelValidators
import explore.model.display.given
import explore.model.formats.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps

case class SpectroscopyConfigurationPanel(
  options: View[SpectroscopyRequirementsData]
) extends ReactFnProps[SpectroscopyConfigurationPanel](SpectroscopyConfigurationPanel.component)

object SpectroscopyConfigurationPanel {
  type Props = SpectroscopyConfigurationPanel

  protected val component =
    ScalaFnComponent[Props] { p =>
      val wv                       = p.options.zoom(SpectroscopyRequirementsData.wavelength)
      val resolution               = p.options.zoom(SpectroscopyRequirementsData.resolution)
      val signalToNoise            = p.options.zoom(SpectroscopyRequirementsData.signalToNoise)
      val signalToNoiseAt          = p.options.zoom(SpectroscopyRequirementsData.signalToNoiseAt)
      val wavelengthCoverage       =
        p.options.zoom(SpectroscopyRequirementsData.wavelengthCoverage)
      val focalPlane               = p.options.zoom(SpectroscopyRequirementsData.focalPlane)
      val focalPlaneAngle          = p.options.zoom(SpectroscopyRequirementsData.focalPlaneAngle)
      val spectroscopyCapabilities =
        p.options.zoom(SpectroscopyRequirementsData.capabilities)

      val wvMicroInput    = ExploreModelValidators.wavelengthValidWedge.optional
      val wvChangeAuditor = ChangeAuditor
        .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
        .allow(s => s === "0" || s === "0.")
        .decimal(3.refined)
        .optional

      val wvUnits =
        <.span("μm ", requiredForITC.unless(wv.get.isDefined))

      ReactFragment(
        <.label("Wavelength",
                HelpIcon("configuration/wavelength.md".refined),
                ExploreStyles.SkipToNext
        ),
        InputWithUnits(
          id = "configuration-wavelength".refined,
          clazz = ExploreStyles.WarningInput.when_(wv.get.isEmpty),
          inline = true,
          value = wv,
          units = wvUnits,
          validFormat = wvMicroInput,
          changeAuditor = wvChangeAuditor,
          disabled = false
        ).clearableNoPadding,
        <.label("λ / Δλ",
                HelpIcon("configuration/spectral_resolution.md".refined),
                ExploreStyles.SkipToNext
        ),
        FormInputEV(
          id = "configuration-resolution-power".refined,
          value = resolution,
          validFormat = InputValidSplitEpi.posInt.optional,
          changeAuditor = ChangeAuditor.posInt.optional
        ).clearableNoPadding,
        <.label("S / N",
                HelpIcon("configuration/signal_to_noise.md".refined),
                ExploreStyles.SkipToNext
        ),
        FormInputEV(
          id = "signal-to-noise".refined,
          value = signalToNoise,
          clazz = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
          validFormat = InputValidSplitEpi.posBigDecimal.optional,
          changeAuditor = ChangeAuditor.posBigDecimal().optional
        ).clearableNoPadding,
        <.div(
          ExploreStyles.InputWithLabel,
          requiredForITC.unless(signalToNoise.get.isDefined),
          <.label("at"),
          InputWithUnits(
            id = "signal-to-noise-at".refined,
            clazz = Css.Empty,
            value = signalToNoiseAt,
            units = "μm",
            validFormat = wvMicroInput,
            changeAuditor = wvChangeAuditor,
            disabled = false
          ).clearableNoPadding
        ),
        <.label("λ Coverage",
                HelpIcon("configuration/wavelength_coverage.md".refined),
                ExploreStyles.SkipToNext
        ),
        InputWithUnits(
          id = "wavelength-coverage".refined,
          clazz = Css.Empty,
          inline = true,
          value = wavelengthCoverage,
          units = "μm",
          validFormat = wvMicroInput,
          changeAuditor = wvChangeAuditor,
          disabled = false
        ).clearableNoPadding,
        <.label("Focal Plane",
                HelpIcon("configuration/focal_plane.md".refined),
                ExploreStyles.SkipToNext
        ),
        EnumViewOptionalSelect(
          id = "focal-plane",
          placeholder = "Any",
          upward = true,
          value = focalPlane,
          clearable = true
        ),
        <.div(
          ExploreStyles.InputWithLabel,
          InputWithUnits(
            id = "focal-plane-angle".refined,
            clazz = Css.Empty,
            value = focalPlaneAngle,
            units = "arcsec",
            validFormat = InputValidWedge.fromFormat(formatArcsec).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional,
            disabled = false
          ).clearableNoPadding
        ),
        <.label(
          "Capabilities",
          HelpIcon("configuration/capabilities.md".refined),
          ExploreStyles.SkipToNext
        ),
        EnumViewOptionalSelect(
          id = "spectroscopy-capabilities",
          clazz = ExploreStyles.ConfigurationCapabilities,
          clearable = true,
          upward = true,
          placeholder = "None",
          value = spectroscopyCapabilities
        )
      )
    }
}
