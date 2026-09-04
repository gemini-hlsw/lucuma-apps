// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order.given
import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.Constants
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode.GnirsCentralWavelengthConfig
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

/**
 * The GNIRS spectroscopy central wavelengths. Each row is a separate configuration: its own
 * exposure time mode, coadds, ITC calculation and calibrations. Modeled on `ImagingFiltersPanel`,
 * which does the same for the imaging modes' filters.
 */
case class GnirsWavelengthsPanel(
  instrument:                   Option[Instrument],
  wavelengthsView:              View[NonEmptyList[GnirsCentralWavelengthConfig]],
  initialWavelengths:           NonEmptyList[GnirsCentralWavelengthConfig],
  requirementsExposureTimeMode: Option[ExposureTimeMode],
  units:                        WavelengthUnits,
  calibrationRole:              Option[CalibrationRole],
  allowRevertCustomization:     Boolean,
  // The central wavelengths themselves are part of the configuration, so editing
  // them (and adding or removing rows) requires customizing it...
  wavelengthReadonly:           Boolean,
  // ...but the exposure time modes are ordinary observation parameters and stay
  // editable without customizing, as they were when they lived in the panel above.
  exposureTimeModeReadonly:     Boolean,
  showCustomization:            Boolean
) extends ReactFnProps(GnirsWavelengthsPanel.component)

object GnirsWavelengthsPanel:

  private given Reusability[GnirsCentralWavelengthConfig] = Reusability.byEq

  // A new row starts one "step" above the longest current wavelength, so it lands
  // somewhere sensible and never collides with an existing entry.
  private val NewWavelengthStepPm: Int = 100_000

  private def nextWavelength(
    current: NonEmptyList[GnirsCentralWavelengthConfig]
  ): Option[Wavelength] =
    Wavelength.fromIntPicometers(
      current.toList
        .map(_.centralWavelength.value.toPicometers.value.value)
        .max + NewWavelengthStepPm
    )

  private val component =
    ScalaFnComponent[GnirsWavelengthsPanel]: props =>
      for
        // As in ImagingFiltersPanel, edit through a plain list so a row can be
        // removed even though the model requires at least one.
        unModdedView <- useStateView(List.empty[GnirsCentralWavelengthConfig])
        _            <- useEffectWithDeps(props.wavelengthsView.get.toList)(unModdedView.set)
      yield
        val localView: View[List[GnirsCentralWavelengthConfig]] =
          unModdedView
            .withModPatch(_.sortBy(_.centralWavelength.value))
            .withOnMod: l =>
              NonEmptyList.fromList(l).fold(Callback.empty)(props.wavelengthsView.set)

        Panel(
          header = <.span(
            "Central Wavelengths",
            HelpIcon("configuration/gnirs/wavelength.md".refined),
            CustomizedGroupAddon(
              "original",
              props.wavelengthsView.set(props.initialWavelengths),
              props.allowRevertCustomization
            ).when(
              props.showCustomization && props.initialWavelengths =!= props.wavelengthsView.get
            ),
            <.span(Icons.ErrorIcon)
              .withTooltip(content = "At least one central wavelength is required.")
              .when(localView.get.isEmpty)
          ),
          toggleable = true,
          collapsed = false
        )(
          <.div(ExploreStyles.GnirsWavelengthGrid)(
            <.span(), // the action button
            <.span("λ Center", ExploreStyles.GnirsWavelengthGridHeader),
            <.span("Exposure Mode", ExploreStyles.GnirsWavelengthGridHeader),
            <.span("Signal/Noise", ExploreStyles.GnirsWavelengthGridHeader),
            <.span("Exp. Time", ExploreStyles.GnirsWavelengthGridHeader),
            <.span("Number of Exp.", ExploreStyles.GnirsWavelengthGridHeader),
            <.span("Coadds", ExploreStyles.GnirsWavelengthGridHeader),
            <.span(Constants.SignalToNoiseAtLabel, ExploreStyles.GnirsWavelengthGridHeader),
            localView.toListOfViews.zipWithIndex
              .toReactFragment(using
                (swView, idx) =>
                  val wavelength = swView.get.centralWavelength
                  React.Fragment(
                    Button(
                      icon = Icons.Trash,
                      clazz = ExploreStyles.GnirsWavelengthGridAction,
                      text = true,
                      disabled = props.wavelengthReadonly,
                      onClick = localView.mod(_.filterNot(_.centralWavelength === wavelength))
                    ).tiny.compact,
                    <.span(
                      ExploreStyles.GnirsWavelengthCenter,
                      FormInputTextView(
                        id = NonEmptyString.unsafeFrom(s"gnirsCentralWavelength$idx"),
                        value = swView.zoom(
                          GnirsCentralWavelengthConfig.centralWavelength.andThen(
                            CentralWavelength.Value
                          )
                        ),
                        label = "λ Center",
                        labelClass = ExploreStyles.HiddenLabel,
                        units = props.units.symbol,
                        validFormat = props.units.toInputFormat,
                        changeAuditor = props.units.toAuditor,
                        disabled = props.wavelengthReadonly
                      )
                    ),
                    ExposureTimeModeEditor(
                      instrument = props.instrument,
                      wavelength = wavelength.value.some,
                      exposureTimeMode = swView.zoom(GnirsCentralWavelengthConfig.exposureTimeMode),
                      coadds = swView.zoom(GnirsCentralWavelengthConfig.coadds).some,
                      scienceMode = ScienceMode.Spectroscopy,
                      readonly = props.exposureTimeModeReadonly,
                      units = props.units,
                      calibrationRole = props.calibrationRole,
                      idPrefix = NonEmptyString.unsafeFrom(s"gnirsWavelength$idx"),
                      forGridRow = true
                    )
                  )
              ),
            Button(
              icon = Icons.ThinPlus,
              severity = Button.Severity.Success,
              clazz = ExploreStyles.GnirsWavelengthGridAction,
              text = true,
              disabled =
                props.wavelengthReadonly || nextWavelength(props.wavelengthsView.get).isEmpty,
              onClick = nextWavelength(props.wavelengthsView.get).foldMap: w =>
                localView.mod(
                  _ :+ GnirsCentralWavelengthConfig(
                    CentralWavelength(w),
                    // There should always be one, but fall back to the last row's.
                    props.requirementsExposureTimeMode
                      .getOrElse(props.wavelengthsView.get.last.exposureTimeMode),
                    props.wavelengthsView.get.last.coadds
                  )
                )
            ).tiny.compact,
            <.span(ExploreStyles.GnirsWavelengthCenter, "Add wavelength")
          )
        )
