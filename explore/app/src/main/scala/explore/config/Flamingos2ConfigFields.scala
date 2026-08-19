// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

/**
 * The Flamingos 2 spectroscopy form, shared by the long slit and MOS panels.
 *
 * The focal plane control differs between the two modes — long slit picks a builtin FPU, MOS picks
 * a custom mask — so it is supplied as a slot rather than described by a flag. Everything else is
 * identical, including the exposure time mode block, the wavelength readout and the offsets editor.
 *
 * The caller owns the `ConfigEditState` machinery and passes the resolved flags in, and owns its
 * own lower grid: long slit has an acquisition panel, MOS has none.
 */
final case class Flamingos2ConfigFields[P <: SlitOffsetPreset](
  fpuControl:                   VdomNode,
  deckerView:                   View[Option[Flamingos2Decker]],
  defaultDecker:                Flamingos2Decker,
  filterView:                   View[Flamingos2Filter],
  initialFilter:                Flamingos2Filter,
  disperserView:                View[Flamingos2Disperser],
  initialDisperser:             Flamingos2Disperser,
  readModeView:                 View[Option[Flamingos2ReadMode]],
  exposureTimeMode:             View[ExposureTimeMode],
  explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]],
  defaultTelescopeConfigs:      SlitTelescopeConfigs,
  defaultForPreset:             P => SlitTelescopeConfigs,
  offsetsHelpId:                NonEmptyString,
  instrument:                   Option[Instrument],
  modeData:                     Reusable[Option[ModeData]],
  units:                        WavelengthUnits,
  calibrationRole:              Option[CalibrationRole],
  etmIdPrefix:                  NonEmptyString,
  isStaff:                      Boolean,
  disableSimpleEdit:            Boolean,
  disableAdvancedEdit:          Boolean,
  showCustomization:            Boolean,
  allowRevertCustomization:     Boolean,
  etmReadonly:                  Boolean,
  presetsReadonly:              Boolean
)(using val enumerated: Enumerated[P], val display: Display[P])
    extends ReactFnProps(Flamingos2ConfigFields.component)

object Flamingos2ConfigFields:
  private type Preset = SlitOffsetPreset

  private val excludedSpectroscopyFilters: Set[Flamingos2Filter] =
    Enumerated[Flamingos2Filter].all.filterNot(_.supportsSpectroscopy).toSet

  private def buildComponent[P <: SlitOffsetPreset] =
    ScalaFnComponent[Flamingos2ConfigFields[P]]: props =>
      import props.given
      import Flamingos2Givens.given

      <.div(ExploreStyles.Flamingos2UpperGrid)(
        <.div(LucumaPrimeStyles.FormColumnCompact)(
          props.fpuControl,
          FormLabel(htmlFor = "decker".refined)("Decker",
                                                HelpIcon("configuration/f2/decker.md".refined)
          ),
          if (props.isStaff)
            CustomizableEnumSelectOptional(
              id = "decker".refined,
              view = props.deckerView.withDefault(props.defaultDecker),
              defaultValue = props.defaultDecker.some,
              disabled = props.disableAdvancedEdit,
              showCustomization = props.showCustomization,
              allowRevertCustomization = props.allowRevertCustomization
            )
          else
            <.label(^.id := "decker",
                    ExploreStyles.FormValue,
                    props.deckerView.get.getOrElse(props.defaultDecker).shortName
            ),
          CustomizableEnumSelect(
            id = "filter".refined,
            view = props.filterView,
            defaultValue = props.initialFilter,
            label = "Filter".some,
            exclude = excludedSpectroscopyFilters,
            helpId = Some("configuration/f2/filter.md".refined),
            disabled = props.disableSimpleEdit,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          ),
          CustomizableEnumSelect(
            id = "disperser".refined,
            view = props.disperserView,
            defaultValue = props.initialDisperser,
            label = "Disperser".some,
            helpId = Some("configuration/f2/disperser.md".refined),
            disabled = props.disableSimpleEdit,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          ),
          CustomizableEnumSelect(
            id = "read-mode".refined,
            view = props.readModeView,
            defaultValue = None,
            label = "Read Mode".some,
            helpId = Some("configuration/f2/read-mode.md".refined),
            disabled = props.disableSimpleEdit,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          )
        ),
        <.div(LucumaPrimeStyles.FormColumnCompact)(
          ExposureTimeModeEditor(
            instrument = props.instrument,
            wavelength = none,
            exposureTimeMode = props.exposureTimeMode,
            coadds = none,
            scienceMode = ScienceMode.Spectroscopy,
            readonly = props.etmReadonly,
            units = props.units,
            calibrationRole = props.calibrationRole,
            idPrefix = props.etmIdPrefix
          ),
          // Per Andy, we'll use the wavelength of the filter as the central wavelength
          LambdaAndIntervalFormValues(
            modeData = props.modeData,
            centralWavelength = props.filterView.get.wavelength,
            units = props.units
          )
        ),
        <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
          SlitTelescopeConfigsEditor(
            explicitValue = props.explicitTelescopeConfigsView,
            defaultValue = props.defaultTelescopeConfigs,
            defaultForPreset = props.defaultForPreset,
            helpId = props.offsetsHelpId,
            presetsReadonly = props.presetsReadonly,
            editingReadonly = props.disableSimpleEdit
          )
        )
      )

  private val component = buildComponent[Preset]
