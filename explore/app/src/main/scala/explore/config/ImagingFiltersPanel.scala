// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.utils.*
import monocle.Lens

case class ImagingFiltersPanel[ImagingFilter, Filter](
  instrument:                   Instrument,
  filtersView:                  View[NonEmptyList[ImagingFilter]],
  filterLens:                   Lens[ImagingFilter, Filter],
  etmLens:                      Lens[ImagingFilter, ExposureTimeMode],
  initialFilters:               NonEmptyList[ImagingFilter],
  allowedFilters:               Set[Filter],
  makeImagingFilter:            (Filter, ExposureTimeMode) => ImagingFilter,
  requirementsExposureTimeMode: Option[ExposureTimeMode],
  units:                        WavelengthUnits,
  calibrationRole:              Option[CalibrationRole],
  allowRevertCustomization:     Boolean,
  readonly:                     Boolean,
  showCustomization:            Boolean
)(using
  val enumerated:               Enumerated[Filter],
  val display:                  Display[Filter],
  val filterReuse:              Reusability[Filter],
  val imagingFilterReuse:       Reusability[ImagingFilter],
  val eq:                       Eq[ImagingFilter]
) extends ReactFnProps(ImagingFiltersPanel.component)

object ImagingFiltersPanel:
  private def buildComponent[ImagingFilter, Filter] =
    ScalaFnComponent[ImagingFiltersPanel[ImagingFilter, Filter]] { props =>
      import props.given

      for
        // for editing purposes, we need a view that will allow the user to delete all of the filters
        unModdedFiltersView <- useStateView(List.empty[ImagingFilter])
        _                   <- useEffectWithDeps(props.filtersView.get.toList)(unModdedFiltersView.set)
        baseExclude         <- useMemo(props.allowedFilters): allowed =>
                                 Enumerated[Filter].all.toSet -- allowed
        excludeFilters      <- useMemo(props.filtersView.get.map(props.filterLens.get)): current =>
                                 baseExclude ++ current.toList.toSet
        newFilterView       <- useStateView(Enumerated[Filter].all.headOption)
        _                   <- useEffectWithDeps(excludeFilters.value): efs =>
                                 if newFilterView.get.forall(efs.contains) then
                                   newFilterView.set(Enumerated[Filter].all.filterNot(efs.contains).headOption)
                                 else Callback.empty
      yield
        val localFiltersView: View[List[ImagingFilter]] =
          unModdedFiltersView
            .withModPatch(_.sortBy(props.filterLens.get))
            .withOnMod: l =>
              NonEmptyList
                .fromList(l)
                .fold(Callback.empty)(props.filtersView.set)

        Panel(
          header = <.span(
            "Filters",
            HelpIcon("configuration/gmos/imaging-filters.md".refined),
            CustomizedGroupAddon(
              "original",
              props.filtersView.set(props.initialFilters),
              props.allowRevertCustomization
            ).when(props.showCustomization && props.initialFilters =!= props.filtersView.get),
            <.span(
              Icons.ErrorIcon
            ).withTooltip(
              content = "At least one filter is required."
            ).when(localFiltersView.get.length === 0)
          ),
          toggleable = true,
          collapsed = false
        )(
          <.div(ExploreStyles.ImagingFilterGrid)(
            <.span(), // the action button
            <.span("Filter", ExploreStyles.ImagingFilterGridHeader),
            <.span("Exposure Mode", ExploreStyles.ImagingFilterGridHeader),
            <.span("Signal/Noise", ExploreStyles.ImagingFilterGridHeader),
            <.span("Exp. Time", ExploreStyles.ImagingFilterGridHeader),
            <.span("Number of Exp.", ExploreStyles.ImagingFilterGridHeader),
            localFiltersView.toListOfViews.zipWithIndex
              .toReactFragment(using
                (imagingFilterView, idx) =>
                  val filter = props.filterLens.get(imagingFilterView.get)
                  React.Fragment(
                    Button(
                      icon = Icons.Trash,
                      clazz = ExploreStyles.ImagingFilterGridAction,
                      text = true,
                      disabled = props.readonly,
                      onClick = localFiltersView.mod(
                        _.filterNot(imf => props.filterLens.get(imf) === filter)
                      )
                    ).tiny.compact,
                    <.span(
                      ExploreStyles.ImagingFilter,
                      FormEnumDropdownView(
                        id = NonEmptyString.unsafeFrom(s"imgFilterFilter$idx"),
                        value = imagingFilterView.zoom(props.filterLens),
                        label = "Filter",
                        clazz = ExploreStyles.ImagingFilter,
                        labelClass = ExploreStyles.HiddenLabel,
                        disabled = props.readonly,
                        exclude = excludeFilters - filter
                      )
                    ),
                    ExposureTimeModeEditor(
                      instrument = props.instrument.some,
                      wavelength = none,
                      exposureTimeMode = imagingFilterView.zoom(props.etmLens),
                      coadds = none,
                      scienceMode = ScienceMode.Imaging,
                      readonly = props.readonly,
                      units = props.units,
                      calibrationRole = props.calibrationRole,
                      idPrefix = NonEmptyString.unsafeFrom(s"imgFilter$idx"),
                      forGridRow = true
                    )
                  )
              ),
            newFilterView.mapValue((fv: View[Filter]) =>
              React.Fragment(
                Button(
                  icon = Icons.ThinPlus,
                  severity = Button.Severity.Success,
                  clazz = ExploreStyles.ImagingFilterGridAction,
                  text = true,
                  disabled = props.readonly,
                  onClick = localFiltersView.mod(
                    _ :+ props.makeImagingFilter(
                      fv.get,
                      // there should always be one, but...
                      props.requirementsExposureTimeMode.getOrElse(
                        ExposureTimeMode.SignalToNoiseMode(SignalToNoise.Min, Wavelength.Min)
                      )
                    )
                  )
                ).tiny.compact,
                FormEnumDropdownView(
                  id = "imgFilterAdd".refined,
                  value = fv,
                  label = "Add Filter",
                  clazz = ExploreStyles.ImagingFilter,
                  labelClass = ExploreStyles.HiddenLabel,
                  disabled = props.readonly,
                  exclude = excludeFilters.value
                )
              )
            )
          )
        )
    }

  private val component = buildComponent[Any, Any]
