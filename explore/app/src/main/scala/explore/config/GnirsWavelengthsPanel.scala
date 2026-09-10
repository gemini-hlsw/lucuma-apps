// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

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
import lucuma.core.model.ExposureTimeMode
import lucuma.react.SizePx
import lucuma.react.common.ReactFnProps
import lucuma.react.pragmaticdnd.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.primereact.tooltip.*
import lucuma.react.syntax.*
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode.GnirsCentralWavelengthConfig
import lucuma.ui.dnd.*
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

  // Roughly a row height, so the gap that opens under the pointer reads as "the row
  // goes here" rather than as a hairline.
  private val DropGapHeight: SizePx = 30.toPx

  private val component =
    ScalaFnComponent[GnirsWavelengthsPanel]: props =>
      for
        // As in ImagingFiltersPanel, edit through a plain list so a row can be
        // removed even though the model requires at least one.
        unModdedView <- useStateView(List.empty[GnirsCentralWavelengthConfig])
        _            <- useEffectWithDeps(props.wavelengthsView.get.toList)(unModdedView.set)
        // The drag-and-drop handlers below are registered once, on mount, so they must
        // not close over anything that changes: they would keep seeing the mount-time
        // value -- an empty list -- forever.  Everything they read comes through a ref.
        rowsRef      <- useShadowRef(unModdedView.get)
        commitRef    <- useShadowRef: (rows: List[GnirsCentralWavelengthConfig]) =>
                          unModdedView.set(rows) >>
                            NonEmptyList.fromList(rows).fold(Callback.empty)(props.wavelengthsView.set)
        dndScope     <- useDragAndDropScope[Int, Int](
                          onDrop = payload =>
                            val from: Int = payload.source.data.value
                            payload.location.current.dropTargets.toList.headOption
                              .map(_.data)
                              .flatMap(d => d.extractClosestEdge.tupleLeft(d.value))
                              .fold(Callback.empty): (nextTo, edge) =>
                                for
                                  rows   <- rowsRef.get
                                  commit <- commitRef.get
                                  _      <- commit(moveInList(from, nextTo, edge)(rows))
                                yield ()
                        )
      yield
        val localView: View[List[GnirsCentralWavelengthConfig]] =
          unModdedView
            .withOnMod: l =>
              NonEmptyList.fromList(l).fold(Callback.empty)(props.wavelengthsView.set)

        dndScope.context(
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
              <.span(), // the drag handle
              <.span(), // the action button
              <.span("λ Center", ExploreStyles.GnirsWavelengthGridHeader),
              <.span("Exposure Mode", ExploreStyles.GnirsWavelengthGridHeader),
              <.span("Signal/Noise", ExploreStyles.GnirsWavelengthGridHeader),
              <.span("Exp. Time", ExploreStyles.GnirsWavelengthGridHeader),
              <.span("Number of Exp.", ExploreStyles.GnirsWavelengthGridHeader),
              <.span("Coadds", ExploreStyles.GnirsWavelengthGridHeader),
              <.span(Constants.SignalToNoiseAtLabel, ExploreStyles.GnirsWavelengthGridHeader),
              // Each row is a subgrid spanning every column, so it is a single element to
              // drag while its controls still line up with the headers and with each other.
              // The rows are identified by position, not by wavelength: a wavelength may
              // appear more than once, and the order is the user's.
              localView.toListOfViews.zipWithIndex
                .toReactFragment(using
                  (swView, idx) =>
                    val wavelength = swView.get.centralWavelength

                    // The drop gap opens on the row the pointer is over, on the side the
                    // dragged row would land.
                    val dropGap: TagMod =
                      dndScope.dragOver.headOption
                        .map(_.data)
                        .filter(d =>
                          d.value === idx && !dndScope.dragging.map(_.value).contains_(idx)
                        )
                        .flatMap(_.extractClosestEdge)
                        .fold(TagMod.empty)(dragOverStyle(DropGapHeight, _))

                    DraggableDropTargetWithHandle[Int, Int](
                      handleRef =>
                        <.div(ExploreStyles.GnirsWavelengthRow)(
                          dropGap,
                          // The handle element stays mounted even when the configuration is
                          // not customized: the drag registration happens once, on mount, so
                          // a handle that appeared later would never become draggable.  Only
                          // the grip icon comes and goes.
                          <.span(ExploreStyles.GnirsWavelengthDragHandle)(
                            if props.wavelengthReadonly then EmptyVdom else Icons.GripDotsVertical
                          ).withRef(handleRef),
                          Button(
                            icon = Icons.Trash,
                            clazz = ExploreStyles.GnirsWavelengthGridAction,
                            text = true,
                            disabled = props.wavelengthReadonly,
                            // By position, not by value: with duplicates allowed, deleting by
                            // wavelength would remove every row that shares it.
                            onClick = localView.mod(l => l.take(idx) ++ l.drop(idx + 1))
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
                            exposureTimeMode =
                              swView.zoom(GnirsCentralWavelengthConfig.exposureTimeMode),
                            coadds = swView.zoom(GnirsCentralWavelengthConfig.coadds).some,
                            scienceMode = ScienceMode.Spectroscopy,
                            readonly = props.exposureTimeModeReadonly,
                            units = props.units,
                            calibrationRole = props.calibrationRole,
                            idPrefix = NonEmptyString.unsafeFrom(s"gnirsWavelength$idx"),
                            forGridRow = true
                          )
                        ),
                      getInitialData = _ => Data(idx),
                      getData = args => Data(idx).attachClosestEdge(args, Axes.Vertical.edges),
                      canDrag = _ => !props.wavelengthReadonly
                    ).withKey(s"gnirs-wavelength-$idx").toUnmounted: VdomNode
                ),
              <.span(), // the new row's drag handle column
              // A new row copies the last one: duplicate wavelengths are legal now, so
              // there is no need to invent a distinct one, and the user edits the row
              // anyway.
              Button(
                icon = Icons.ThinPlus,
                severity = Button.Severity.Success,
                clazz = ExploreStyles.GnirsWavelengthGridAction,
                text = true,
                disabled = props.wavelengthReadonly,
                onClick = localView.mod: l =>
                  val last = l.lastOption.getOrElse(props.wavelengthsView.get.last)
                  l :+ GnirsCentralWavelengthConfig(
                    last.centralWavelength,
                    // There should always be one, but fall back to the last row's.
                    props.requirementsExposureTimeMode.getOrElse(last.exposureTimeMode),
                    last.coadds
                  )
              ).tiny.compact,
              <.span(ExploreStyles.GnirsWavelengthCenter, "Add wavelength")
            )
          )
        )
