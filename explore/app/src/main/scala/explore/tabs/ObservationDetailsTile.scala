// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.CalibrationSets
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.VisitTimeCharge
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationPriority
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TooActivation
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.UndoSetter
import monocle.Iso

import scala.collection.immutable.SortedSet

final case class ObservationDetailsTile(
  observation:           UndoSetter[Observation],
  programType:           ProgramType,
  allocatedScienceBands: SortedSet[ScienceBand],
  userId:                Option[User.Id],
  timeCharges:           Option[List[VisitTimeCharge]],
  refreshingCharges:     Boolean,
  refreshTimeCharges:    Callback,
  readonly:              Boolean
) extends Tile[ObservationDetailsTile](
      ObsTabTileIds.DetailsId.id,
      "Observation Details",
      autoHeight = true,
      autoHeightMinRows = 4
    )(ObservationDetailsTile):
  val hasAllocations: Boolean = allocatedScienceBands.nonEmpty

  // Only a science program is ever allocated band time. Other program types would sit forever on
  // an empty selector, so the band is shown for them only when one is somehow already set.
  val showScienceBand: Boolean =
    programType === ProgramType.Science || observation.get.scienceBand.isDefined

object ObservationDetailsTile
    extends TileComponent[ObservationDetailsTile]((props, _) =>
      for ctx <- useContext(AppContext.ctx)
      yield
        import ctx.given

        val digest = props.observation.get.execution.digest

        def duration(time: TimeSpan, tooltip: Option[VdomNode] = none): VdomNode =
          TimeSpanView(time, TimeSpanFormatter.HoursMinutesLetter, tooltip = tooltip)

        def telluricsTooltip(created: Int, expected: Int): VdomNode =
          s"$created created, $expected expected. Created tellurics use their own estimate; " +
            "expected ones use the average of the group's tellurics, or 15m before any exists."

        val totalTooltip: VdomNode =
          "Includes this observation's tellurics. Other separately scheduled calibrations are " +
            "not included."

        val scienceBandView: View[Option[ScienceBand]] =
          props.observation
            .zoom(Observation.scienceBand)
            .undoableView(Iso.id[Option[ScienceBand]].asLens)
            .withOnMod: band =>
              ctx.odbApi
                .updateObservations(
                  List(props.observation.get.id),
                  ObservationPropertiesInput(scienceBand = band.orIgnore)
                )
                .runAsync

        val scienceBandSelector: VdomNode =
          FormEnumDropdownOptionalView(
            id = NonEmptyString.unsafeFrom(s"science-band-${props.observation.get.id}"),
            value = scienceBandView,
            label = "Band",
            // Only bands the program holds an allocation for can be chosen.
            exclude = Enumerated[ScienceBand].all.toSet
              -- props.allocatedScienceBands
              -- scienceBandView.get,
            disabled = props.readonly || !props.hasAllocations,
            showClear = false,
            // A disabled control swallows tooltips, so the reason has to be on its face.
            placeholder = if props.hasAllocations then "Not set" else "No time allocation",
            clazz = ExploreStyles.ObservationDetailsSelect
          )

        val priorityView: View[ObservationPriority] =
          props.observation
            .zoom(Observation.priority)
            .undoableView(Iso.id[ObservationPriority].asLens)
            .withOnMod: priority =>
              ctx.odbApi
                .updateObservations(
                  List(props.observation.get.id),
                  ObservationPropertiesInput(priority = priority.assign)
                )
                .runAsync

        val prioritySelector: VdomNode =
          SelectButtonEnumView(
            id = NonEmptyString.unsafeFrom(s"priority-${props.observation.get.id}"),
            view = priorityView,
            label = "Priority",
            disabled = props.readonly,
            groupClass = ExploreStyles.ObservationDetailsPriority
          )

        // The ODB raises the scheduling mode itself when the activation requires it, so only the
        // activation is sent.
        val tooActivationView: View[TooActivation] =
          props.observation
            .zoom(Observation.tooActivationWithMode)
            .undoableView(Iso.id[TooActivation].asLens)
            .withOnMod: activation =>
              ctx.odbApi
                .updateObservations(
                  List(props.observation.get.id),
                  ObservationPropertiesInput(
                    schedulingConstraints =
                      SchedulingConstraintsInput(tooActivation = activation.assign).assign
                  )
                )
                .runAsync

        val tooActivationSelector: VdomNode =
          FormEnumDropdownView(
            id = NonEmptyString.unsafeFrom(s"too-activation-${props.observation.get.id}"),
            value = tooActivationView,
            label =
              React.Fragment("ToO Activation", HelpIcon("observation/too-activation.md".refined)),
            disabled = props.readonly,
            clazz = ExploreStyles.ObservationDetailsSelect
          )

        val estimatedDuration: VdomNode =
          digest.value.fold(EmptyVdom): d =>
            val setupCount: Int = d.setupCount.value

            val steps     = d.science.steps
            val gcalTotal = steps.flats.time.programTime +| steps.arcs.time.programTime

            val scienceTime: TimeSpan =
              (steps.biases.time |+| steps.darks.time |+| steps.observing.time).programTime

            val total: TimeSpan =
              d.fullTimeEstimate.programTime +| d.calibrations.existing.time.programTime

            val gcalSetsRow: Option[VdomNode] =
              CalibrationSets
                .text(d.science.gcalSets, gcalTotal)
                .map(FormInfo(_, "Flats & Arcs"))

            val cals = d.calibrations

            val telluricsRow: Option[VdomNode] =
              CalibrationSets
                .countTimesEach(cals.count, (cals.existing.time |+| cals.expected.time).programTime)
                .map: text =>
                  val tooltip =
                    telluricsTooltip(cals.existing.count.value, cals.expected.count.value)
                  FormInfo(text, "Tellurics", tooltip = tooltip)

            <.div(ExploreStyles.ObservationDetailsColumn)(
              <.div(ExploreStyles.ObservationDetailsSection, digest.staleClass)(
                "Remaining Estimate Duration"
              )
                .withOptionalTooltip(digest.staleTooltip),
              FormInfo(
                duration(scienceTime),
                "Science Sequence"
              ),
              gcalSetsRow,
              FormInfo(
                <.span(s"$setupCount × ", duration(d.setup.full)),
                "Setup"
              ),
              telluricsRow,
              FormInfo(
                <.span(ExploreStyles.ObservationDetailsTotal)(duration(total, totalTooltip.some)),
                "Total"
              )
            )

        TileContents:
          <.div(ExploreStyles.ObservationDetailsForm)(
            <.div(ExploreStyles.ObservationDetailsColumn)(
              FormInfo(props.observation.get.referenceWithId, "Observation"),
              scienceBandSelector.when(props.showScienceBand),
              prioritySelector,
              tooActivationSelector.unless(props.observation.get.isCalibration)
            ),
            estimatedDuration,
            TimeChargesTable(props.userId,
                             props.timeCharges,
                             props.refreshingCharges,
                             props.refreshTimeCharges
            )
          )
    )
