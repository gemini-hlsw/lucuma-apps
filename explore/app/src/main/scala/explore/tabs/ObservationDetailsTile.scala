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
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationPriority
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceBand
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.DurationFormatter
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

        val scienceTooltip: VdomNode =
          "Includes the flats and arcs taken within the science sequence."

        val totalTooltip: VdomNode =
          "Does not include time for telluric standards or other separately scheduled calibrations."

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

        val estimatedDuration: VdomNode =
          digest.value.fold(EmptyVdom): d =>
            val setupCount: Int = d.setupCount.value
            val gcalSets: Int   = d.science.gcalSets.value

            val flats                     = d.science.steps.flats
            val arcs                      = d.science.steps.arcs
            val gcalTotal                 = flats.time.programTime +| arcs.time.programTime
            def secs(t: TimeSpan): String = DurationFormatter(t.toDuration)

            val gcalSetsRow: Option[VdomNode] =
              NonZeroInt
                .from(gcalSets)
                .toOption
                .map: n =>
                  val sets = if gcalSets === 1 then "1 set" else s"$gcalSets sets"
                  FormInfo(s"$sets, ${secs(gcalTotal)} (${secs(gcalTotal /| n)} each)",
                           "Flats & Arcs"
                  )

            <.div(ExploreStyles.ObservationDetailsColumn)(
              <.div(ExploreStyles.ObservationDetailsSection, digest.staleClass)(
                "Estimated Duration"
              )
                .withOptionalTooltip(digest.staleTooltip),
              FormInfo(
                duration(d.science.timeEstimate.programTime, scienceTooltip.some),
                "Science Sequence"
              ),
              gcalSetsRow,
              FormInfo(
                <.span(s"$setupCount × ", duration(d.setup.full)),
                "Setup"
              ),
              FormInfo(duration(d.fullTimeEstimate.programTime, totalTooltip.some), "Total")
            )

        TileContents:
          <.div(ExploreStyles.ObservationDetailsForm)(
            <.div(ExploreStyles.ObservationDetailsColumn)(
              FormInfo(props.observation.get.referenceWithId, "Observation"),
              scienceBandSelector.when(props.showScienceBand),
              prioritySelector
            ),
            estimatedDuration
          )
    )
