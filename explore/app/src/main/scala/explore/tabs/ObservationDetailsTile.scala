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
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
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
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean
) extends Tile[ObservationDetailsTile](
      ObsTabTileIds.DetailsId.id,
      "Observation Details",
      autoHeight = true,
      autoHeightMinRows = 4
    )(ObservationDetailsTile):
  // A program with no allocations has no band to pick, so the selector would offer nothing.
  val showScienceBand: Boolean = allocatedScienceBands.nonEmpty

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
                  ObservationPropertiesInput(scienceBand = band.orUnassign)
                )
                .runAsync

        val scienceBandSelector: VdomNode =
          FormEnumDropdownOptionalView(
            id = NonEmptyString.unsafeFrom(s"science-band-${props.observation.get.id}"),
            value = scienceBandView,
            label = "Band",
            // Only the bands the program actually holds an allocation for can be chosen.
            exclude = Enumerated[ScienceBand].all.toSet -- props.allocatedScienceBands,
            disabled = props.readonly,
            clazz = ExploreStyles.ObservationDetailsBand
          )

        val estimatedDuration: VdomNode =
          digest.value.fold(EmptyVdom): d =>
            val setupCount: Int = d.setupCount.value

            <.div(ExploreStyles.ObservationDetailsColumn)(
              <.div(ExploreStyles.ObservationDetailsSection, digest.staleClass)(
                "Estimated Duration"
              )
                .withOptionalTooltip(digest.staleTooltip),
              FormInfo(
                duration(d.science.timeEstimate.programTime, scienceTooltip.some),
                "Science Sequence"
              ),
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
              scienceBandSelector.when(props.showScienceBand)
            ),
            estimatedDuration
          )
    )
