// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.primereact.FormInfo
import lucuma.ui.undo.UndoSetter

final case class ObservationDetailsTile(
  observation: UndoSetter[Observation],
  readonly:    Boolean
) extends Tile[ObservationDetailsTile](
      ObsTabTileIds.DetailsId.id,
      "Observation Details",
      autoHeight = true,
      autoHeightMinRows = 4
    )(ObservationDetailsTile)

object ObservationDetailsTile
    extends TileComponent[ObservationDetailsTile]((props, _) =>
      val digest = props.observation.get.execution.digest

      // The caveats ride on the TimeSpanView and not on FormInfo: FormInfo hangs its tooltip on the
      // span wrapping the value, and the inner TimeSpanView tooltip would shadow it.
      def duration(time: TimeSpan, caveat: Option[VdomNode] = none): VdomNode =
        TimeSpanView(time, TimeSpanFormatter.HoursMinutesLetter, tooltip = caveat)

      val scienceCaveat: VdomNode =
        "Includes the flats and arcs taken within the science sequence."

      val totalCaveat: VdomNode =
        "Does not include time for telluric standards or other separately scheduled calibrations."

      val estimatedDuration: VdomNode =
        digest.value.fold(EmptyVdom): d =>
          val setupCount: Int = d.setupCount.value

          <.div(ExploreStyles.ObservationDetailsColumn)(
            <.div(ExploreStyles.ObservationDetailsSection, digest.staleClass)("Estimated Duration")
              .withOptionalTooltip(digest.staleTooltip),
            FormInfo(
              duration(d.science.timeEstimate.programTime, scienceCaveat.some),
              "Science Sequence"
            ),
            FormInfo(
              <.span(s"$setupCount × ", duration(d.setup.full)),
              "Setup"
            ),
            FormInfo(duration(d.fullTimeEstimate.programTime, totalCaveat.some), "Total")
          )

      TileContents:
        <.div(ExploreStyles.ObservationDetailsForm)(
          <.div(ExploreStyles.ObservationDetailsColumn)(
            FormInfo(props.observation.get.referenceWithId, "Observation")
          ),
          estimatedDuration
        )
    )
