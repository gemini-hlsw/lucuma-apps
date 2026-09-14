// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.Observation
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.primereact.FormInfo
import lucuma.ui.undo.UndoSetter

// Nothing is editable yet; the UndoSetter and readonly are carried so that adding the first
// editable field does not mean rewiring the call site.
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
      TileContents:
        // The grid lives on a div of our own: an auto-height tile wraps bodyClass's div in
        // another one, which would leave the grid with a single child.
        <.div(ExploreStyles.ObservationDetailsForm)(
          FormInfo(props.observation.get.referenceWithId, "Observation")
        )
    )
