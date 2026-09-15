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
        <.div(ExploreStyles.ObservationDetailsForm)(
          FormInfo(props.observation.get.referenceWithId, "Observation")
        )
    )
