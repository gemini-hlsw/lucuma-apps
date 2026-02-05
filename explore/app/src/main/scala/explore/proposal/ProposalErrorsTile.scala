// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.ProposalTabTileIds
import japgolly.scalajs.react.vdom.html_<^.*

final case class ProposalErrorsTile(errors: List[String])
    extends Tile[ProposalErrorsTile](
      id = ProposalTabTileIds.ErrorsId.id,
      title = s"Errors (${errors.size})"
    )(ProposalErrorsTile)

object ProposalErrorsTile
    extends TileComponent[ProposalErrorsTile]((props, _) =>
      TileContents:
        <.div(ExploreStyles.ProposalErrorsTile)(
          props.errors
            .map: e =>
              <.div(Icons.ErrorIcon, e)
            .toTagMod
        )
    )
