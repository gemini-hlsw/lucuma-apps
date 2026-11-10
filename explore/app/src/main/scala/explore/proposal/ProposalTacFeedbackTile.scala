// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import explore.components.Tile
import explore.components.TileComponent
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.model.ProposalTabTileIds
import explore.model.TacFeedback
import explore.model.TacRecommendedTime
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.*
import lucuma.ui.format.TimeSpanFormatter

final case class ProposalTacFeedbackTile(feedback: List[TacFeedback])
    extends Tile[ProposalTacFeedbackTile](
      id = ProposalTabTileIds.TacFeedbackId.id,
      title = "TAC Feedback",
      autoHeight = true,
      autoHeightMinRows = 3
    )(ProposalTacFeedbackTile)

object ProposalTacFeedbackTile
    extends TileComponent[ProposalTacFeedbackTile]((props, _) =>
      def label(text: String): VdomNode =
        <.span(ExploreStyles.ProposalTacFeedbackLabel, text)

      def recommendedTimes(times: List[TacRecommendedTime]): String =
        if (times.isEmpty) "None"
        else
          times
            .map(t => s"${TimeSpanFormatter.DecimalHours.format(t.time)} ${t.band.shortName}")
            .mkString(", ")

      // The headings of all partners share the columns of a single grid, so
      // they line up when laid out on one line.
      def partnerFeedback(f: TacFeedback): VdomNode =
        React.Fragment.withKey(f.timeAccountingCategory.tag)(
          // Only the categories that belong to a partner have a flag.
          <.span(ExploreStyles.ProposalTacFeedbackPartner)(
            f.timeAccountingCategory.renderFlag,
            f.timeAccountingCategory.description
          ),
          label("Decision:"),
          <.span(f.decision.shortName),
          label("Partner Ranking:"),
          <.span(f.ranking.value.bigDecimal.stripTrailingZeros.toPlainString),
          label("Recommended Time:"),
          <.span(recommendedTimes(f.recommendedTimes)),
          f.comments.map(c => <.div(ExploreStyles.ProposalTacFeedbackComments, c.value))
        )

      TileContents(
        body = <.div(ExploreStyles.ProposalTacFeedbackTile)(
          if (props.feedback.isEmpty)
            <.div(ExploreStyles.ProposalTacFeedbackEmpty, "No TAC feedback yet.")
          else
            <.div(ExploreStyles.ProposalTacFeedbackGrid)(
              props.feedback.map(partnerFeedback).toVdomArray
            )
        )
      )
    )
