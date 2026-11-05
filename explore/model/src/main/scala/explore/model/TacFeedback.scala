// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

/** The time a partner TAC recommends for a science band. */
case class TacRecommendedTime(band: ScienceBand, time: TimeSpan) derives Eq, Decoder

/**
 * A partner TAC's feedback on a proposal. The ODB returns the recommended times sorted by band.
 */
case class TacFeedback(
  partner:          Partner,
  decision:         TacPartnerDecision,
  ranking:          PosBigDecimal,
  recommendedTimes: List[TacRecommendedTime],
  comments:         Option[NonEmptyString]
) derives Eq,
      Decoder
