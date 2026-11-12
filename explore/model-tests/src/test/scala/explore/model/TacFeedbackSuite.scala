// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.parser.decode
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.TimeSpan
import munit.FunSuite

class TacFeedbackSuite extends FunSuite:

  test("decode TAC feedback as returned by the ODB"):
    val json = """
      [
        {
          "timeAccountingCategory": "CA",
          "decision": "ACCEPTED",
          "ranking": 6,
          "recommendedTimes": [
            { "band": "BAND1", "time": { "microseconds": 7200000000 } },
            { "band": "BAND2", "time": { "microseconds": 3600000000 } }
          ],
          "comments": "Great science."
        },
        {
          "timeAccountingCategory": "DD",
          "decision": "REJECTED",
          "ranking": 2.5,
          "recommendedTimes": [],
          "comments": null
        }
      ]
    """

    assertEquals(
      decode[List[TacFeedback]](json),
      List(
        TacFeedback(
          TimeAccountingCategory.CA,
          TacPartnerDecision.Accepted,
          PosBigDecimal.unsafeFrom(BigDecimal(6)),
          List(
            TacRecommendedTime(ScienceBand.Band1, TimeSpan.fromHours(2).get),
            TacRecommendedTime(ScienceBand.Band2, TimeSpan.fromHours(1).get)
          ),
          NonEmptyString.unsafeFrom("Great science.").some
        ),
        TacFeedback(
          TimeAccountingCategory.DD,
          TacPartnerDecision.Rejected,
          PosBigDecimal.unsafeFrom(BigDecimal("2.5")),
          Nil,
          none
        )
      ).asRight
    )
