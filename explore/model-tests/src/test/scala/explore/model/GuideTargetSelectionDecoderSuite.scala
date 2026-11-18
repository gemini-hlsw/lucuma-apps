// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import io.circe.parser.parse
import lucuma.ags.GuideStarId
import lucuma.core.model.Target
import munit.FunSuite

class GuideTargetSelectionDecoderSuite extends FunSuite:
  private val TargetIdValue: Target.Id = Target.Id.fromLong(42L).get

  private def decode(targetEnvironment: String): Decoder.Result[Option[GuideStarId]] =
    val json: Json = parse(targetEnvironment).toOption.get
    json.hcursor
      .downField("guideTargetSelection")
      .as[Option[GuideStarId]](using Observation.GuideTargetSelectionDecoder)

  test("a missing or null selection is automatic"):
    assertEquals(decode("{}"), none.asRight)
    assertEquals(decode("""{ "guideTargetSelection": null }"""), none.asRight)

  test("a catalog name"):
    assertEquals(
      decode(
        """{ "guideTargetSelection": { "catalogName": "Gaia DR3 123", "observationTarget": null } }"""
      ),
      GuideStarId.GaiaDr3(123L).some.asRight
    )

  test("an observation target with an id"):
    assertEquals(
      decode(
        s"""{ "guideTargetSelection": { "catalogName": null, "observationTarget": { "targetId": "${TargetIdValue.show}" } } }"""
      ),
      GuideStarId.ObservationTarget(TargetIdValue.some).some.asRight
    )

  test("an observation target without an id"):
    assertEquals(
      decode(
        """{ "guideTargetSelection": { "catalogName": null, "observationTarget": { "targetId": null } } }"""
      ),
      GuideStarId.ObservationTarget(none).some.asRight
    )

  test("a catalog name that is not a guide star name is no selection"):
    assertEquals(
      decode(
        """{ "guideTargetSelection": { "catalogName": "Tycho 1-2-3", "observationTarget": null } }"""
      ),
      none.asRight
    )
