// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import io.circe.parser.decode
import lucuma.core.enums.ObservationQaState
import munit.FunSuite

class ObservationQaSuite extends FunSuite:

  // `override` is aliased to `stateOverride` in `ObservationSubquery`, as it is a Scala keyword.
  test("decode QA as returned by the ODB"):
    assertEquals(
      decode[ObservationQa]("""{ "computed": "USABLE", "stateOverride": "PASS" }"""),
      ObservationQa(ObservationQaState.Usable.some, ObservationQaState.Pass.some).asRight
    )
    assertEquals(
      decode[ObservationQa]("""{ "computed": null, "stateOverride": null }"""),
      ObservationQa(none, none).asRight
    )

  test("the override takes precedence over the computed state"):
    assertEquals(
      ObservationQa(ObservationQaState.Usable.some, ObservationQaState.Fail.some).state,
      ObservationQaState.Fail.some
    )
    assertEquals(
      ObservationQa(ObservationQaState.Usable.some, none).state,
      ObservationQaState.Usable.some
    )
    assertEquals(
      ObservationQa(none, ObservationQaState.Pass.some).state,
      ObservationQaState.Pass.some
    )
    assertEquals(ObservationQa(none, none).state, none)

  test("clearing the override restores the computed state"):
    val qa = ObservationQa(ObservationQaState.Usable.some, ObservationQaState.Fail.some)
    assertEquals(
      ObservationQa.stateOverride.replace(none)(qa).state,
      ObservationQaState.Usable.some
    )
