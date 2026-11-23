// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.arb.ArbStep.given
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostDynamicConfig.given
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import munit.FunSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import java.util.UUID

final class SequenceRowSuite extends FunSuite:

  private def row[D](using Arbitrary[Step[D]]): SequenceRow[D] =
    SequenceRow.FutureStep(
      arbitrary[Step[D]].sample.get,
      Atom.Id.fromUuid(new UUID(0L, 1L)),
      none,
      none,
      none,
      SequenceType.Science
    )

  // The empty-column check reads every value of every row, for every instrument.
  test("GHOST and IGRINS-2 rows have no grating or filter, and do not throw"):
    List(row[GhostDynamicConfig], row[Igrins2DynamicConfig]).foreach: r =>
      assertEquals(r.gratingName, none)
      assertEquals(r.filterName, none)
    assertEquals(
      SequenceColumns.emptyColumns(List(row[GhostDynamicConfig], row[Igrins2DynamicConfig])),
      Set(
        SequenceColumns.WavelengthColumnId,
        SequenceColumns.FPUColumnId,
        SequenceColumns.GratingColumnId,
        SequenceColumns.FilterColumnId
      )
    )
