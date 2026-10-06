// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all.*
import lucuma.core.model.Target
import lucuma.react.table.ColumnDef

class TargetColumnsSuite extends munit.FunSuite:
  test("column picker lists every program column, in table order"):
    val columns =
      TargetColumns.Builder
        .ForProgram(ColumnDef[Target], _.some, _ => none, _.name.value, _ => none, _ => none)
        .AllColumns

    // The icon column has no header, so it has no entry in the column picker.
    assertEquals(
      columns.map(_.id).filterNot(_ == TargetColumns.IconColumnId),
      TargetColumns.AllColNames.keys.toList
    )
