// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite
import observe.ui.model.arb.ArbRootModel
import org.scalacheck.Test

class RootModelDataSuite extends DisciplineSuite {
  import ArbRootModel.given

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[RootModelDataSuite]", EqTests[RootModelData].eqv)
}
