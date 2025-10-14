// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.kernel.laws.discipline.*
import observe.model.enums.KeywordName

class KeywordsSuite extends munit.DisciplineSuite with KeywordArbitraries {
  checkAll("Eq[KeywordName]", EqTests[KeywordName].eqv)
  checkAll("Eq[KeywordType]", EqTests[KeywordType].eqv)
  checkAll("Eq[InternalKeyword]", EqTests[InternalKeyword].eqv)
  checkAll("Eq[KeywordBag]", EqTests[KeywordBag].eqv)
  checkAll("Monoid[KeywordBag]", MonoidTests[KeywordBag].monoid)
}
