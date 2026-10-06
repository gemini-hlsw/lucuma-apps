// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import lucuma.core.math.Angle
import lucuma.core.model.GmosIfuAnalysis

class GmosIfuAnalysisKindSuite extends munit.FunSuite:
  import GmosIfuAnalysisKind.*

  private def mas(n: Int): Angle = Angle.milliarcseconds.reverseGet(n)

  private val default: GmosIfuAnalysis    = GmosIfuAnalysis.Default
  private val defaultSum: GmosIfuAnalysis = GmosIfuAnalysis.Sum(GmosIfuAnalysis.DefaultSumRadius)

  test("defaultFor uses the mode default for its own shape and core defaults for the other"):
    assertEquals(defaultFor(default)(Single), default)
    assertEquals(defaultFor(default)(Sum), defaultSum)

    val sumDefault = GmosIfuAnalysis.Sum(mas(500))
    assertEquals(defaultFor(sumDefault)(Sum), sumDefault)
    assertEquals(defaultFor(sumDefault)(Single), GmosIfuAnalysis.Single(Angle.Angle0))

  test("a default angle follows the shape"):
    assertEquals(switchKind(default)(Sum)(default), defaultSum)
    assertEquals(switchKind(default)(Single)(defaultSum), default)

  test("a customized angle is kept"):
    assertEquals(
      switchKind(default)(Sum)(GmosIfuAnalysis.Single(mas(500))),
      GmosIfuAnalysis.Sum(mas(500))
    )
    assertEquals(
      switchKind(default)(Single)(GmosIfuAnalysis.Sum(mas(700))),
      GmosIfuAnalysis.Single(mas(700))
    )

  test("switching to the same shape changes nothing"):
    List(default, defaultSum, GmosIfuAnalysis.Single(mas(500)), GmosIfuAnalysis.Sum(mas(700)))
      .foreach: a =>
        assertEquals(switchKind(default)(fromGmosIfuAnalysis(a))(a), a)

  test("a customized zero offset becomes the default radius"):
    val singleDefault = GmosIfuAnalysis.Single(mas(300))
    assertEquals(
      switchKind(singleDefault)(Sum)(GmosIfuAnalysis.Single(Angle.Angle0)),
      defaultSum
    )
