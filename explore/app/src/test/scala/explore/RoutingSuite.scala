// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Eq
import explore.model.Page
import explore.model.arb.ArbPage
import explore.model.arb.ArbPage.given
import japgolly.scalajs.react.extra.router.Path
import lucuma.core.model.arb.ArbObservationReference
import lucuma.core.model.arb.ArbProgramReference
import lucuma.core.model.arb.ArbProposalReference
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import scala.compiletime.constValue
import scala.deriving.Mirror

/**
 * Checks the whole routing table against the `Format` laws: every page prints to a path that parses
 * back to it, and every path the router accepts normalizes to a canonical one. Testing the table
 * rather than single routes also catches a rule capturing a path printed by another rule.
 */
class RoutingSuite extends DisciplineSuite:

  private val config = Routing.config

  /** The routing table as a `Format`. Redirects, including not-found, parse to `None`. */
  val routesFormat: Format[Path, Page] =
    Format(path => config.rules.parse(path).runNow().toOption, config.rules.path)

  given Eq[Path] = Eq.fromUniversalEquals

  /** Reverses the ids of every `a:b:c` segment and repeats the first one. */
  private def reorderIdLists(path: String): String =
    path
      .split("/", -1)
      .map: segment =>
        if segment.contains(":") then
          val ids = segment.split(":").toList
          (ids.reverse :+ ids.head).mkString(":")
        else segment
      .mkString("/")

  /**
   * Printed pages, plus perturbations the router must either reject or normalize: reordered and
   * repeated id lists, case changes, slashes, and the non-canonical reference strings from core.
   */
  val paths: Gen[Path] =
    val printed: Gen[String] = arbitrary[Page].map(routesFormat.reverseGet(_).value)

    Gen
      .oneOf(
        printed,
        printed.map(reorderIdLists),
        printed.map(_.toUpperCase),
        printed.map(_.stripPrefix("/")),
        printed.map(_ + "/"),
        ArbProposalReference.proposalReferenceStrings.map("/" + _),
        ArbProgramReference.programReferenceStrings.map("/" + _),
        ArbObservationReference.observationReferenceStrings.map("/" + _),
        arbitrary[String]
      )
      .map(Path(_))

  /**
   * Number of cases of the enum `A`, read at compile time from the tuple of case types in its
   * `Mirror`. It follows the enum definition, so it can't go stale.
   */
  private inline def caseCount[A](using mirror: Mirror.SumOf[A]): Int =
    constValue[Tuple.Size[mirror.MirroredElemTypes]]

  // The laws only cover the cases `ArbPage` generates, so a `Page` case missing there is untested.
  // Takes one page from each generator and collects their ordinals (the position of each case in
  // the enum). They must be exactly (0 until the number of cases): a missing generator leaves a gap,
  // and two generators for the same case collapse into one ordinal.
  test("ArbPage generates every Page case"):
    // A fixed seed is enough: each generator always yields the same case, only the ids vary.
    val generated: Set[Int] =
      ArbPage.pageGens.map(_.pureApply(Gen.Parameters.default, Seed(0L)).ordinal).toSet
    assertEquals(generated, (0 until caseCount[Page]).toSet)

  checkAll("Routing", FormatTests(routesFormat).formatWith(paths))
