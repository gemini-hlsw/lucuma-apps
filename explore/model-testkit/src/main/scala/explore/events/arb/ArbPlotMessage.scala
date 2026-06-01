// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events.arb

import cats.syntax.all.*
import explore.events.PlotMessage
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.model.Tracking
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.model.arb.ArbTracking.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbPlotMessage:
  given Arbitrary[PlotMessage.RequestSemester] =
    Arbitrary:
      for
        semester <- arbitrary[Semester]
        site     <- arbitrary[Site]
        tracking <- arbitrary[Tracking]
        dayRate  <- arbitrary[Long]
      yield PlotMessage.RequestSemester(semester, site, tracking, dayRate)

  given Cogen[PlotMessage.RequestSemester] =
    Cogen[(Semester, Site, Tracking, Long)].contramap(r =>
      (r.semester, r.site, r.tracking, r.dayRate)
    )

  given Arbitrary[PlotMessage.SemesterPoint] =
    Arbitrary:
      for
        epochMilli       <- arbitrary[Long]
        visibilityMillis <- arbitrary[Long]
      yield PlotMessage.SemesterPoint(epochMilli, visibilityMillis)

  given Cogen[PlotMessage.SemesterPoint] =
    Cogen[(Long, Long)].contramap(s => (s.epochMilli, s.visibilityMillis))

  given Arbitrary[PlotMessage.Request] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[PlotMessage.RequestSemester],
        Gen.const(PlotMessage.CleanCache)
      )

  given Cogen[PlotMessage.Request] =
    Cogen[Either[Unit, PlotMessage.RequestSemester]].contramap {
      case PlotMessage.CleanCache         => ().asLeft
      case r: PlotMessage.RequestSemester => r.asRight
    }

object ArbPlotMessage extends ArbPlotMessage
