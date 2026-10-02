// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.Visit
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval

/**
 * One visit's charges between nautical twilights, in program time. The duration is what is charged
 * before staff corrections.
 */
final case class TimeChargeRow(
  visitId:     Visit.Id,
  interval:    Option[TimestampInterval],
  duration:    TimeSpan,
  corrections: List[VisitTimeCharge.Correction],
  charged:     TimeSpan
) derives Eq

enum TimeChargeLine derives Eq:
  def visitId: Visit.Id

  case ForVisit(visitId: Visit.Id, interval: Option[TimestampInterval], duration: TimeSpan)
  case ForCorrection(visitId: Visit.Id, index: Int, correction: VisitTimeCharge.Correction)

enum TimeChargeColumn derives Eq:
  case Visit, Start, End, Duration

  def ordering: Ordering[TimeChargeRow] =
    this match
      case Visit    => Ordering.by(_.visitId)
      case Start    => Ordering.by(_.interval.map(_.start))
      case End      => Ordering.by(_.interval.map(_.end))
      case Duration => Ordering.by(_.duration)

enum TimeCharges derives Eq:
  case NoVisits
  case NoNightVisits
  case Rows(rows: NonEmptyList[TimeChargeRow])

object TimeCharges:
  def fromVisits(visits: List[VisitTimeCharge]): TimeCharges =
    if visits.isEmpty then NoVisits
    else visits.flatMap(_.nightRow).toNel.fold(NoNightVisits)(Rows(_))

  extension (rows: NonEmptyList[TimeChargeRow])
    /**
     * Visits ordered by the given column, each followed by its corrections, oldest correction
     * first. Corrections never move away from their visit.
     */
    def lines(sortBy: TimeChargeColumn, descending: Boolean): List[TimeChargeLine] =
      val ordering = sortBy.ordering
      rows.toList
        .sorted(using if descending then ordering.reverse else ordering)
        .flatMap: r =>
          TimeChargeLine.ForVisit(r.visitId, r.interval, r.duration) ::
            r.corrections
              .sortBy(_.created)
              .zipWithIndex
              .map((c, i) => TimeChargeLine.ForCorrection(r.visitId, i, c))

    def total: TimeSpan = rows.foldMap(_.charged)
