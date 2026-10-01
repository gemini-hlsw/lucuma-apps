// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.Visit
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import org.typelevel.cats.time.given

import java.time.LocalDate

/**
 * One visit's charges between nautical twilights, in program time.
 */
final case class TimeChargeRow(
  visitId:     Visit.Id,
  night:       Option[LocalDate],
  interval:    Option[TimestampInterval],
  nightTime:   TimeSpan,
  discounts:   List[VisitTimeCharge.Discount],
  corrections: List[VisitTimeCharge.Correction],
  charged:     TimeSpan
) derives Eq:
  lazy val discountTime: TimeSpan = discounts.foldMap(_.amount)

  lazy val correction: Option[TimeCharges.Signed] = TimeCharges.Signed.net(corrections)

enum TimeCharges derives Eq:
  case NoVisits
  case NoNightVisits
  case Rows(rows: NonEmptyList[TimeChargeRow])

object TimeCharges:
  def fromVisits(visits: List[VisitTimeCharge]): TimeCharges =
    if visits.isEmpty then NoVisits
    else visits.reverse.flatMap(_.nightRow).toNel.fold(NoNightVisits)(Rows(_))

  final case class Signed(op: TimeChargeCorrection.Op, amount: TimeSpan) derives Eq

  object Signed:
    private def combine(all: List[Signed]): Option[Signed] =
      val micros = all.foldMap: s =>
        s.op match
          case TimeChargeCorrection.Op.Add      => s.amount.toMicroseconds
          case TimeChargeCorrection.Op.Subtract => -s.amount.toMicroseconds
      Option.when(micros =!= 0L):
        val op =
          if micros > 0 then TimeChargeCorrection.Op.Add else TimeChargeCorrection.Op.Subtract
        Signed(op, TimeSpan.unsafeFromMicroseconds(micros.abs))

    def net(corrections: List[VisitTimeCharge.Correction]): Option[Signed] =
      combine(corrections.map(c => Signed(c.op, c.amount)))

    def sum(all: List[Option[Signed]]): Option[Signed] =
      combine(all.flattenOption)

  final case class Total(
    nightTime:    TimeSpan,
    discountTime: TimeSpan,
    correction:   Option[Signed],
    charged:      TimeSpan
  ) derives Eq

  object Total:
    def of(rows: NonEmptyList[TimeChargeRow]): Total =
      Total(
        rows.foldMap(_.nightTime),
        rows.foldMap(_.discountTime),
        Signed.sum(rows.toList.map(_.correction)),
        rows.foldMap(_.charged)
      )
