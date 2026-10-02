// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.enums.ChargeClass
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.timeaccounting.given

/**
 * A visit's time charge invoice as computed by the ODB.
 */
final case class VisitTimeCharge(
  visitId:       Visit.Id,
  interval:      Option[TimestampInterval],
  executionTime: CategorizedTime,
  discounts:     List[VisitTimeCharge.Discount],
  corrections:   List[VisitTimeCharge.Correction],
  finalCharge:   CategorizedTime
) derives Eq:
  private lazy val (daylight, otherDiscounts) =
    discounts.partition(_.kind === VisitTimeCharge.DiscountKind.Daylight)

  /**
   * The visit restricted to nautical twilight. The ODB already worked out the daylight portions as
   * discounts, so they are removed here rather than recomputing twilight. A visit spent wholly in
   * daylight has no row.
   */
  lazy val nightRow: Option[TimeChargeRow] =
    val nightTime = executionTime(ChargeClass.Program) -| daylight.foldMap(_.amount)
    Option.unless(daylight.nonEmpty && nightTime.isZero):
      val nightInterval =
        interval.flatMap: i =>
          daylight.map(_.interval).foldLeft(List(i))((rem, d) => rem.flatMap(_.minus(d))) match
            case Nil  => none
            case rest => TimestampInterval.between(rest.head.start, rest.last.end).some
      TimeChargeRow(
        visitId,
        nightInterval,
        nightTime -| otherDiscounts.foldMap(_.amount),
        corrections.filter(_.chargeClass === ChargeClass.Program),
        finalCharge(ChargeClass.Program)
      )

object VisitTimeCharge:
  enum DiscountKind(val typename: String) derives Eq:
    case Daylight extends DiscountKind("TimeChargeDaylightDiscount")
    case NoData   extends DiscountKind("TimeChargeNoDataDiscount")
    case Overlap  extends DiscountKind("TimeChargeOverlapDiscount")
    case Qa       extends DiscountKind("TimeChargeQaDiscount")

  object DiscountKind:
    def fromTypename(typename: String): Option[DiscountKind] =
      values.find(_.typename === typename)

  final case class Discount(
    kind:     DiscountKind,
    interval: TimestampInterval,
    amount:   TimeSpan
  ) derives Eq

  final case class Correction(
    created:     Timestamp,
    user:        Option[User.Id],
    chargeClass: ChargeClass,
    op:          TimeChargeCorrection.Op,
    amount:      TimeSpan,
    comment:     Option[String]
  ) derives Eq

  given Decoder[Discount] = c =>
    for
      typename <- c.get[String]("__typename")
      kind     <- DiscountKind
                    .fromTypename(typename)
                    .toRight(DecodingFailure(s"Unknown time charge discount type $typename", c.history))
      interval <- c.get[TimestampInterval]("interval")
      amount   <- c.get[TimeSpan]("amount")
    yield Discount(kind, interval, amount)

  given Decoder[Correction] = c =>
    for
      created     <- c.get[Timestamp]("created")
      user        <- c.get[Option[User.Id]]("user")(using
                       Decoder.decodeOption(using Decoder.instance(_.get[User.Id]("id")))
                     )
      chargeClass <- c.get[ChargeClass]("chargeClass")
      op          <- c.get[TimeChargeCorrection.Op]("op")
      amount      <- c.get[TimeSpan]("amount")
      comment     <- c.get[Option[String]]("comment")
    yield Correction(created, user, chargeClass, op, amount, comment)

  given Decoder[VisitTimeCharge] = c =>
    val invoice = c.downField("timeChargeInvoice")
    for
      id          <- c.get[Visit.Id]("id")
      interval    <- c.get[Option[TimestampInterval]]("interval")
      execution   <- invoice.get[CategorizedTime]("executionTime")
      discounts   <- invoice.get[List[Discount]]("discounts")
      corrections <- invoice.get[List[Correction]]("corrections")
      charge      <- invoice.get[CategorizedTime]("finalCharge")
    yield VisitTimeCharge(id, interval, execution, discounts, corrections, charge)
