// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.timeaccounting.given

/**
 * A visit's time charge invoice as computed by the ODB, along with what is needed to place it on
 * the observing night.
 */
final case class VisitTimeCharge(
  visitId:       Visit.Id,
  site:          Site,
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
        nightInterval.map(i =>
          ObservingNight.fromSiteAndInstant(site, i.start.toInstant).toLocalDate
        ),
        nightInterval,
        nightTime,
        otherDiscounts,
        corrections.filter(_.chargeClass === ChargeClass.Program),
        finalCharge(ChargeClass.Program)
      )

object VisitTimeCharge:
  enum DiscountKind(val typename: String, val label: String) derives Eq:
    case Daylight extends DiscountKind("TimeChargeDaylightDiscount", "Daylight")
    case NoData   extends DiscountKind("TimeChargeNoDataDiscount", "No data")
    case Overlap  extends DiscountKind("TimeChargeOverlapDiscount", "Overlap")
    case Qa       extends DiscountKind("TimeChargeQaDiscount", "QA")

  object DiscountKind:
    def fromTypename(typename: String): Option[DiscountKind] =
      values.find(_.typename === typename)

  final case class Discount(
    kind:     DiscountKind,
    interval: TimestampInterval,
    amount:   TimeSpan,
    comment:  Option[String]
  ) derives Eq

  final case class Correction(
    chargeClass: ChargeClass,
    op:          TimeChargeCorrection.Op,
    amount:      TimeSpan,
    comment:     Option[String]
  ) derives Eq

  given Decoder[Discount] = Decoder.instance: c =>
    for
      t <- c.get[String]("__typename")
      k <- DiscountKind
             .fromTypename(t)
             .toRight(DecodingFailure(s"Unknown time charge discount type $t", c.history))
      i <- c.get[TimestampInterval]("interval")
      a <- c.get[TimeSpan]("amount")
      m <- c.get[String]("comment")
    yield Discount(k, i, a, Option.when(m.nonEmpty)(m))

  given Decoder[Correction] = Decoder.instance: c =>
    for
      z <- c.get[ChargeClass]("chargeClass")
      o <- c.get[TimeChargeCorrection.Op]("op")
      a <- c.get[TimeSpan]("amount")
      m <- c.get[Option[String]]("comment")
    yield Correction(z, o, a, m)

  given Decoder[VisitTimeCharge] = Decoder.instance: c =>
    val invoice = c.downField("timeChargeInvoice")
    for
      id <- c.get[Visit.Id]("id")
      s  <- c.get[Site]("site")
      i  <- c.get[Option[TimestampInterval]]("interval")
      e  <- invoice.get[CategorizedTime]("executionTime")
      d  <- invoice.get[List[Discount]]("discounts")
      r  <- invoice.get[List[Correction]]("corrections")
      f  <- invoice.get[CategorizedTime]("finalCharge")
    yield VisitTimeCharge(id, s, i, e, d, r, f)
