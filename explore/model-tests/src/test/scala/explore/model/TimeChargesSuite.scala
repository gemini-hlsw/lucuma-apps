// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.parser.decode
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Site
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import munit.FunSuite

import java.time.LocalDate

class TimeChargesSuite extends FunSuite:
  private def ts(s: String): Timestamp = Timestamp.parse(s).toOption.get

  private def minutes(m: Int): TimeSpan = TimeSpan.fromMinutes(m).get

  private def program(m: Int): CategorizedTime =
    CategorizedTime(ChargeClass.Program -> minutes(m))

  private def interval(start: String, end: String): TimestampInterval =
    TimestampInterval.between(ts(start), ts(end))

  private def daylight(start: String, end: String, m: Int): VisitTimeCharge.Discount =
    VisitTimeCharge.Discount(VisitTimeCharge.DiscountKind.Daylight,
                             interval(start, end),
                             minutes(m),
                             none
    )

  private def qa(m: Int): VisitTimeCharge.Discount =
    VisitTimeCharge.Discount(
      VisitTimeCharge.DiscountKind.Qa,
      interval("2026-03-02T01:00:00Z", "2026-03-02T01:10:00Z"),
      minutes(m),
      "bad seeing".some
    )

  private def correction(
    op:          TimeChargeCorrection.Op,
    m:           Int,
    chargeClass: ChargeClass = ChargeClass.Program
  ): VisitTimeCharge.Correction =
    VisitTimeCharge.Correction(chargeClass, op, minutes(m), none)

  private def visit(
    id:          Long,
    span:        Option[TimestampInterval],
    execution:   Int,
    discounts:   List[VisitTimeCharge.Discount] = Nil,
    corrections: List[VisitTimeCharge.Correction] = Nil,
    charged:     Int
  ): VisitTimeCharge =
    VisitTimeCharge(
      Visit.Id.fromLong(id).get,
      Site.GS,
      span,
      program(execution),
      discounts,
      corrections,
      program(charged)
    )

  test("no visits"):
    assertEquals(TimeCharges.fromVisits(Nil), TimeCharges.NoVisits)

  test("a visit wholly at night keeps its interval and full program time"):
    val v   = visit(1,
                  interval("2026-03-02T01:00:00Z", "2026-03-02T03:00:00Z").some,
                  execution = 120,
                  charged = 120
    )
    val row = v.nightRow.get
    assertEquals(row.nightTime, minutes(120))
    assertEquals(row.interval, v.interval)
    assertEquals(row.charged, minutes(120))
    assertEquals(row.night, LocalDate.of(2026, 3, 2).some)

  test("a visit starting before twilight is clipped and loses the daylight time"):
    val v   = visit(
      1,
      interval("2026-03-01T23:35:00Z", "2026-03-02T01:35:00Z").some,
      execution = 120,
      discounts = List(daylight("2026-03-01T23:35:00Z", "2026-03-01T23:50:00Z", 15)),
      charged = 105
    )
    val row = v.nightRow.get
    assertEquals(row.nightTime, minutes(105))
    assertEquals(row.interval, interval("2026-03-01T23:50:00Z", "2026-03-02T01:35:00Z").some)
    assertEquals(row.discounts, Nil)

  test("a visit crossing both twilights is clipped at both ends"):
    val v   = visit(
      1,
      interval("2026-03-01T23:40:00Z", "2026-03-02T09:20:00Z").some,
      execution = 580,
      discounts = List(
        daylight("2026-03-01T23:40:00Z", "2026-03-01T23:50:00Z", 10),
        daylight("2026-03-02T09:10:00Z", "2026-03-02T09:20:00Z", 10)
      ),
      charged = 560
    )
    val row = v.nightRow.get
    assertEquals(row.nightTime, minutes(560))
    assertEquals(row.interval, interval("2026-03-01T23:50:00Z", "2026-03-02T09:10:00Z").some)

  test("a visit wholly in daylight has no row"):
    val v = visit(
      1,
      interval("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z").some,
      execution = 60,
      discounts = List(daylight("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z", 60)),
      charged = 0
    )
    assertEquals(v.nightRow, none)

  test("a visit at night with no program time yet still has a row"):
    val v = visit(1,
                  interval("2026-03-02T01:00:00Z", "2026-03-02T01:00:30Z").some,
                  execution = 0,
                  charged = 0
    )
    assertEquals(v.nightRow.map(_.nightTime), TimeSpan.Zero.some)

  test("only daylight visits are reported as having no night-time visits"):
    val v = visit(
      1,
      interval("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z").some,
      execution = 60,
      discounts = List(daylight("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z", 60)),
      charged = 0
    )
    assertEquals(TimeCharges.fromVisits(List(v)), TimeCharges.NoNightVisits)

  test("other discounts are kept and program corrections are netted"):
    val v   = visit(
      1,
      interval("2026-03-02T01:00:00Z", "2026-03-02T03:00:00Z").some,
      execution = 120,
      discounts = List(qa(10)),
      corrections = List(
        correction(TimeChargeCorrection.Op.Add, 15),
        correction(TimeChargeCorrection.Op.Subtract, 5),
        correction(TimeChargeCorrection.Op.Add, 30, ChargeClass.NonCharged)
      ),
      charged = 120
    )
    val row = v.nightRow.get
    assertEquals(row.discountTime, minutes(10))
    assertEquals(row.correction, TimeCharges.Signed(TimeChargeCorrection.Op.Add, minutes(10)).some)

  test("rows are newest first with totals"):
    val older = visit(1,
                      interval("2026-03-02T01:00:00Z", "2026-03-02T02:00:00Z").some,
                      execution = 60,
                      charged = 60
    )
    val newer = visit(
      2,
      interval("2026-03-03T01:00:00Z", "2026-03-03T03:00:00Z").some,
      execution = 120,
      discounts = List(qa(10)),
      corrections = List(correction(TimeChargeCorrection.Op.Subtract, 20)),
      charged = 90
    )
    TimeCharges.fromVisits(List(older, newer)) match
      case TimeCharges.Rows(rows) =>
        assertEquals(rows.map(_.visitId), NonEmptyList.of(newer.visitId, older.visitId))
        val total = TimeCharges.Total.of(rows)
        assertEquals(total.nightTime, minutes(180))
        assertEquals(total.discountTime, minutes(10))
        assertEquals(total.correction,
                     TimeCharges.Signed(TimeChargeCorrection.Op.Subtract, minutes(20)).some
        )
        assertEquals(total.charged, minutes(150))
      case other                  => fail(s"Expected rows, got $other")

  test("decodes a visit with its invoice"):
    val json     =
      """
      {
        "id": "v-1",
        "site": "GS",
        "interval": { "start": "2026-03-01 23:35:00", "end": "2026-03-02 01:35:00" },
        "timeChargeInvoice": {
          "executionTime": {
            "program": { "microseconds": 7200000000 },
            "nonCharged": { "microseconds": 0 }
          },
          "discounts": [
            {
              "__typename": "TimeChargeDaylightDiscount",
              "interval": { "start": "2026-03-01 23:35:00", "end": "2026-03-01 23:50:00" },
              "amount": { "microseconds": 900000000 },
              "comment": "Observation executed during the day."
            }
          ],
          "corrections": [
            {
              "chargeClass": "PROGRAM",
              "op": "SUBTRACT",
              "amount": { "microseconds": 300000000 },
              "comment": null
            }
          ],
          "finalCharge": {
            "program": { "microseconds": 6000000000 },
            "nonCharged": { "microseconds": 1200000000 }
          }
        }
      }
      """
    val expected = visit(
      1,
      interval("2026-03-01T23:35:00Z", "2026-03-02T01:35:00Z").some,
      execution = 120,
      discounts = List(
        daylight("2026-03-01T23:35:00Z", "2026-03-01T23:50:00Z", 15)
          .copy(comment = "Observation executed during the day.".some)
      ),
      corrections = List(correction(TimeChargeCorrection.Op.Subtract, 5)),
      charged = 100
    ).copy(finalCharge =
      CategorizedTime(ChargeClass.Program -> minutes(100), ChargeClass.NonCharged -> minutes(20))
    )
    assertEquals(decode[VisitTimeCharge](json), expected.asRight)

  test("visits wholly in daylight from the ODB have no rows"):
    val json =
      """
      [
        {
          "id": "v-13e2",
          "site": "GN",
          "interval": {
            "start": "2026-07-05T02:28:18.110493Z",
            "end": "2026-07-05T02:33:50.47894Z"
          },
          "timeChargeInvoice": {
            "executionTime": {
              "program": { "microseconds": 332368447 },
              "nonCharged": { "microseconds": 0 }
            },
            "discounts": [
              {
                "__typename": "TimeChargeDaylightDiscount",
                "interval": {
                  "start": "2026-07-05T02:28:18.110493Z",
                  "end": "2026-07-05T02:33:50.47894Z"
                },
                "amount": { "microseconds": 332368447 },
                "comment": "Time spent observing pre-dusk (nautical twilight)."
              }
            ],
            "corrections": [],
            "finalCharge": {
              "program": { "microseconds": 0 },
              "nonCharged": { "microseconds": 0 }
            }
          }
        }
      ]
      """
    assertEquals(
      decode[List[VisitTimeCharge]](json).map(TimeCharges.fromVisits),
      TimeCharges.NoNightVisits.asRight
    )
