// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import explore.model.TimeCharges.*
import io.circe.parser.decode
import lucuma.core.enums.ChargeClass
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import munit.FunSuite

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
                             minutes(m)
    )

  private def qa(m: Int): VisitTimeCharge.Discount =
    VisitTimeCharge.Discount(
      VisitTimeCharge.DiscountKind.Qa,
      interval("2026-03-02T01:00:00Z", "2026-03-02T01:10:00Z"),
      minutes(m)
    )

  private def correction(
    op:          TimeChargeCorrection.Op,
    m:           Int,
    created:     String = "2026-03-05T12:00:00Z",
    chargeClass: ChargeClass = ChargeClass.Program
  ): VisitTimeCharge.Correction =
    VisitTimeCharge.Correction(ts(created), none, chargeClass, op, minutes(m), none)

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
    assertEquals(row.duration, minutes(120))
    assertEquals(row.interval, v.interval)
    assertEquals(row.charged, minutes(120))

  test("a visit starting before twilight is clipped and loses the daylight time"):
    val v   = visit(
      1,
      interval("2026-03-01T23:35:00Z", "2026-03-02T01:35:00Z").some,
      execution = 120,
      discounts = List(daylight("2026-03-01T23:35:00Z", "2026-03-01T23:50:00Z", 15)),
      charged = 105
    )
    val row = v.nightRow.get
    assertEquals(row.duration, minutes(105))
    assertEquals(row.interval, interval("2026-03-01T23:50:00Z", "2026-03-02T01:35:00Z").some)

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
    assertEquals(row.duration, minutes(560))
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
    assertEquals(v.nightRow.map(_.duration), TimeSpan.Zero.some)

  test("only daylight visits are reported as having no night-time visits"):
    val v = visit(
      1,
      interval("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z").some,
      execution = 60,
      discounts = List(daylight("2026-03-01T20:00:00Z", "2026-03-01T21:00:00Z", 60)),
      charged = 0
    )
    assertEquals(TimeCharges.fromVisits(List(v)), TimeCharges.NoNightVisits)

  test("other discounts come off the duration and only program corrections are kept"):
    val forProgram = correction(TimeChargeCorrection.Op.Add, 15)
    val v          = visit(
      1,
      interval("2026-03-02T01:00:00Z", "2026-03-02T03:00:00Z").some,
      execution = 120,
      discounts = List(qa(10)),
      corrections = List(
        forProgram,
        correction(TimeChargeCorrection.Op.Add, 30, chargeClass = ChargeClass.NonCharged)
      ),
      charged = 125
    )
    val row        = v.nightRow.get
    assertEquals(row.duration, minutes(110))
    assertEquals(row.corrections, List(forProgram))

  test("lines are newest visit first, each followed by its corrections, with the charged total"):
    val older  = visit(1,
                      interval("2026-03-02T01:00:00Z", "2026-03-02T02:00:00Z").some,
                      execution = 60,
                      charged = 60
    )
    val later  = correction(TimeChargeCorrection.Op.Subtract, 20, "2026-03-06T12:00:00Z")
    val sooner = correction(TimeChargeCorrection.Op.Add, 5, "2026-03-05T12:00:00Z")
    val newer  = visit(
      2,
      interval("2026-03-03T01:00:00Z", "2026-03-03T03:00:00Z").some,
      execution = 120,
      corrections = List(later, sooner),
      charged = 105
    )
    TimeCharges.fromVisits(List(older, newer)) match
      case TimeCharges.Rows(rows) =>
        assertEquals(
          rows.lines,
          List(
            TimeChargeLine.ForVisit(newer.visitId, newer.interval, minutes(120)),
            TimeChargeLine.ForCorrection(newer.visitId, 0, sooner),
            TimeChargeLine.ForCorrection(newer.visitId, 1, later),
            TimeChargeLine.ForVisit(older.visitId, older.interval, minutes(60))
          )
        )
        assertEquals(rows.total, minutes(165))
      case other                  => fail(s"Expected rows, got $other")

  test("decodes a visit with its invoice"):
    val json     =
      """
      {
        "id": "v-1",
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
              "amount": { "microseconds": 900000000 }
            }
          ],
          "corrections": [
            {
              "created": "2026-03-05 12:00:00",
              "user": { "id": "u-771" },
              "chargeClass": "PROGRAM",
              "op": "SUBTRACT",
              "amount": { "microseconds": 300000000 },
              "comment": "Weather"
            },
            {
              "created": "2026-03-05 12:00:00",
              "user": null,
              "chargeClass": "PROGRAM",
              "op": "ADD",
              "amount": { "microseconds": 300000000 },
              "comment": null
            }
          ],
          "finalCharge": {
            "program": { "microseconds": 6300000000 },
            "nonCharged": { "microseconds": 900000000 }
          }
        }
      }
      """
    val expected = visit(
      1,
      interval("2026-03-01T23:35:00Z", "2026-03-02T01:35:00Z").some,
      execution = 120,
      discounts = List(daylight("2026-03-01T23:35:00Z", "2026-03-01T23:50:00Z", 15)),
      corrections = List(
        correction(TimeChargeCorrection.Op.Subtract, 5)
          .copy(user = User.Id.fromLong(0x771).get.some, comment = "Weather".some),
        correction(TimeChargeCorrection.Op.Add, 5)
      ),
      charged = 105
    ).copy(finalCharge =
      CategorizedTime(ChargeClass.Program -> minutes(105), ChargeClass.NonCharged -> minutes(15))
    )
    assertEquals(decode[VisitTimeCharge](json), expected.asRight)

  test("visits wholly in daylight from the ODB have no rows"):
    val json =
      """
      [
        {
          "id": "v-13e2",
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
                "amount": { "microseconds": 332368447 }
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
