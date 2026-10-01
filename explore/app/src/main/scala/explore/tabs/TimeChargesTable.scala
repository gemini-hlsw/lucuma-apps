// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.TimeChargeLine
import explore.model.TimeCharges
import explore.model.TimeCharges.*
import explore.model.VisitTimeCharge
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Divider
import lucuma.react.table.*
import lucuma.ui.table.*

import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

case class TimeChargesTable(visits: Option[List[VisitTimeCharge]])
    extends ReactFnProps(TimeChargesTable.component)

object TimeChargesTable:
  private given Reusability[List[VisitTimeCharge]] = Reusability.byEq
  private given Reusability[TimeCharges]           = Reusability.byEq

  private val ColDef = ColumnDef[TimeChargeLine].WithTableMeta[Option[TimeSpan]]

  private val VisitColId: ColumnId    = ColumnId("visit")
  private val StartColId: ColumnId    = ColumnId("start")
  private val EndColId: ColumnId      = ColumnId("end")
  private val DurationColId: ColumnId = ColumnId("duration")

  private val TimestampFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MMM-dd HH:mm:ss 'UTC'").withZone(ZoneOffset.UTC)

  private val DateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MMM-dd").withZone(ZoneOffset.UTC)

  private def timestamp(t: Timestamp): String = TimestampFormatter.format(t.toInstant)

  private def duration(t: TimeSpan): String =
    val secs = t.toSeconds.toLong
    f"${secs / 3600}h ${secs % 3600 / 60}%02dm ${secs % 60}%02ds"

  private def correctionText(c: VisitTimeCharge.Correction): String =
    val source = (DateFormatter.format(c.created.toInstant) :: c.user.map(_.show).toList)
      .mkString(", ")
    s"${c.comment.getOrElse("Time correction")} ($source)"

  private def correctionAmount(c: VisitTimeCharge.Correction): String =
    c.op match
      case TimeChargeCorrection.Op.Add      => duration(c.amount)
      case TimeChargeCorrection.Op.Subtract => s"-${duration(c.amount)}"

  private val Columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        ColDef(VisitColId, _.visitId, "Visit", _.value.show),
        ColDef(
          StartColId,
          identity,
          "Start",
          _.value match
            case TimeChargeLine.ForVisit(_, interval, _) =>
              interval.fold("-")(i => timestamp(i.start))
            case TimeChargeLine.ForCorrection(_, _, c)   => correctionText(c)
        ),
        ColDef(
          EndColId,
          identity,
          "End",
          _.value match
            case TimeChargeLine.ForVisit(_, interval, _) =>
              interval.fold("-")(i => timestamp(i.end))
            case TimeChargeLine.ForCorrection(_, _, _)   => EmptyVdom
          ,
          footer = _ => "Total"
        ),
        ColDef(
          DurationColId,
          identity,
          "Duration",
          _.value match
            case TimeChargeLine.ForVisit(_, _, d)      => duration(d)
            case TimeChargeLine.ForCorrection(_, _, c) => correctionAmount(c)
          ,
          footer = _.table.options.meta.flatten.fold(EmptyVdom)(t => duration(t))
        )
      )

  private def lineId(line: TimeChargeLine): RowId =
    line match
      case TimeChargeLine.ForVisit(v, _, _)      => RowId(v.show)
      case TimeChargeLine.ForCorrection(v, i, _) => RowId(s"${v.show}-correction-$i")

  private val component = ScalaFnComponent[TimeChargesTable]: props =>
    for
      charges <- useMemo(props.visits)(_.map(TimeCharges.fromVisits))
      lines   <- useMemo(charges.value):
                   case Some(TimeCharges.Rows(rows)) => rows.lines
                   case _                            => Nil
      table   <- useReactTable:
                   TableOptions(
                     Columns,
                     lines,
                     getRowId = (line, _, _) => lineId(line),
                     meta = charges.value.collect:
                       case TimeCharges.Rows(rows) => rows.total
                     ,
                     enableSorting = false,
                     enableColumnResizing = false
                   )
    yield
      val body: VdomNode =
        charges.value match
          case None                            => EmptyVdom
          case Some(TimeCharges.NoVisits)      => <.div("No visits yet")
          case Some(TimeCharges.NoNightVisits) => <.div("No night-time visits")
          case Some(TimeCharges.Rows(_))       =>
            PrimeTable(
              table,
              tableMod = ExploreStyles.TimeChargesTable,
              // A correction's description runs across the start and end columns.
              cellMod = (cell, _, render) =>
                cell.row.original match
                  case TimeChargeLine.ForCorrection(_, _, _)
                      if cell.column.id.value === StartColId.value =>
                    render(^.colSpan := 2)
                  case TimeChargeLine.ForCorrection(_, _, _)
                      if cell.column.id.value === EndColId.value =>
                    EmptyVdom
                  case _ =>
                    render
            )

      <.div(ExploreStyles.TimeChargesSection)(
        Divider(),
        <.div(ExploreStyles.ObservationDetailsSection)("Time Charges"),
        body
      )
