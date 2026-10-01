// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.TimeChargeRow
import explore.model.TimeCharges
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.time.format.GppDateFormatter
import lucuma.core.util.time.format.GppTimeTZFormatter
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.table.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.table.*

import scala.concurrent.duration.*

case class TimeChargesTable(obsId: Observation.Id) extends ReactFnProps(TimeChargesTable.component)

object TimeChargesTable:
  private given Reusability[TimeCharges] = Reusability.byEq

  private val ColDef = ColumnDef[TimeChargeRow].WithTableMeta[Option[TimeCharges.Total]]

  private def footerTotal(f: TimeCharges.Total => VdomNode)(
    c: HeaderContext[TimeChargeRow, ?, Option[TimeCharges.Total], ?, ?, ?, ?]
  ): VdomNode =
    c.table.options.meta.flatten.fold(EmptyVdom)(f)

  private def duration(t: TimeSpan): VdomNode =
    TimeSpanView(t, TimeSpanFormatter.HoursMinutesLetter)

  private def signed(s: Option[TimeCharges.Signed]): VdomNode =
    s.fold[VdomNode]("-"): s =>
      val sign = s.op match
        case TimeChargeCorrection.Op.Add      => "+"
        case TimeChargeCorrection.Op.Subtract => "−"
      <.span(sign, duration(s.amount))

  private def details(lines: List[String]): Option[VdomNode] =
    lines.toNel.map(ls => <.div(ls.toList.toTagMod(using l => <.div(l))))

  private def withDetails(node: VdomNode, lines: List[String]): VdomNode =
    details(lines).fold(node)(d => <.span(node).withTooltip(d))

  private val Columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        ColDef(
          ColumnId("night"),
          _.night,
          "Night",
          _.value.fold("-")(GppDateFormatter.format),
          footer = _ => "Total"
        ),
        ColDef(
          ColumnId("start"),
          _.interval,
          "Start (UTC)",
          _.value.fold("-")(i => GppTimeTZFormatter.format(i.start.toInstant))
        ),
        ColDef(
          ColumnId("end"),
          _.interval,
          "End (UTC)",
          _.value.fold("-")(i => GppTimeTZFormatter.format(i.end.toInstant))
        ),
        ColDef(
          ColumnId("night-time"),
          _.nightTime,
          "Night Time",
          c => duration(c.value),
          footer = footerTotal(t => duration(t.nightTime))
        ),
        ColDef(
          ColumnId("discounts"),
          identity,
          "Discounts",
          c =>
            withDetails(
              duration(c.value.discountTime),
              c.value.discounts.map: d =>
                s"${d.kind.label}: ${TimeSpanFormatter.HoursMinutesLetter.format(d.amount)}" +
                  d.comment.foldMap(m => s" ($m)")
            ),
          footer = footerTotal(t => duration(t.discountTime))
        ),
        ColDef(
          ColumnId("corrections"),
          identity,
          "Corrections",
          c => withDetails(signed(c.value.correction), c.value.corrections.flatMap(_.comment)),
          footer = footerTotal(t => signed(t.correction))
        ),
        ColDef(
          ColumnId("charged"),
          _.charged,
          "Charged",
          c => duration(c.value),
          footer = footerTotal(t => duration(t.charged))
        )
      )

  private val component = ScalaFnComponent[TimeChargesTable]: props =>
    for
      ctx     <- useContext(AppContext.ctx)
      visits  <- useEffectKeepResultOnMount(ctx.odbApi.observationTimeCharges(props.obsId))
      refresh <- useThrottledCallback(5.seconds)(visits.refresh.value.to[IO])
      _       <-
        useEffectStreamResourceOnMount:
          ctx.odbApi.stepEventSubscription(props.obsId).map(_.evalMap(_ => refresh.to[IO]))
      _       <-
        useEffectStreamResourceOnMount:
          ctx.odbApi.datasetEventSubscription(props.obsId).map(_.evalMap(_ => refresh.to[IO]))
      charges <-
        useMemo(visits.state.value.toOption.map(v => TimeCharges.fromVisits(v.get)))(identity)
      rows    <- useMemo(charges.value):
                   case Some(TimeCharges.Rows(rows)) => rows.toList
                   case _                            => Nil
      table   <- useReactTable:
                   TableOptions(
                     Columns,
                     rows,
                     getRowId = (row, _, _) => RowId(row.visitId.toString),
                     meta = charges.value.collect:
                       case TimeCharges.Rows(rows) => TimeCharges.Total.of(rows)
                     ,
                     enableSorting = false,
                     enableColumnResizing = false
                   )
    yield
      val body: VdomNode =
        charges.value match
          case None if visits.state.value.isError =>
            <.div("Could not load time charges")
          case None                               => EmptyVdom
          case Some(TimeCharges.NoVisits)         => <.div("No visits yet")
          case Some(TimeCharges.NoNightVisits)    => <.div("No night-time visits")
          case Some(TimeCharges.Rows(_))          =>
            PrimeTable(table, tableMod = ExploreStyles.TimeChargesTable)

      <.div(ExploreStyles.TimeChargesSection)(
        <.div(ExploreStyles.ObservationDetailsSection)("Time Charges"),
        body
      )
