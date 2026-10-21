// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.TimeChargeColumn
import explore.model.TimeChargeLine
import explore.model.TimeCharges
import explore.model.TimeCharges.*
import explore.model.VisitTimeCharge
import explore.model.enums.TableId
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.time.format.GppDateFormatter
import lucuma.core.util.time.format.UtcFormatter
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Divider
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.react.table.*
import lucuma.ui.format.DurationSpacedPaddedFormatter
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import java.time.ZoneOffset

case class TimeChargesTable(
  userId:     Option[User.Id],
  visits:     Option[List[VisitTimeCharge]],
  refreshing: Boolean,
  refresh:    Callback,
  onCorrect:  Option[Visit.Id => Callback]
) extends ReactFnProps(TimeChargesTable.component)

object TimeChargesTable:
  private given Reusability[List[VisitTimeCharge]] = Reusability.byEq
  private given Reusability[TimeCharges]           = Reusability.byEq

  private case class Meta(total: Option[TimeSpan], onCorrect: Option[Visit.Id => Callback])

  private val ColDef = ColumnDef[TimeChargeLine].WithTableMeta[Meta]

  private val VisitColId: ColumnId    = ColumnId("visit")
  private val StartColId: ColumnId    = ColumnId("start")
  private val EndColId: ColumnId      = ColumnId("end")
  private val DurationColId: ColumnId = ColumnId("duration")

  private def timestamp(t: Timestamp): String = UtcFormatter.format(t.toInstant)

  // Padded, as the durations stack in a right-aligned column with a total.
  private def duration(t: TimeSpan): String = DurationSpacedPaddedFormatter(t.toDuration)

  private def correctionText(c: VisitTimeCharge.Correction): String =
    val date   = GppDateFormatter.format(c.created.toInstant.atZone(ZoneOffset.UTC).toLocalDate)
    val source = (date :: c.user.map(_.show).toList)
      .mkString(", ")
    s"${c.comment.getOrElse("Time correction")} ($source)"

  private def correctionAmount(c: VisitTimeCharge.Correction): String =
    c.op match
      case TimeChargeCorrection.Op.Add      => duration(c.amount)
      case TimeChargeCorrection.Op.Subtract => s"-${duration(c.amount)}"

  // Every duration gets the same trailing slot so the amounts stay aligned with the buttons.
  private def actionSlot(meta: Option[Meta], button: Option[VdomNode] = none): VdomNode =
    meta
      .flatMap(_.onCorrect)
      .fold(EmptyVdom)(_ => <.span(ExploreStyles.TimeChargesAction)(button))

  private def correctButton(meta: Option[Meta], visitId: Visit.Id): Option[VdomNode] =
    meta
      .flatMap(_.onCorrect)
      .map: onCorrect =>
        Button(
          icon = Icons.ClockRotateLeftLight,
          severity = Button.Severity.Secondary,
          tooltip = "Add a time correction for this visit",
          tooltipOptions = TooltipOptions(position = Tooltip.Position.Left),
          onClick = onCorrect(visitId)
        ).mini.compact

  private val Columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        ColDef(VisitColId, _.visitId, "Visit", _.value.show, enableSorting = true),
        ColDef(
          StartColId,
          identity,
          "Start (UTC)",
          _.value match
            case TimeChargeLine.ForVisit(_, interval, _) =>
              interval.fold("-")(i => timestamp(i.start))
            case TimeChargeLine.ForCorrection(_, _, c)   =>
              <.span(Icons.ClockRotateLeftLight, correctionText(c))
          ,
          enableSorting = true
        ),
        ColDef(
          EndColId,
          identity,
          "End (UTC)",
          _.value match
            case TimeChargeLine.ForVisit(_, interval, _) =>
              interval.fold("-")(i => timestamp(i.end))
            case TimeChargeLine.ForCorrection(_, _, _)   => EmptyVdom
          ,
          footer = _ => "Total",
          enableSorting = true
        ),
        ColDef(
          DurationColId,
          identity,
          "Duration",
          cell =>
            val meta = cell.table.options.meta
            cell.value match
              case TimeChargeLine.ForVisit(v, _, d)      =>
                React.Fragment(duration(d), actionSlot(meta, correctButton(meta, v)))
              case TimeChargeLine.ForCorrection(_, _, c) =>
                React.Fragment(correctionAmount(c), actionSlot(meta))
          ,
          footer = cell =>
            val meta = cell.table.options.meta
            meta
              .flatMap(_.total)
              .fold(EmptyVdom)(t => React.Fragment(duration(t), actionSlot(meta)))
          ,
          enableSorting = true
        )
      )

  private val SortColumns: Map[String, TimeChargeColumn] =
    Map(
      VisitColId.value    -> TimeChargeColumn.Visit,
      StartColId.value    -> TimeChargeColumn.Start,
      EndColId.value      -> TimeChargeColumn.End,
      DurationColId.value -> TimeChargeColumn.Duration
    )

  private val DefaultSorting: Sorting = Sorting(StartColId -> SortDirection.Descending)

  // The visits are ordered here rather than by the table, so a visit's corrections stay with it.
  // The table still owns the sort state, which the state store saves and restores. Clearing the
  // sort falls back to newest first.
  private def sortOf(sorting: Sorting): (TimeChargeColumn, Boolean) =
    sorting.value.headOption
      .flatMap(s => SortColumns.get(s.columnId.value).map(_ -> s.direction.toDescending))
      .getOrElse((TimeChargeColumn.Start, true))

  private def lineId(line: TimeChargeLine): RowId =
    line match
      case TimeChargeLine.ForVisit(v, _, _)      => RowId(v.show)
      case TimeChargeLine.ForCorrection(v, i, _) => RowId(s"${v.show}-correction-$i")

  private val component = ScalaFnComponent[TimeChargesTable]: props =>
    for
      ctx        <- useContext(AppContext.ctx)
      charges    <- useMemo(props.visits)(_.map(TimeCharges.fromVisits))
      sorting    <- useState(DefaultSorting)
      lines      <- useMemo((charges.value, sorting.value)):
                      case (Some(TimeCharges.Rows(rows)), s) =>
                        val (column, descending) = sortOf(s)
                        rows.lines(column, descending)
                      case _                                 => Nil
      tableState <- useMemo(sorting.value)(s => PartialTableState(sorting = s))
      table      <- useReactTableWithStateStore:
                      import ctx.given

                      TableOptionsWithStateStore(
                        TableOptions(
                          Columns,
                          lines,
                          getRowId = (line, _, _) => lineId(line),
                          meta = Meta(
                            charges.value.collect:
                              case TimeCharges.Rows(rows) => rows.total
                            ,
                            props.onCorrect
                          ),
                          enableSorting = true,
                          manualSorting = true,
                          state = tableState,
                          onSortingChange = (updater: Updater[Sorting]) =>
                            sorting.modState: current =>
                              updater match
                                case Updater.Set(value) => value
                                case Updater.Mod(fn)    => fn(current)
                          ,
                          enableColumnResizing = false
                        ),
                        TableStore(props.userId, TableId.TimeCharges)
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
              striped = true,
              compact = Compact.Very,
              hoverableRows = false,
              tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.TimeChargesTable,
              rowMod = rowTagMod: row =>
                row.original match
                  case TimeChargeLine.ForCorrection(_, _, _) =>
                    ExploreStyles.TimeChargeCorrectionRow
                  case _                                     => TagMod.empty
              ,
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
        <.div(ExploreStyles.ObservationDetailsSection)(
          "Time Charges",
          Button(
            severity = Button.Severity.Secondary,
            icon = Icons.ArrowsRotate,
            tooltip = "Refresh the time charges",
            loading = props.refreshing,
            disabled = props.refreshing,
            rounded = true,
            onClick = props.refresh
          ).mini.compact
        ),
        body
      )
