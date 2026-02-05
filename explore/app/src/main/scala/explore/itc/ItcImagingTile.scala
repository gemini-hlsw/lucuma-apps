// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import crystal.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.syntax.pot.given
import eu.timepit.refined.types.numeric.PosInt
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ModesTableCommon
import explore.model.AppContext
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.Progress
import explore.model.TargetList
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ModeRow
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.SignalToNoiseAt
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

final case class ItcImagingTile(
  uid:                 Option[User.Id],
  selectedConfigs:     ConfigSelection,
  observation:         Observation,
  obsTargets:          TargetList,
  customSedTimestamps: List[Timestamp],
  selectedTarget:      View[Option[ItcTarget]]
) extends Tile[ItcImagingTile](
      ObsTabTileIds.ItcId.id,
      "ITC",
      bodyClass = ExploreStyles.ItcImagingTileBody
    )(ItcImagingTile)

object ItcImagingTile
    extends TileComponent[ItcImagingTile]({ (props, _) =>
      import ModesTableCommon.*

      given Reusability[ImagingTargetAndResults] = Reusability.byEq

      given Reusability[ImagingResults] = Reusability.byEq

      def toMessage(msg: String, severity: Message.Severity) =
        Message(text = msg, severity = severity): VdomNode

      case class ImagingFilterRow(
        id:         Int,
        instrument: ItcInstrumentConfig,
        result:     Pot[EitherNec[ItcTargetProblem, ItcResult]]
      ) extends ModeRow
          with TableRowWithResult derives Eq {
        val config  = instrument
        def enabled = true

        private def withResult[A](
          f: (TimeSpan, PosInt, Option[SignalToNoiseAt]) => A
        ): Option[A] =
          result.toOption.collect { case Right(ItcResult.Result(e, t, _, s, _)) => f(e, t, s) }

        val singleSN: Option[SingleSN] =
          withResult((_, _, s) => s.map(_.single)).flatten

        override lazy val totalSN: Option[TotalSN] =
          withResult((_, _, s) => s.map(_.total)).flatten

        val exposureTime: Option[TimeSpan] =
          withResult((e, _, _) => e)

        val exposureCount: Option[PosInt] =
          withResult((_, t, _) => t)
      }

      val ColDef = ColumnDef[ImagingFilterRow].WithTableMeta[TableMeta]

      val FilterColId     = ColumnId("filter")
      val InstrumentColId = ColumnId("instrument")
      val TotalSNColId    = ColumnId("totalsn")
      val ExpTimeColId    = ColumnId("exptime")
      val ExposuresColId  = ColumnId("exposures")

      val columnNames: Map[ColumnId, String] =
        Map(
          InstrumentColId -> "Instrument",
          ExpTimeColId    -> "Time",
          TotalSNColId    -> "S/N",
          FilterColId     -> "Filter",
          ExposuresColId  -> "Exposures"
        )

      def column[V](
        id:       ColumnId,
        accessor: ImagingFilterRow => V
      ): ColumnDef.Single.WithTableMeta[ImagingFilterRow, V, TableMeta] =
        ColDef(id, accessor, columnNames.getOrElse(id, id.value))

      lazy val columns =
        List(
          column(InstrumentColId, _.config.instrument.shortName)
            .withCell(_.value: String)
            .withSize(120.toPx),
          column(FilterColId, _.config.filterStr)
            .withCell(_.value: String)
            .withSize(69.toPx)
            .sortable,
          column(ExposuresColId, _.result)
            .withHeader(progressingCellHeader("Exposures"))
            .withCell: cell =>
              itcCell(cell.value, ItcColumns.Exposures)
            .withSize(80.toPx),
          column(ExpTimeColId, _.result)
            .withHeader(progressingCellHeader("Time"))
            .withCell: cell =>
              itcCell(cell.value, ItcColumns.Time)
            .withSize(85.toPx),
          column(TotalSNColId, _.result)
            .withHeader(progressingCellHeader("S/N"))
            .withCell: cell =>
              itcCell(cell.value, ItcColumns.SN)
            .withSize(85.toPx)
        )

      for {
        ctx           <- useContext(AppContext.ctx)
        tileState     <- useStateView(ItcTileState.Empty)
        imagingQuerier =
          ItcImagingQuerier(
            props.observation,
            props.selectedConfigs.configs.map(_.instrumentConfig),
            props.obsTargets,
            props.customSedTimestamps
          )
        // Update calculationResults for the selected configs
        _             <- useEffectWithDeps(imagingQuerier): querier =>
                           import ctx.given

                           tileState
                             .zoom(ItcTileState.calculationResults)
                             .set(Pot.pending)
                             .toAsync >>
                             querier.requestCalculations
                               .flatMap: result =>
                                 tileState
                                   .zoom(ItcTileState.calculationResults)
                                   .set(result.ready)
                                   .toAsync
        // Initialize selected target if none is set
        _             <-
          useEffectWithDeps((props.selectedTarget.get, tileState.get.imagingTargetResults)):
            (selectedTarget, availableTargets) =>
              selectedTarget match
                case None => props.selectedTarget.set(availableTargets.headOption.map(_.target))
                case _    => Callback.empty
        rowsOrMsg     <- useMemo(tileState.get.calculationResults):
                           _ match
                             case Pot.Pending      =>
                               toMessage("Waiting for ITC...", Message.Severity.Info)
                                 .asLeft[List[ImagingFilterRow]]
                             case Pot.Error(t)     =>
                               toMessage(s"Error calling ITC: ${t.getMessage}",
                                         Message.Severity.Error
                               ).asLeft
                             case Pot.Ready(value) =>
                               value match
                                 case Left(problems) =>
                                   toMessage(problems.toList.map(_.message).mkString(", "),
                                             Message.Severity.Warning
                                   ).asLeft
                                 case Right(result)  =>
                                   result.toList.zipWithIndex.map { case ((config, res), idx) =>
                                     ImagingFilterRow(idx, config.mode, res.ready)
                                   }.asRight

        cols  <- useMemo(()): _ =>
                   columns
        table <- useReactTable(
                   TableOptions(
                     cols,
                     rowsOrMsg.map(_.getOrElse(List.empty)),
                     getRowId = (row, _, _) => RowId(row.id.toString),
                     enableSorting = true,
                     enableColumnResizing = true,
                     meta = TableMeta(none[Progress])
                   )
                 )
      } yield
        val selectedTarget: Option[ImagingTargetAndResults] =
          props.selectedTarget.get.flatMap(tileState.get.selectedImagingTargetFor)

        val options: List[SelectItem[ImagingTargetAndResults]] =
          tileState.get.imagingTargetResults
            .map(t => SelectItem(label = t.target.name.value, value = t))

        // The title is always visible thus it is better for it to control processing
        val title: VdomNode =
          // Display target selector if we have targets and results
          selectedTarget.map: (gr: ImagingTargetAndResults) =>
            <.div(
              ExploreStyles.ItcTileTitle,
              <.label(s"Target:"),
              Dropdown(
                clazz = ExploreStyles.ItcTileTargetSelector,
                value = gr,
                onChange = (o: ImagingTargetAndResults) => props.selectedTarget.set(Some(o.target)),
                options = options
              ).when(options.length > 1),
              <.span(gr.target.name.value)
                .when(options.length === 1)
            )

        val body: VdomNode =
          rowsOrMsg.swap.getOrElse(PrimeTable(table, compact = Compact.Very))

        TileContents(title, body)
    })
