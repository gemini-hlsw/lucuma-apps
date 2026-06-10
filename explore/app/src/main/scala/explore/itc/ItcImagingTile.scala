// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.Order
import cats.Order.given
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
import explore.modes.ItcInstrumentConfig.Flamingos2Imaging
import explore.modes.ItcInstrumentConfig.GmosNorthImaging
import explore.modes.ItcInstrumentConfig.GmosSouthImaging
import explore.modes.ModeRow
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.ItcCcd
import lucuma.itc.SignalToNoiseAt
import lucuma.react.SizePx
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.format.*
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

      given Reusability[EitherNec[ItcQueryProblem, ImagingResults]] = Reusability.byEq

      // We only have imaging configs here, and all of them will be the same type, so this is
      // sufficient for our purposes
      given Order[ItcInstrumentConfig] = Order.from:
        case (GmosNorthImaging(filter = filter1), GmosNorthImaging(filter = filter2))   =>
          filter1.compare(filter2)
        case (GmosSouthImaging(filter = filter1), GmosSouthImaging(filter = filter2))   =>
          filter1.compare(filter2)
        case (Flamingos2Imaging(filter = filter1), Flamingos2Imaging(filter = filter2)) =>
          filter1.compare(filter2)
        case (_, _)                                                                     => 0

      def toMessage(msg: String, severity: Message.Severity) =
        Message(text = msg, severity = severity): VdomNode

      case class ImagingFilterRow(
        id:               Int,
        instrumentConfig: ItcInstrumentConfig,
        result:           Pot[EitherNec[ItcTargetProblem, ItcResult]]
      ) extends ModeRow
          with TableRowWithResult derives Eq {
        val config  = instrumentConfig
        def enabled = true

        private def withResult[A](
          f: (TimeSpan, PosInt, Option[SignalToNoiseAt]) => A
        ): Option[A] =
          result.toOption.collect { case Right(r @ ItcResult.Result(_, _)) =>
            f(r.exposureTime, r.exposures, r.snAt)
          }

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

      // Number of detectors for the S/N columns
      def detectorCount(results: Pot[EitherNec[ItcQueryProblem, ImagingResults]]): Int =
        results.toOption
          .flatMap(_.toOption)
          .map:
            _.values
              .flatMap(_.values)
              .flatMap(_.toOption)
              .collect:
                case r @ ItcResult.Result(_, _) => r.ccdCount
          .flatMap(_.maxOption)
          .getOrElse(1)

      def columnsFor(numDetectors: Int) =
        def ccdValue[V](idx: Int, get: ItcCcd => V): ImagingFilterRow => Option[V] =
          _.result.toOption
            .flatMap(_.toOption)
            .flatMap(ItcResult.result.getOption)
            .flatMap(_.ccds.get(idx))
            .map(get)

        // Single ccd column
        def ccdColumn[V](
          id:       String,
          header:   String,
          accessor: ItcCcd => V,
          format:   V => VdomNode,
          size:     SizePx
        ): ColumnDef.Single.WithTableMeta[ImagingFilterRow, Option[V], TableMeta] =
          ColDef(
            ColumnId(id),
            accessor = ccdValue(0, accessor),
            cell = _.value.map(format),
            header = header,
            size = size
          )

        // per-CCD columns
        def ccdColumns[V](
          idBase: String,
          header: String,
          get:    ItcCcd => V,
          format: V => VdomNode,
          size:   SizePx
        ): ColumnDef.Group.WithTableMeta[ImagingFilterRow, TableMeta] =
          ColumnDef.Group(
            id = ColumnId(idBase),
            header = _ => <.div(header, ExploreStyles.ItcImagingTableGroupHeader),
            columns = (0 until numDetectors).map { ccdIndex =>
              ColDef(
                ColumnId(s"${idBase}-$ccdIndex"),
                accessor = ccdValue(ccdIndex, get),
                cell = _.value.map(format),
                header = s"CCD ${ccdIndex + 1}",
                size = size
              )
            }.toList
          )

        def snColumns[V](
          id:       String,
          header:   String,
          get:      ItcCcd => V,
          format:   V => VdomNode,
          ccdSize:  SizePx,
          flatSize: SizePx
        ) =
          if numDetectors <= 1 then ccdColumn(id, header, get, format, flatSize)
          else ccdColumns(id, header, get, format, ccdSize)

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
          snColumns("sn+bg-ccd",
                    "S/N + Background",
                    _.peakPixelFlux,
                    v => f"$v%.0fe-",
                    85.toPx,
                    110.toPx
          ),
          snColumns("single-sn-ccd",
                    "S/N per Exposure",
                    _.singleSNRatio.value,
                    _.format,
                    85.toPx,
                    110.toPx
          ),
          snColumns("total-sn-ccd", "Total S/N", _.totalSNRatio.value, _.format, 85.toPx, 90.toPx)
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
        // Initialize selected target if none is set, and update if no longer in list
        _             <-
          useEffectWithDeps((props.selectedTarget.get, tileState.get.imagingTargets)):
            (selectedTarget, availableTargets) =>
              selectedTarget match
                case Some(itcTarget) =>
                  props.selectedTarget
                    .set(tileState.get.imagingDefaultTarget)
                    .unless_(availableTargets.contains(itcTarget))
                case _               =>
                  props.selectedTarget.set(tileState.get.imagingDefaultTarget)
        rowsOrMsg     <- useMemo(
                           (tileState.get.calculationResults, props.selectedTarget.get)
                         ): (results, oTarget) =>
                           results match
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
                                   oTarget
                                     .flatMap(result.get(_))
                                     .map: imagingResultsMap =>
                                       imagingResultsMap.toList
                                         .sortBy((params, _) => params.mode)
                                         .map: (params, r) =>
                                           ImagingFilterRow(params.hashCode, params.mode, r.ready)
                                     .orEmpty
                                     .asRight
        cols          <- useMemo(detectorCount(tileState.get.calculationResults)): numDetectors =>
                           columnsFor(numDetectors)
        table         <- useReactTable(
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

        val options: List[SelectItem[ItcTarget]] =
          tileState.get.imagingTargets
            .map(t => SelectItem(label = t.name.value, value = t))

        val title: VdomNode =
          props.selectedTarget.get.map: (itcTarget: ItcTarget) =>
            <.div(
              ExploreStyles.ItcTileTitle,
              <.label(s"Target:"),
              Dropdown(
                clazz = ExploreStyles.ItcTileTargetSelector,
                value = itcTarget,
                onChange = (t: ItcTarget) => props.selectedTarget.set(t.some),
                options = options
              ).when(options.length > 1),
              <.span(itcTarget.name.value)
                .when(options.length === 1)
            )

        val body: VdomNode =
          rowsOrMsg.swap.getOrElse(PrimeTable(table, compact = Compact.Very))

        TileContents(title, body)
    })
