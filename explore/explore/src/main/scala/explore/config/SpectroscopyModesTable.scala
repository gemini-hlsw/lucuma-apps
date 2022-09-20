// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import boopickle.DefaultBasic.*
import cats.data.*
import cats.effect.*
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import coulomb.Quantity
import coulomb.policy.spire.standard.given
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ObsQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.implicits.*
import explore.itc.*
import explore.model.Progress
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.boopickle.*
import explore.model.itc.*
import explore.model.reusability.*
import explore.modes.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SiderealTracking
import lucuma.core.util.Display
import lucuma.refined.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.utils.*
import react.circularprogressbar.CircularProgressbar
import react.common.Css
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.semanticui.*
import react.semanticui.collections.table.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.label.Label
import react.virtuoso.*
import react.virtuoso.raw.ListRange
import reactST.reactTable.*
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.SortByFn
import reactST.reactTable.mod.UseTableRowProps
import spire.math.Bounded
import spire.math.Interval

import java.text.DecimalFormat
import java.util.UUID
import scala.concurrent.duration.*

import scalajs.js.|

case class SpectroscopyModesTable(
  scienceMode:              View[Option[ScienceMode]],
  spectroscopyRequirements: SpectroscopyRequirementsData,
  constraints:              ConstraintSet,
  targets:                  Option[List[ItcTarget]],
  baseCoordinates:          Option[Coordinates],
  matrix:                   SpectroscopyModesMatrix,
  onSelect:                 Callback
)(using val ctx:            AppContextIO)
    extends ReactFnProps(SpectroscopyModesTable.component) {
  val brightestTarget: Option[ItcTarget] =
    for
      w  <- spectroscopyRequirements.wavelength
      tg <- targets
      b  <- tg.brightestAt(w)
    yield b
}

private object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable

  type ColId = NonEmptyString

  given Reusability[ListRange] =
    Reusability.by(x => (x.startIndex.toInt, x.endIndex.toInt))

  extension (r: ListRange) def isDefined: Boolean = !(r.startIndex === 0 && r.endIndex === 0)

  given Reusability[EitherNec[ItcQueryProblems, ItcResult]] = Reusability.byEq

  given Reusability[SpectroscopyModesMatrix] = Reusability.by(_.matrix.length)

  given Reusability[ItcResultsCache] = Reusability.by(_.cache.size)

  given Reusability[Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]] =
    Reusability.never

  val ModesTableDef = TableDef[SpectroscopyModeRow].withSortBy.withBlockLayout

  val ModesTable = new SUITableVirtuoso(ModesTableDef)

  val decFormat = new DecimalFormat("0.###")

  val gratingDisplay: Display[ModeGrating] = Display.byShortName {
    case ModeGrating.NoGrating      => "-"
    case ModeGrating.SomeGrating(t) => t
  }

  def column[V](id: ColId, accessor: SpectroscopyModeRow => V) =
    ModesTableDef
      .Column(id, accessor)
      .setHeader(columnNames.getOrElse(id, id.value): String)

  val SelectedColumnId: ColId    = "selected".refined
  val InstrumentColumnId: ColId  = "instrument".refined
  val SlitWidthColumnId: ColId   = "slit_width".refined
  val SlitLengthColumnId: ColId  = "slit_length".refined
  val GratingColumnId: ColId     = "grating".refined
  val FilterColumnId: ColId      = "filter".refined
  val CoverageColumnId: ColId    = "coverage".refined
  val FPUColumnId: ColId         = "fpu".refined
  val ResolutionColumnId: ColId  = "resolution".refined
  val AvailablityColumnId: ColId = "availability".refined
  val TimeColumnId: ColId        = "time".refined

  private val columnNames: Map[ColId, String] =
    Map[NonEmptyString, String](
      InstrumentColumnId  -> "Instrument",
      SlitWidthColumnId   -> "Slit Width",
      SlitLengthColumnId  -> "Slit Length",
      GratingColumnId     -> "Grating",
      FilterColumnId      -> "Filter",
      FPUColumnId         -> "FPU",
      CoverageColumnId    -> "Coverage",
      ResolutionColumnId  -> "λ / Δλ",
      AvailablityColumnId -> "Avail.",
      TimeColumnId        -> "Time"
    )

  val formatSlitWidth: ModeSlitSize => String = ss =>
    decFormat.format(
      ModeSlitSize.milliarcseconds.get(ss.size).setScale(3, BigDecimal.RoundingMode.UP)
    )

  val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.size).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

  def formatGrating(grating: InstrumentRow#Grating): String = grating match
    case f: GmosSouthGrating => f.shortName
    case f: GmosNorthGrating => f.shortName
    case f: F2Disperser      => f.shortName
    case f: GpiDisperser     => f.shortName
    case f: GnirsDisperser   => f.shortName
    case r                   => r.toString

  def formatFilter(filter: InstrumentRow#Filter): String = filter match
    case Some(f: GmosSouthFilter) => f.shortName
    case Some(f: GmosNorthFilter) => f.shortName
    case f: F2Filter              => f.shortName
    case f: GnirsFilter           => f.shortName
    case _: None.type             => "none"
    case r                        => r.toString

  def formatWavelengthCoverage(r: Interval[Quantity[BigDecimal, Micrometer]]): String = r match
    case Bounded(a, b, _) =>
      List(a, b)
        .map(q => decFormat.format(q.value.setScale(3, BigDecimal.RoundingMode.DOWN)))
        .mkString(" - ")
    case _                =>
      "-"

  def formatInstrument(r: (Instrument, NonEmptyString)): String = r match
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName

  def formatFPU(r: FocalPlane): String = r match
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"

  def itcCell(c: EitherNec[ItcQueryProblems, ItcResult]): VdomElement =
    val content: TagMod = c match {
      case Left(nel)                        =>
        if (nel.exists(_ == ItcQueryProblems.UnsupportedMode))
          <.span(Icons.Ban.color("red"))
            .withTooltip(tooltip = "Mode not supported", placement = Placement.RightStart)
        else {
          val content = nel.collect {
            case ItcQueryProblems.MissingSignalToNoise => <.span("Set S/N")
            case ItcQueryProblems.MissingWavelength    => <.span("Set Wavelength")
            case ItcQueryProblems.MissingTargetInfo    => <.span("Missing target info")
            case ItcQueryProblems.GenericError(e)      => e.split("\\.").mkTagMod(<.br)
          }.toList

          <.span(Icons.TriangleSolid)
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
        }
      case Right(r: ItcResult.Result)       =>
        <.span(formatDuration(r.duration.toSeconds))
          .withTooltip(
            placement = Placement.RightStart,
            tooltip = s"${r.exposures} × ${formatDuration(r.exposureTime.toSeconds)}"
          )
      case Right(ItcResult.Pending)         =>
        Icons.Spinner.spin(true)
      case Right(ItcResult.SourceTooBright) =>
        <.span(Icons.SunBright.color("yellow"))
          .withTooltip(tooltip = "Source too bright")
    }
    <.div(ExploreStyles.ITCCell, content)

  def sortItcFun(
    itc:         ItcResultsCache,
    cw:          Option[Wavelength],
    sn:          Option[PosBigDecimal],
    constraints: ConstraintSet,
    target:      Option[ItcTarget]
  ): SortByFn[SpectroscopyModeRow] =
    (
      rowA: UseTableRowProps[SpectroscopyModeRow],
      rowB: UseTableRowProps[SpectroscopyModeRow],
      _:    String | String,
      desc: Boolean | Unit
    ) =>
      (itc.forRow(cw, sn, constraints, target, rowA.original),
       itc.forRow(cw, sn, constraints, target, rowB.original)
      ) match {
        case (Right(ItcResult.Result(e1, t1)), Right(ItcResult.Result(e2, t2))) =>
          (e1.toMillis * t1 - e2.toMillis * t2).toDouble
        case (Left(_), Right(ItcResult.Result(e1, t1)))                         =>
          e1.toMillis * t1.toDouble
        case (Right(ItcResult.Result(e1, t1)), Left(_))                         =>
          -e1.toMillis * t1.toDouble
        case _                                                                  =>
          (desc: Any) match {
            case true  => -Double.MaxValue
            case false => Double.MaxValue
            case _     => 0
          }
      }

  def columns(
    cw:          Option[Wavelength],
    fpu:         Option[FocalPlane],
    sn:          Option[PosBigDecimal],
    constraints: ConstraintSet,
    target:      Option[ItcTarget],
    itc:         ItcResultsCache,
    progress:    Option[Progress]
  ) =
    List(
      column(InstrumentColumnId, SpectroscopyModeRow.instrumentAndConfig.get)
        .setCell(c => formatInstrument(c.value))
        .setWidth(120)
        .setMinWidth(50)
        .setMaxWidth(150),
      column(TimeColumnId, itc.forRow(cw, sn, constraints, target, _))
        .setCell(c => itcCell(c.value))
        .setWidth(80)
        .setMinWidth(80)
        .setMaxWidth(80)
        .setHeader(_ =>
          <.div(ExploreStyles.ITCHeaderCell)(
            "Time",
            progress
              .map(p =>
                CircularProgressbar(p.percentage.value.value,
                                    strokeWidth = 15,
                                    className = "explore-modes-table-itc-circular-progressbar"
                )
              )
          )
        )
        .setSortType(sortItcFun(itc, cw, sn, constraints, target))
        .setDisableSortBy(progress.isDefined),
      column(SlitWidthColumnId, SpectroscopyModeRow.slitWidth.get)
        .setCell(c => formatSlitWidth(c.value))
        .setWidth(96)
        .setMinWidth(96)
        .setMaxWidth(96)
        .setSortType(DefaultSortTypes.number),
      column(SlitLengthColumnId, SpectroscopyModeRow.slitLength.get)
        .setCell(c => formatSlitLength(c.value))
        .setWidth(100)
        .setMinWidth(100)
        .setMaxWidth(100)
        .setSortType(DefaultSortTypes.number),
      column(GratingColumnId, SpectroscopyModeRow.grating.get)
        .setCell(c => formatGrating(c.value))
        .setWidth(95)
        .setMinWidth(95)
        .setMaxWidth(95),
      column(FilterColumnId, SpectroscopyModeRow.filter.get)
        .setCell(c => formatFilter(c.value))
        .setWidth(69)
        .setMinWidth(69)
        .setMaxWidth(69),
      column(FPUColumnId, SpectroscopyModeRow.fpu.get)
        .setCell(c => formatFPU(c.value))
        .setWidth(62)
        .setMinWidth(62)
        .setMaxWidth(62),
      column(CoverageColumnId, SpectroscopyModeRow.coverageInterval(cw))
        .setCell(c => formatWavelengthCoverage(c.value))
        .setWidth(100)
        .setMinWidth(100)
        .setMaxWidth(100)
        .setSortType(DefaultSortTypes.number),
      column(ResolutionColumnId, SpectroscopyModeRow.resolution.get)
        .setCell(c => c.value.toString)
        .setWidth(70)
        .setMinWidth(70)
        .setMaxWidth(70)
        .setSortType(DefaultSortTypes.number),
      column(AvailablityColumnId, rowToConf)
        .setCell(_.value.fold("No")(_ => "Yes"))
        .setWidth(66)
        .setMinWidth(66)
        .setMaxWidth(66)
        .setSortType(DefaultSortTypes.number)
    ).filter { case c => (c.id.toString) != FPUColumnId.value || fpu.isEmpty }

  extension (row: SpectroscopyModeRow)
    def rowToConf: Option[ScienceMode] =
      row.instrument match {
        case GmosNorthSpectroscopyRow(grating, fpu, filter)
            if row.focalPlane === FocalPlane.SingleSlit =>
          ScienceMode
            .GmosNorthLongSlit(
              basic = ScienceModeBasic.GmosNorthLongSlit(grating, filter, fpu),
              advanced = ScienceModeAdvanced.GmosNorthLongSlit.Empty
            )
            .some
        case GmosSouthSpectroscopyRow(grating, fpu, filter)
            if row.focalPlane === FocalPlane.SingleSlit =>
          ScienceMode
            .GmosSouthLongSlit(
              basic = ScienceModeBasic.GmosSouthLongSlit(grating, filter, fpu),
              advanced = ScienceModeAdvanced.GmosSouthLongSlit.Empty
            )
            .some
        case _ => none
      }

    def equalsConf(conf: ScienceMode): Boolean =
      rowToConf.exists(_ === conf)

    def enabledRow: Boolean =
      List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(row.instrument.instrument) &&
        row.focalPlane === FocalPlane.SingleSlit

  def selectedRowIndex(
    scienceMode: Option[ScienceMode],
    rows:        List[SpectroscopyModeRow]
  ): Option[Int] =
    scienceMode
      .map(selected => rows.indexWhere(_.equalsConf(selected)))
      .filterNot(_ === -1)

  def visibleRows(visibleRange: ListRange, rows: List[SpectroscopyModeRow]) = {
    val s = visibleRange.startIndex.toInt
    val e = visibleRange.endIndex.toInt

    (for { i <- s to e } yield rows.lift(i)).toList.flattenOption
  }

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // rows
      .useMemoBy(p => (p.matrix, p.spectroscopyRequirements, p.baseCoordinates.map(_.dec))) {
        _ => (matrix, s, dec) =>
          val rows                =
            matrix
              .filtered(
                focalPlane = s.focalPlane,
                capabilities = s.capabilities,
                wavelength = s.wavelength,
                slitWidth = s.focalPlaneAngle,
                resolution = s.resolution,
                coverage = s.wavelengthCoverage.flatMap(
                  _.micrometer.toValue[BigDecimal].toRefined[NonNegative].toOption
                ),
                declination = dec
              )
          val (enabled, disabled) = rows.partition(enabledRow)
          enabled ++ disabled
      }
      // itc results cache
      .useState(
        ItcResultsCache(Map.empty[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]])
      )
      // itcProgress
      .useState(none[Progress])
      .useMemoBy { (props, rows, itc, _) => // Calculate the common errors
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.signalToNoise,
         props.brightestTarget,
         props.constraints,
         rows,
         itc
        )
      } { (_, _, _, _) => (wavelength, sn, targets, constraints, rows, itc) =>
        rows.value
          .map(
            itc.value.forRow(wavelength, sn, constraints, targets, _)
          )
          .collect { case Left(p) =>
            p.toList.filter {
              case ItcQueryProblems.MissingTargetInfo => true
              case _                                  => false
            }.distinct
          }
          .flatten
          .toList
          .distinct
      }
      .useMemoBy { (props, _, itc, itcProgress, _) => // Memo Cols
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.signalToNoise,
         props.brightestTarget,
         props.constraints,
         itc,
         itcProgress
        )
      } { (_, _, _, _, _) => (wavelength, focalPlane, sn, targets, constraints, itc, itcProgress) =>
        columns(wavelength, focalPlane, sn, constraints, targets, itc.value, itcProgress.value)
      }
      // selectedIndex
      .useStateBy((props, rows, _, _, _, _) => selectedRowIndex(props.scienceMode.get, rows))
      // Recompute state if conf or requirements change.
      .useEffectWithDepsBy((props, _, _, _, _, _, _) =>
        (props.scienceMode.get, props.spectroscopyRequirements)
      ) { (_, rows, _, _, _, _, selectedIndex) => (scienceMode, _) =>
        selectedIndex.setState(selectedRowIndex(scienceMode, rows))
      }
      // tableInstance
      .useTableBy((_, rows, _, _, _, cols, _) => ModesTableDef(cols, rows))
      // virtuosoRef
      // This useMemo may be deceptive: it actually memoizes the ref, which is a wrapper to a mutable value.
      .useMemo(())(_ => ModesTable.createRef)
      // visibleRange
      .useState(none[ListRange])
      // atTop
      .useState(false)
      // Recalculate ITC values if the wv or sn change or if the rows get modified
      .useStreamResourceBy((props, _, _, _, _, _, _, _, _, range, _) =>
        (
          props.spectroscopyRequirements.wavelength,
          props.spectroscopyRequirements.signalToNoise,
          props.constraints,
          props.brightestTarget,
          range
        )
      ) {
        (props, _, itcResults, itcProgress, _, _, _, ti, _, range, _) =>
          (wavelength, signalToNoise, constraints, brightestTarget, range) =>
            import props.given

            val sortedRows = ti.value.preSortedRows.map(_.original).toList

            (wavelength, signalToNoise, brightestTarget)
              .mapN { (w, sn, t) =>
                // Discard modes already in the cache
                val modes =
                  (range.value.foldMap(visibleRows(_, sortedRows)) ++ sortedRows).distinct
                    .filterNot { row =>
                      val cache = itcResults.value.cache
                      val cw    = row.coverageCenter(w)
                      row.instrument match
                        case m: GmosNorthSpectroscopyRow =>
                          cw.exists(w => cache.contains(ItcRequestParams(w, sn, constraints, t, m)))
                        case m: GmosSouthSpectroscopyRow =>
                          cw.exists(w => cache.contains(ItcRequestParams(w, sn, constraints, t, m)))
                        case _                           => true
                    }

                if (modes.length === 1 || (modes.length > 1 && range.value.exists(_.isDefined))) {
                  val progressZero = Progress.initial(NonNegInt.unsafeFrom(modes.length)).some
                  (for
                    _       <- Resource.eval(itcProgress.setStateAsync(progressZero))
                    request <-
                      ItcClient[IO]
                        .request(ItcMessage.Query(w, sn, constraints, t, modes))
                        .map(
                          // Avoid intermediate rerenders. They are slow.
                          _.groupWithin(100, 500.millis).evalTap(itcResponseChunk =>
                            itcProgress
                              .modStateAsync(
                                _.map(_.increment(NonNegInt.unsafeFrom(itcResponseChunk.size)))
                                  .filterNot(_.complete)
                              ) >>
                              // Update the cache
                              itcResults.modStateAsync(_.updateN(itcResponseChunk.toList))
                          )
                        )
                  yield request).some
                } else none
              }
              .flatten
              .getOrElse(Resource.pure(fs2.Stream()))
      }
      .render {
        (
          props,
          rows,
          _,
          _,
          errs,
          _,
          selectedIndex,
          tableInstance,
          virtuosoRef,
          visibleRange,
          atTop,
          _
        ) =>
          def toggleRow(row: SpectroscopyModeRow): Option[ScienceMode] =
            rowToConf(row).filterNot(conf => props.scienceMode.get.contains_(conf))

          def scrollButton(
            content:        VdomNode,
            style:          Css,
            indexDiff:      Int => Int,
            indexCondition: Int => Boolean
          ): TagMod =
            selectedIndex.value.whenDefined(idx =>
              Button(
                compact = true,
                onClick = virtuosoRef.foreach(
                  _.raw.scrollIntoView(
                    ScrollIntoViewLocation(index = indexDiff(idx - 2),
                                           behavior = ScrollBehavior.Smooth
                    )
                  )
                )
              )(
                ExploreStyles.ScrollButton,
                style
              )(content).when(indexCondition(idx))
            )

          val errLabel: List[VdomNode] = errs.collect {
            case ItcQueryProblems.MissingWavelength    =>
              Label(clazz = ExploreStyles.WarningLabel, size = sizes.Small)("Set Wav..")
            case ItcQueryProblems.MissingSignalToNoise =>
              Label(clazz = ExploreStyles.WarningLabel, size = sizes.Small)("Set S/N")
            case ItcQueryProblems.MissingTargetInfo    =>
              Label(clazz = ExploreStyles.WarningLabel, size = sizes.Small)("Missing Target Info")
          }

          val selectedTarget =
            for
              w <- props.spectroscopyRequirements.wavelength
              t <- props.brightestTarget
              if props.targets.exists(_.length > 1)
              if errLabel.isEmpty
            yield Label(size = sizes.Small, clazz = ExploreStyles.ModesTableTarget)(
              s"on ${t.name.value}"
            ).some

          React.Fragment(
            <.div(ExploreStyles.ModesTableTitle)(
              <.label(
                ExploreStyles.ModesTableCount,
                s"${rows.length} matching configurations",
                HelpIcon("configuration/table.md".refined)
              ),
              <.div(
                ExploreStyles.ModesTableInfo,
                errLabel.toTagMod,
                selectedTarget
              )
            ),
            <.div(ExploreStyles.ExploreTable, ExploreStyles.ModesTable)(
              ModesTable
                .Component(
                  table = Table(
                    celled = true,
                    selectable = true,
                    striped = true,
                    compact = TableCompact.Very,
                    clazz = ExploreStyles.Virtualized
                  )(),
                  header = true,
                  headerCell = (c: ModesTableDef.ColumnType) =>
                    TableHeaderCell(clazz =
                      ExploreStyles.StickyColumn |+| ExploreStyles.ModesHeader
                    )(
                      ^.textTransform.capitalize.when(c.id.toString =!= ResolutionColumnId.value),
                      ^.textTransform.none.when(c.id.toString === ResolutionColumnId.value)
                    ),
                  row = (rowData: ModesTableDef.RowType) =>
                    TableRow(
                      disabled = !enabledRow(rowData.original),
                      clazz = ExploreStyles.TableRowSelected.when_(
                        selectedIndex.value.exists(_ === rowData.index.toInt)
                      )
                    )(
                      ^.onClick --> (
                        props.scienceMode.set(toggleRow(rowData.original)) >>
                          selectedIndex.setState(rowData.index.toInt.some) >>
                          props.onSelect
                      ),
                      props2Attrs(rowData.getRowProps())
                    ),
                  emptyMessage = "No matching modes"
                )(
                  tableInstance,
                  initialIndex = selectedIndex.value.map(idx => (idx - 2).max(0)),
                  rangeChanged = (
                    (range: ListRange) => visibleRange.setState(range.some)
                  ).some,
                  atTopChange = ((value: Boolean) => atTop.setState(value)).some
                )
                .withRef(virtuosoRef),
              scrollButton(
                Icons.ChevronDoubleUp,
                ExploreStyles.SelectedUp,
                _ - 1,
                idx =>
                  visibleRange.value.exists(range =>
                    (range.startIndex.toInt > 0 || !atTop.value) && range.startIndex > idx - 2
                  )
              ),
              scrollButton(
                Icons.ChevronDoubleDown,
                ExploreStyles.SelectedDown,
                _ + 1,
                idx => visibleRange.value.exists(_.endIndex < idx - 1)
              )
            )
          )
      }
}
