// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.archiveDuplication

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.TileComponent
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ArchiveDuplication
import explore.model.ArchiveDuplicationEntry
import explore.model.ArchiveMatch
import explore.model.Observation
import explore.model.ObservationList
import explore.model.OverviewTabTileIds
import explore.model.ProgramArchiveDuplications
import explore.model.TargetList
import explore.model.enums.TableId
import explore.model.enums.TileSizeState
import explore.model.enums.Visible
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProposalStatus
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.CalculationState
import lucuma.react.primereact.*
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

// Each observation fans out server-side into several archive queries for its instrument
// equivalence group. We allow max four concurrent observation queries.
private val MaxConcurrentSearches = 4

private given controlsReuse: Reusability[ArchiveDuplicationControls]                     = Reusability.byEq
private given entriesReuse: Reusability[List[ArchiveDuplicationEntry]]                   = Reusability.byEq
private given matchCacheReuse: Reusability[Map[Observation.Id, Pot[List[ArchiveMatch]]]] =
  Reusability.byEq

/**
 * The Archive Duplication Search tile: the program's observations with their Match Count,
 * expandable to the Archive Matches the Search found.
 */
final case class ArchiveDuplicationTile(
  userId:         Option[User.Id],
  programId:      Program.Id,
  observations:   ObservationList,
  targets:        TargetList,
  proposalStatus: ProposalStatus,
  readonly:       Boolean
) extends Tile[ArchiveDuplicationTile](
      id = OverviewTabTileIds.ArchiveDuplicationId.id,
      title = "Archive Duplication Search"
    )(ArchiveDuplicationTile)

object ArchiveDuplicationTile
    extends TileComponent[ArchiveDuplicationTile]({ (props, tileSize) =>
      import ArchiveDuplicationColumns.*
      import ArchiveDuplicationRow.*

      useContext(AppContext.ctx).flatMap: ctx =>
        import ctx.given

        for
          duplications     <- useStateView(Map.empty[Observation.Id, Pot[ArchiveDuplication]])
          matches          <- useStateView(Map.empty[Observation.Id, Pot[List[ArchiveMatch]]])
          columnVisibility <- useStateView(DefaultColumnVisibility)
          showFilters      <- useStateView(Visible.Hidden)
          headersLoaded    <- useStateView(false)
          _                <- useEffectOnMount:
                                ctx.odbApi
                                  .programArchiveDuplications(props.programId)
                                  .attempt
                                  .flatMap:
                                    case Right(headers) =>
                                      duplications.async.set:
                                        headers.view.mapValues(Pot.apply).toMap
                                    case Left(t)        =>
                                      duplications.async.set:
                                        props.observations.keys
                                          .map(_ -> Pot.error[ArchiveDuplication](t))
                                          .toMap
                                  .guarantee(headersLoaded.async.set(true))
          _                <- useEffectStreamResourceOnMount:
                                ctx.odbApi
                                  .obsCalcSubscription(props.programId)
                                  .map:
                                    _.filter(_.newCalculationState.contains(CalculationState.Ready))
                                      .evalMap: update =>
                                        val obsId = update.observationId
                                        ctx.odbApi
                                          .observationArchiveDuplication(obsId)
                                          .attempt
                                          .flatMap:
                                            case Right(Some(header)) =>
                                              // A Search in flight outranks the stored header it
                                              // is about to replace.
                                              duplications.async.mod: current =>
                                                if current.get(obsId).exists(_.isPending) then current
                                                else current.updated(obsId, Pot(header))
                                            // A row this tile cannot refresh is left as it stands
                                            // rather than blanked: the stale snapshot on screen is
                                            // more use than nothing.
                                            case _                   => IO.unit
          search            = ProgramArchiveDuplications(
                                props.observations,
                                props.targets,
                                duplications.get,
                                headersLoaded.get,
                                props.readonly,
                                props.proposalStatus
                              )
          // A ref rather than the state view: `onExpand` is captured by the memoized columns.
          requested        <- useRef(Set.empty[Observation.Id])
          // The mutation hands back the new result, so it is merged straight into local state: no
          // refetch, no cache invalidation.
          runSearch         = (obsId: Observation.Id) =>
                                // Logged to tell we did a query
                                ctx.logger.info(s"Requesting Archive Duplication Search for $obsId") >>
                                  duplications.async.mod(_.updated(obsId, Pot.pending)) >>
                                  ctx.odbApi
                                    .refreshArchiveDuplication(obsId)
                                    .attempt
                                    .flatMap:
                                      case Right(dupli) =>
                                        ctx.logger.info(
                                          s"Archive Duplication Search for $obsId returned ${dupli.state}" +
                                            s" with ${dupli.matchCount.value} match(es)"
                                        ) >>
                                          duplications.async.mod(_.updated(obsId, Pot(dupli))) >>
                                          matches.async.mod(_ - obsId) >>
                                          requested.mod(_ - obsId).toAsync
                                      case Left(t)      =>
                                        ctx.logger.warn(t)(
                                          s"Archive Duplication Search for $obsId failed"
                                        ) >>
                                          duplications.async.mod(_.updated(obsId, Pot.error(t)))
          // Collapsing and re-expanding a row does not re-fetch.
          onExpand          = (obsId: Observation.Id) =>
                                val load: IO[Unit] =
                                  matches.async.mod(_.updated(obsId, Pot.pending)) >>
                                    ctx.odbApi
                                      .observationArchiveMatches(obsId)
                                      .attempt
                                      .flatMap: result =>
                                        matches.async.mod:
                                          _.updated(obsId, result.fold(Pot.error, Pot.apply))
                                if requested.value.contains(obsId) then Callback.empty
                                else requested.mod(_ + obsId) >> load.runAsyncAndForget
          controls          =
            ArchiveDuplicationControls(search.disabledReason, search.searchInFlight)
          cols             <- useMemo(controls): ctrls =>
                                columns(
                                  props.programId,
                                  ctx,
                                  ctrls,
                                  onExpand,
                                  obsId => runSearch(obsId).runAsyncAndForget
                                )
          rows             <- useMemo((search.entries, matches.get)): (entries, matchCache) =>
                                entries.map: entry =>
                                  Expandable(
                                    ObsRow(entry),
                                    ArchiveDuplicationRow.subRowsFor(entry, matchCache)
                                  )
          tableState       <- useMemo(columnVisibility.get): cv =>
                                PartialTableState(columnVisibility = cv)
          table            <- useReactTableWithStateStore:
                                TableOptionsWithStateStore(
                                  TableOptions(
                                    cols,
                                    rows,
                                    enableExpanding = true,
                                    getSubRows = (row, _) => row.subRows,
                                    getRowId = (row, _, _) => RowId(row.value.rowId),
                                    enableSorting = true,
                                    enableColumnFilters = true,
                                    state = tableState,
                                    onColumnVisibilityChange = columnVisibility.handleTableUpdate
                                  ),
                                  TableStore(props.userId, TableId.ArchiveDuplication),
                                  ColumnsExcludedFromVisibility
                                )
          resizer          <- useResizeDetector
        yield
          val sweep: Callback =
            (ctx.logger.info(
              s"Sweeping Archive Duplication Search over ${search.sweepObservations.length}" +
                s" observation(s), $MaxConcurrentSearches at a time"
            ) >>
              search.sweepObservations
                .parTraverseN(MaxConcurrentSearches)(runSearch)
                .void).runAsyncAndForget

          // Both kinds of wait the tile can be in: reading the stored results on open, and running
          // a Search. They are told apart in the tooltip rather than by two different icons.
          val busyIndicator: VdomNode =
            val waitingFor: Option[String] =
              if !search.headersLoaded then "Loading the stored Search results…".some
              else if search.searchInFlight then "Searching the archive…".some
              else none
            waitingFor.fold(EmptyVdom): content =>
              <.span(Icons.Spinner.withSpin(true)).withTooltip(content = content)

          val title: VdomNode =
            if tileSize === TileSizeState.Minimized then EmptyVdom
            else
              React.Fragment(
                <.span(ExploreStyles.TableSelectionToolbar)(
                  <.span(s"${search.withMatchesCount} of ${search.entries.length} with matches"),
                  busyIndicator,
                  Button(
                    size = Button.Size.Small,
                    // The sweep runs several observations at a time and each one takes seconds, so
                    // the button says it is working rather than only going disabled.
                    icon =
                      if search.searchInFlight then Icons.Spinner.withSpin(true)
                      else Icons.ListCheck,
                    label =
                      if search.sweepObservations.isEmpty then "Run check"
                      else s"Run check (${search.sweepObservations.length})",
                    disabled = search.sweepState.disabled,
                    tooltip = search.sweepState.tooltip,
                    onClick = sweep
                  ).compact,
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.Filter,
                    severity =
                      if showFilters.get.value then Button.Severity.Primary
                      else Button.Severity.Secondary,
                    onClick = showFilters.mod(_.flip) >>
                      table.resetColumnFilters().when_(showFilters.get.value),
                    tooltip = "Toggle column filters"
                  ).compact
                ),
                ColumnSelectorInTitle(SelectableColumnNames, columnVisibility)
              )

          val notApplicableNote: VdomNode =
            if search.notApplicable.isEmpty then EmptyVdom
            else
              <.div(
                s"${search.notApplicable.length} observation(s) cannot be checked against the archive."
              ).withTooltip(content = search.notApplicable.map(_.displayLabel).mkString(", "))

          val body: VdomNode =
            React.Fragment(
              notApplicableNote,
              PrimeAutoHeightVirtualizedTable(
                table,
                _ => 32.toPx,
                striped = true,
                compact = Compact.Very,
                containerRef = resizer.ref,
                tableMod = ExploreStyles.ExploreTable,
                columnFilterRenderer =
                  if showFilters.get.value then FilterMethod.render else _ => EmptyVdom,
                headerCellMod = _ => ExploreStyles.StickyHeader,
                emptyMessage = <.div("No observations to check against the archive.")
              )
            )

          TileContents(title, body)
    })
