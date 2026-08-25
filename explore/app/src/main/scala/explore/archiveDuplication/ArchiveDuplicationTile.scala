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
import lucuma.react.primereact.*
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

// Each observation fans out server-side into several archive queries for its instrument
// equivalence group, so four concurrent observations is already a dozen or more archive queries in
// flight.
private val MaxConcurrentSearches = 4

private given configsReuse: Reusability[Map[Observation.Id, Option[BasicConfiguration]]] =
  Reusability.byEq
private given controlsReuse: Reusability[ArchiveDuplicationControls]                     = Reusability.byEq
private given entriesReuse: Reusability[List[ArchiveDuplicationEntry]]                   = Reusability.byEq
private given matchCacheReuse: Reusability[Map[Observation.Id, Pot[List[ArchiveMatch]]]] =
  Reusability.byEq

/**
 * The Archive Duplication Search tile: the program's observations with their Match Count,
 * expandable to the Archive Matches the Search found.
 *
 * It pulls its own data and holds its own state — nothing pushes archive results to Explore. See
 * docs/adr/0007-archive-duplication-is-pulled-not-pushed.md.
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
          // The ODB reports NOT_APPLICABLE for an observation with no observing mode, and nothing
          // pushes a new result once one is set (ADR 0007). Re-reading the headers when a
          // configuration changes is what lets such an observation join the table.
          _                <- useEffectWithDeps(
                                props.observations.view.mapValues(_.basicConfiguration).toMap
                              ): _ =>
                                ctx.odbApi
                                  .programArchiveDuplications(props.programId)
                                  .attempt
                                  .flatMap:
                                    case Right(headers) =>
                                      // A Search in flight outranks the stored header it is about
                                      // to replace.
                                      duplications.async.mod: current =>
                                        headers.map: (obsId, header) =>
                                          obsId -> current
                                            .get(obsId)
                                            .filter(_.isPending)
                                            .getOrElse(Pot(header))
                                    case Left(t)        =>
                                      duplications.async.set:
                                        props.observations.keys
                                          .map(_ -> Pot.error[ArchiveDuplication](t))
                                          .toMap
                                  .guarantee(headersLoaded.async.set(true))
          search           <- HookResult:
                                ProgramArchiveDuplications(
                                  props.observations,
                                  props.targets,
                                  duplications.get,
                                  headersLoaded.get,
                                  props.readonly,
                                  props.proposalStatus
                                )
          // The mutation hands back the new result, so it is merged straight into local state: no
          // refetch, no cache invalidation. A cached match set is dropped, since it now describes
          // an older Search. Failures are isolated per observation, so a sweep goes on.
          runSearch        <- HookResult: (obsId: Observation.Id) =>
                                duplications.async.mod(_.updated(obsId, Pot.pending)) >>
                                  ctx.odbApi
                                    .refreshArchiveDuplication(obsId)
                                    .attempt
                                    .flatMap:
                                      case Right(dupli) =>
                                        duplications.async.mod(_.updated(obsId, Pot(dupli))) >>
                                          matches.async.mod(_ - obsId)
                                      case Left(t)      =>
                                        duplications.async.mod(_.updated(obsId, Pot.error(t)))
          // A ref rather than the state view: `onExpand` is captured by the memoized columns, so a
          // render-time snapshot of the match cache would go stale and re-fetch on every expand.
          requested        <- useRef(Set.empty[Observation.Id])
          // Collapsing and re-expanding a row does not re-fetch.
          onExpand         <- HookResult: (obsId: Observation.Id) =>
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
          controls         <- HookResult:
                                ArchiveDuplicationControls(
                                  search.controlsEnabled && !search.searchInFlight,
                                  search.disabledReason
                                )
          cols             <- useMemo(controls): ctrls =>
                                columns(
                                  props.programId,
                                  ctx,
                                  ctrls,
                                  onExpand,
                                  obsId => runSearch(obsId).runAsyncAndForget
                                )
          rows             <- useMemo((search.entries, matches.get)): (entries, matchCache) =>
                                def subRows(
                                  entry: ArchiveDuplicationEntry
                                ): List[Expandable[ArchiveDuplicationRow]] =
                                  if !entry.hasMatches then Nil
                                  else
                                    matchCache.get(entry.id) match
                                      case Some(Pot.Ready(found)) =>
                                        found.map(m => Expandable(MatchRow(entry.id, m)))
                                      case Some(Pot.Error(t))     =>
                                        List(
                                          Expandable(
                                            StatusRow(
                                              entry.id,
                                              s"Could not load matches: ${t.getMessage}",
                                              false
                                            )
                                          )
                                        )
                                      case _                      =>
                                        // Stands in for the matches until they are fetched, and is
                                        // what makes a collapsed row expandable.
                                        List(
                                          Expandable(StatusRow(entry.id, "Loading matches…", true))
                                        )

                                entries.map: entry =>
                                  Expandable(ObsRow(entry), subRows(entry))
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
          val sweepDisabled: Boolean =
            !search.controlsEnabled || !headersLoaded.get || search.searchInFlight ||
              search.sweepObservations.isEmpty

          val sweepTooltip: String =
            search.disabledReason.getOrElse:
              if !headersLoaded.get then "Loading the stored Search results…"
              else if search.searchInFlight then "A Search is already running."
              else if search.sweepObservations.isEmpty then "Every observation has been checked."
              else
                s"Run the Archive Duplication Search for ${search.sweepObservations.length} unchecked observation(s)"

          val sweep: Callback =
            search.sweepObservations
              .parTraverseN(MaxConcurrentSearches)(runSearch)
              .void
              .runAsyncAndForget

          val title: VdomNode =
            if tileSize === TileSizeState.Minimized then EmptyVdom
            else
              React.Fragment(
                <.span(ExploreStyles.TableSelectionToolbar)(
                  <.span(s"${search.withMatchesCount} of ${search.entries.length} with matches"),
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.ListCheck,
                    label = "Check Unchecked",
                    disabled = sweepDisabled,
                    tooltip = sweepTooltip,
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
              ).withTooltip(content =
                search.notApplicable.map(o => o.reference.fold(o.id.show)(_.label)).mkString(", ")
              )

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
