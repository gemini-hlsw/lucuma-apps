// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.Focused
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.enums.AppTab
import explore.model.enums.TableId
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Display
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import monocle.Iso
import org.scalajs.dom.File as DOMFile

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

object TargetSummaryTile:
  def apply(
    userId:             Option[User.Id],
    programId:          Program.Id,
    targets:            View[TargetList],
    targetObservations: Map[Target.Id, SortedSet[Observation.Id]],
    selectObservation:  (Observation.Id, Target.Id) => Callback,
    selectedTargetIds:  View[List[Target.Id]],
    focusedTargetId:    Option[Target.Id],
    focusTargetId:      Option[Target.Id] => Callback,
    readonly:           Boolean,
    backButton:         VdomNode
  ): Tile[TargetSummaryTile.TileState] =
    Tile(
      ObsTabTileIds.TargetSummaryId.id,
      s"Target Summary (${targets.get.size})",
      TileState.Initial,
      backButton.some
    )(
      tileState =>
        Body(
          userId,
          programId,
          targets,
          targetObservations,
          selectObservation,
          selectedTargetIds,
          focusedTargetId,
          focusTargetId,
          tileState.get.filesToImport.size,
          tileState.zoom(TileState.columnVisibility),
          tileState.zoom(TileState.toggleAllRowsSelected).set.compose(_.some),
          tileState.get.disposition
        ),
      (tileState, _) =>
        Title(
          programId,
          readonly,
          tileState.zoom(TileState.filesToImport),
          tileState.zoom(TileState.columnVisibility),
          focusTargetId,
          tileState.get.toggleAllRowsSelected,
          tileState.zoom(TileState.disposition)
        )
    )

  case class TileState(
    filesToImport:         List[DOMFile],
    columnVisibility:      ColumnVisibility,
    toggleAllRowsSelected: Option[Boolean => Callback],
    disposition:           TargetDisposition
  )

  object TileState:
    val Initial: TileState =
      TileState(List.empty, TargetColumns.DefaultVisibility, none, TargetDisposition.Science)

    val filesToImport         = Focus[TileState](_.filesToImport)
    val columnVisibility      = Focus[TileState](_.columnVisibility)
    val toggleAllRowsSelected = Focus[TileState](_.toggleAllRowsSelected)
    val disposition           = Focus[TileState](_.disposition)

  private val IdColumnId: ColumnId           = ColumnId("id")
  private val CountColumnId: ColumnId        = ColumnId("count")
  private val ObservationsColumnId: ColumnId = ColumnId("observations")

  private val ColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(IdColumnId -> "Id") ++
      TargetColumns.AllColNames ++
      TreeSeqMap(
        CountColumnId        -> "Count",
        ObservationsColumnId -> "Observations"
      )

  private case class Title(
    programId:             Program.Id,
    readonly:              Boolean,
    filesToImport:         View[List[DOMFile]],
    columnVisibility:      View[ColumnVisibility],
    focusTargetId:         Option[Target.Id] => Callback,
    toggleAllRowsSelected: Option[Boolean => Callback],
    disposition:           View[TargetDisposition]
  ) extends ReactFnProps(Title.component)

  private object Title {
    private type Props = Title

    private given Display[TargetDisposition] = Display.byShortName:
      case TargetDisposition.Science     => "Science"
      case TargetDisposition.Calibration => "Calibration"
      case TargetDisposition.BlindOffset => "Blind Offset"

    private val component =
      ScalaFnComponent[Props]: props =>
        def onTextChange(e: ReactEventFromInput): Callback =
          val files = e.target.files.toList
          // set value to null so we can reuse the import button
          (Callback(e.target.value = null) *> props.filesToImport.set(files))
            .when_(files.nonEmpty)

        React.Fragment(
          <.div(ExploreStyles.TableSelectionToolbar)(
            React.Fragment(
              if (props.readonly) EmptyVdom
              else
                React.Fragment(
                  HelpIcon("target/main/target-import.md".refined),
                  <.label(
                    PrimeStyles.Component |+| PrimeStyles.Button |+|
                      PrimeStyles.ButtonSmall |+| LucumaPrimeStyles.Compact |+| ExploreStyles.FileUpload,
                    ^.htmlFor := "target-import"
                  )(
                    Icons.FileArrowUp
                      .withClass(PrimeStyles.ButtonIcon |+| PrimeStyles.ButtonIconLeft),
                    "Import"
                  ),
                  <.input(
                    ^.tpe    := "file",
                    ^.onChange ==> onTextChange,
                    ^.id     := "target-import",
                    ^.name   := "file",
                    ^.accept := ".csv"
                  ),
                  TargetImportPopup(props.programId, props.filesToImport)
                ),
              props.toggleAllRowsSelected.map: toggleAllRowsSelected =>
                React.Fragment(
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.CheckDouble,
                    label = "Select All",
                    onClick = toggleAllRowsSelected(true)
                  ).compact,
                  Button(
                    size = Button.Size.Small,
                    icon = Icons.SquareXMark,
                    label = "Select None",
                    onClick = props.focusTargetId(none) >> toggleAllRowsSelected(false)
                  ).compact
                ),
              EnumDropdownView(
                id = "target-disposition".refined,
                value = props.disposition
              )
            )
          ),
          ColumnSelectorInTitle(ColNames.toList, props.columnVisibility)
        )
  }

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    targets:                  View[TargetList],
    targetObservations:       Map[Target.Id, SortedSet[Observation.Id]],
    selectObservation:        (Observation.Id, Target.Id) => Callback,
    selectedTargetIds:        View[List[Target.Id]],
    focusedTargetId:          Option[Target.Id],
    focusTargetId:            Option[Target.Id] => Callback,
    filesToImportCount:       Int,
    columnVisibility:         View[ColumnVisibility],
    setToggleAllRowsSelected: (Boolean => Callback) => Callback,
    disposition:              TargetDisposition
  ) extends ReactFnProps(Body.component)

  private object Body:
    private type Props = Body

    private val ColDef = ColumnDef[TargetWithId]

    private val ColumnClasses: Map[ColumnId, Css] = Map(
      IdColumnId                 -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
      TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
      TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
    )

    private val ScrollOptions =
      rawVirtual.mod
        .ScrollToOptions()
        .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
        .setAlign(rawVirtual.mod.ScrollAlignment.center)

    private def column[V](id: ColumnId, accessor: TargetWithId => V) =
      ColDef(id, row => accessor(row), ColNames(id))

    private def columns(
      ctx:                AppContext[IO],
      programId:          Program.Id,
      targetObservations: Map[Target.Id, SortedSet[Observation.Id]],
      focusTargetId:      Option[Target.Id] => Callback,
      selectObservation:  (Observation.Id, Target.Id) => Callback
    ) =
      List(
        ColDef(
          IdColumnId,
          _.id,
          "id",
          cell =>
            <.a(
              ^.href := ctx.pageUrl(
                (AppTab.Targets, programId, Focused.target(cell.value)).some
              ),
              ^.onClick ==> (e =>
                e.preventDefaultCB >> e.stopPropagationCB >>
                  focusTargetId(cell.value.some)
              )
            )(
              cell.value.toString
            )
        ).sortable
      ) ++
        TargetColumns.Builder.ForProgram(ColDef).AllColumns ++
        List(
          column(
            CountColumnId,
            x => targetObservations.get(x.id).map(_.size).orEmpty
          ) // TODO Right align
            .withCell(_.value.toString),
          column(
            ObservationsColumnId,
            x => (x.id, targetObservations.get(x.id).orEmpty.toList)
          )
            .withCell(cell =>
              val (tid, obsIds) = cell.value
              <.span(
                obsIds
                  .map(obsId =>
                    <.a(
                      ^.href := ctx.pageUrl(
                        (AppTab.Targets, programId, Focused.singleObs(obsId, tid.some)).some
                      ),
                      ^.onClick ==> (e =>
                        e.preventDefaultCB >> e.stopPropagationCB >>
                          selectObservation(obsId, cell.row.original.id)
                      ),
                      obsId.show
                    )
                  )
                  .mkReactFragment(", ")
              )
            )
            .withEnableSorting(false)
        )

    private val targetIds2RowSelection: Iso[List[Target.Id], RowSelection] =
      Iso[List[Target.Id], RowSelection](targetIds =>
        RowSelection:
          targetIds.map(targetId => RowId(targetId.toString) -> true).toMap
      )(selection =>
        selection.value
          .filter(_._2)
          .keys
          .toList
          .map(rowId => Target.Id.parse(rowId.value))
          .flattenOption
      )

    private val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx            <- useContext(AppContext.ctx)
          cols           <- useMemo(()): _ =>
                              columns(
                                ctx,
                                props.programId,
                                props.targetObservations,
                                props.focusTargetId,
                                props.selectObservation
                              )
          rows           <- useMemo(props.targets.get, props.disposition): (targets, disposition) =>
                              targets.toList
                                .filter((_, twid) => twid.disposition === disposition)
                                .map(_._2)
          table          <- useReactTableWithStateStore:
                              import ctx.given

                              val rowSelection: View[RowSelection] =
                                props.selectedTargetIds.as(targetIds2RowSelection)

                              TableOptionsWithStateStore(
                                TableOptions(
                                  cols,
                                  rows,
                                  getRowId = (row, _, _) => RowId(row.id.toString),
                                  enableSorting = true,
                                  enableColumnResizing = true,
                                  columnResizeMode = ColumnResizeMode.OnChange,
                                  enableMultiRowSelection = true,
                                  state = PartialTableState(
                                    columnVisibility = props.columnVisibility.get,
                                    rowSelection = rowSelection.get
                                  ),
                                  onColumnVisibilityChange = stateInViewHandler(props.columnVisibility.mod),
                                  onRowSelectionChange = stateInViewHandler(
                                    rowSelection
                                      .withOnMod: rs =>
                                        // We'll only unfocus if something is selected. Otherwise this is
                                        // called on initial load and prevents direct navigation to a url for
                                        // a target, and also doesn't allow focusing of a newly created target
                                        // while an observation is selected. See https://app.shortcut.com/lucuma/story/4425/select-newly-created-target
                                        props.focusTargetId(none).unless_(rs.value.isEmpty)
                                      .mod(_)
                                  )
                                ),
                                TableStore(props.userId, TableId.TargetsSummary, cols)
                              )
          _              <- useEffectOnMount:
                              props.setToggleAllRowsSelected(table.toggleAllRowsSelected)
          virtualizerRef <- useRef(none[HTMLTableVirtualizer])
          resizer        <- useResizeDetector
          _              <- useEffectWithDeps(props.focusedTargetId, resizer): (focusedTargetId, _) =>
                              focusedTargetId.foldMap: targetId =>
                                virtualizerRef.get.flatMap: refOpt =>
                                  val focusedTargetIdStr: String = targetId.toString
                                  Callback:
                                    for
                                      virtualizer <- refOpt
                                      idx         <- table
                                                       .getRowModel()
                                                       .flatRows
                                                       .indexWhere(_.id.value === focusedTargetIdStr)
                                                       .some
                                                       .filterNot(_ == -1)
                                    yield virtualizer.scrollToIndex(idx + 1, ScrollOptions)
        yield PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
          headerCellMod = headerCell =>
            ColumnClasses
              .get(headerCell.column.id)
              .orEmpty |+| ExploreStyles.StickyHeader,
          rowMod = row =>
            TagMod(
              ExploreStyles.TableRowSelected.when_(
                row.getIsSelected() || props.focusedTargetId.exists(_.toString === row.id.value)
              ),
              ^.onClick ==> table.getMultiRowSelectedHandler(row.id)
            ),
          cellMod = cell => ColumnClasses.get(cell.column.id).orEmpty,
          virtualizerRef = virtualizerRef,
          emptyMessage = <.div(Constants.NoTargets)
          // workaround to redraw when files are imported
        ).withKey(s"summary-table-${props.filesToImportCount}")
