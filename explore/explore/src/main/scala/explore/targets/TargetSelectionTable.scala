// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table.Table
import react.semanticui.collections.table.TableCell
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.TableHeaderCell
import react.semanticui.collections.table.TableRow
import react.semanticui.elements.button.Button
import react.semanticui.sizes
import reactST.reactTable.*

case class TargetSelectionTable(
  targets:       List[TargetSearchResult],
  onSelected:    TargetSearchResult => Callback,
  selectedIndex: Option[Int],
  onClick:       (TargetSearchResult, Int) => Callback
) extends ReactFnProps(TargetSelectionTable.component)

object TargetSelectionTable {
  private type Props = TargetSelectionTable

  private val TargetTable = TableDef[TargetSearchResult].withSortBy

  private val TargetTableComponent = new SUITable(TargetTable)

  private val columnClasses: Map[String, Css] = Map(
    "select" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummarySelect),
    "type"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithSelect),
    "name"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithSelect)
  )

  private val component = ScalaFnComponent
    .withHooks[Props]
    // cols
    .useMemoBy(_ => ()) { props => _ =>
      List(
        TargetTable
          .Column("select", target => target)
          .setCell(cell =>
            Button(
              size = sizes.Tiny,
              compact = true,
              positive = true,
              icon = true,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                e.stopPropagationCB >> props.onSelected(cell.value)
            )(
              ^.tpe := "button"
            )(
              cell.value.targetWithOptId.optId.fold(React.Fragment(Icons.New, "Add"))(_ =>
                React.Fragment(Icons.Link, "Link")
              )
            )
          )
          .setDisableSortBy(true)
      ) ++
        TargetColumns
          .BaseColumnBuilder(TargetTable)(_.target.some)
          .allColumns
    }
    // rows
    .useMemoBy((props, _) => props.targets)((_, _) => identity)
    // table
    .useTableBy((_, cols, rows) =>
      TargetTable(
        cols,
        rows,
        Reuse.always((options: TargetTable.OptionsType) =>
          options
            .setAutoResetSortBy(false)
        )
      )
    )
    // .useMemo
    .render((props, _, _, tableInstance) =>
      TargetTableComponent(
        table = Table(celled = true,
                      selectable = true,
                      striped = true,
                      compact = TableCompact.Very,
                      unstackable = true,
                      clazz = ExploreStyles.ExploreTable
        )(),
        header = true,
        headerCell = (col: TargetTable.ColumnType) =>
          TableHeaderCell(clazz =
            columnClasses.get(col.id.toString).orEmpty |+| ExploreStyles.StickyHeader
          )(
            ^.textTransform.none,
            ^.whiteSpace.nowrap
          ),
        row = (rowData: TargetTable.RowType) =>
          TableRow(
            clazz = ExploreStyles.TableRowSelected.when_(
              props.selectedIndex.contains_(rowData.index.toInt)
            )
          )(
            ^.onClick --> props.onClick(rowData.original, rowData.index.toInt),
            props2Attrs(rowData.getRowProps())
          ),
        cell = (cell: TargetTable.CellType[?]) =>
          TableCell(clazz = columnClasses.get(cell.column.id.toString).orEmpty)(
            ^.whiteSpace.nowrap
          )
      )(tableInstance)
    )
}
