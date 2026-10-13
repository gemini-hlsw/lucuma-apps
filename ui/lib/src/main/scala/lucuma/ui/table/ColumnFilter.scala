// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table

import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.react.table.Column
import lucuma.react.table.ColumnDef
import lucuma.react.table.Row
import lucuma.react.table.SortingFn
import lucuma.ui.primereact.DebouncedInputText

object ColumnFilter:
  case class Text(
    col:         Column[?, ?, ?, ?, ?, String, ?],
    delayMillis: Int = 250,
    placeholder: String = "<Filter>",
    clazz:       Css = Css.Empty
  ) extends ReactFnProps(Text)

  object Text
      extends ReactFnComponent[Text](props =>
        val filterValue: String = props.col.getFilterValue().map(_.toString).getOrElse("")
        DebouncedInputText(
          id = NonEmptyString.unsafeFrom(s"${props.col.id.value}-filter"),
          delayMillis = props.delayMillis,
          placeholder = props.placeholder,
          value = filterValue,
          onChange = v => props.col.setFilterValue(v.some.filter(_.nonEmpty)),
          clazz = props.clazz
        ).withMods(^.width := "100%")
      )

  /** Requires table with facetedUniqueValues enabled */
  case class Select[A](
    col:         Column[?, A, ?, ?, ?, String, ?],
    display:     A => String = (a: A) => a.toString,
    placeholder: String = "<Filter>",
    showCount:   Boolean = true,
    clazz:       Css = Css.Empty,
    panelClass:  Css = Css.Empty
  ) extends ReactFnProps(Select.component):
    private val present: List[(String, Int)] = presentOptions(col, display)

    // A filter value restored from preferences may match no current row. Without an option for it
    // the dropdown shows the placeholder while the filter still hides every row, so keep it listed
    // (first, since it can't be placed by the column's ordering) and clearable.
    private val missing: Option[(String, Int)] =
      col.getFilterValue().filterNot(v => present.exists(_._1 == v)).map(_ -> 0)

    protected[table] val options: List[SelectItem[String]] =
      (missing.toList ++ present)
        .map: (name, count) =>
          val label: String = if showCount then s"$name (${count})" else name
          SelectItem(name, label)

  /**
   * The non-empty displayed values of the column's faceted rows, with their row counts. Distinct
   * raw values can render identically, so rows are grouped by displayed text. If the column has a
   * sorting function, the options are ordered by it, using the first row of each group as its
   * representative; otherwise, or on ties, they're ordered by displayed text.
   */
  private def presentOptions[T, A, TM, CM, TF](
    col:     Column[T, A, TM, CM, TF, String, ?],
    display: A => String
  ): List[(String, Int)] =
    val groups: List[(String, (Row[T, TM, CM, TF], Int))] =
      col
        .getFacetedRowModel()
        .flatRows
        .groupMapReduce(row => display(row.getValue[A](col.id)))(_ -> 1):
          case ((first, n1), (_, n2)) => first -> (n1 + n2)
        .filter((name, _) => name.nonEmpty)
        .toList

    val byRow: (Row[T, TM, CM, TF], Row[T, TM, CM, TF]) => Int =
      col.columnDef match
        case single: ColumnDef.Single[?, ?, ?, ?, ?, ?, ?] if single.sortingFn.isDefined =>
          val sortingFn: SortingFn[T, TM, CM, TF] = col.getSortingFn()
          sortingFn(_, _, col.id)
        case _                                                                           =>
          (_, _) => 0

    groups
      .sortWith:
        case ((name1, (row1, _)), (name2, (row2, _))) =>
          byRow(row1, row2) match
            case 0 => name1 < name2
            case c => c < 0
      .map((name, group) => name -> group._2)

  trait SelectBuilder[A]:
    type Props = Select[A]

    val component = ScalaFnComponent[Props]: props =>
      DropdownOptional[String](
        id = s"${props.col.id.value}-filter",
        value = props.col.getFilterValue(),
        options = props.options,
        onChange = props.col.setFilterValue(_),
        showClear = true,
        placeholder = props.placeholder,
        clazz = props.clazz,
        panelClass = props.panelClass
      ).withMods(^.width := "100%")

  object Select extends SelectBuilder[Any]
