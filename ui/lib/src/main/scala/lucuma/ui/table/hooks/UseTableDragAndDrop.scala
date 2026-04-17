// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.SizePx
import lucuma.react.pragmaticdnd.*
import lucuma.react.pragmaticdnd.facade.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.dnd.*
import org.scalajs.dom.HTMLElement

import scala.annotation.unused

import scalajs.js

private type Target[D] = (data: D, edge: Edge)

private val DefaultGapHeight: SizePx = 15.toPx

final case class RowDraggingInfo[D](
  isDragging:     Option[D],
  isDraggingOver: Option[Target[D]]
)

final case class UseTableDragAndDrop[D, T, TM, CM, TF](
  rowMod:  ((Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) => (
    Row[T, TM, CM, TF],
    Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
  ) => VdomNode,
  cellMod: ((Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) => (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode,
  context: Context.Provided[DragAndDropContext]
)

final case class UseVirtualizedTableDragAndDrop[D, T, TM, CM, TF](
  useTableDragAndDrop: UseTableDragAndDrop[D, T, TM, CM, TF],
  containerRef:        Ref.ToVdom[HTMLElement]
):
  export useTableDragAndDrop.*

// Allow skipping the tagMod function if not needed.
extension [D, T, TM, CM, TF](
  rowMod: ((Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) => (
    Row[T, TM, CM, TF],
    Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
  ) => VdomNode
)
  def apply()
    : (Row[T, TM, CM, TF], Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]) => VdomNode =
    rowMod((_, _) => TagMod.empty)

extension [D, T, TM, CM, TF](
  cellMod: ((Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) => (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode
)
  def apply(): (
    Cell[T, Any, TM, CM, TF, Any, Any],
    Option[Ref.ToVdom[HTMLElement]],
    TagOf[HTMLElement]
  ) => VdomNode =
    cellMod((_, _) => TagMod.empty)

object UseTableDragAndDrop:
  extension [D](dragOver: List[DropTargetRecord[D]])
    private def headTarget: Option[Target[D]] =
      dragOver.headOption.map(_.data).flatMap(d => d.extractClosestEdge.map((d.value, _)))

  def useTableDragAndDrop[D: cats.Eq, T, TM, CM, TF](
    @unused table: Table[T, TM, CM, TF], // Not used, just to infer type parameters.
    handleColId:   ColumnId,
    getData:       Row[T, TM, CM, TF] => D,
    canDrag:       js.UndefOr[(D, DraggableGetFeedbackArgs) => Boolean] = js.undefined,
    canDrop:       js.UndefOr[(D, DropTargetGetFeedbackArgs[D]) => Boolean] = js.undefined,
    onDrop:        (D, Option[Target[D]]) => Callback = (_: D, _: Option[Target[D]]) => Callback.empty,
    gapHeight:     SizePx = DefaultGapHeight
  ): HookResult[UseTableDragAndDrop[D, T, TM, CM, TF]] =
    for dndScope <- useDragAndDropScope[D, D](
                      onDrop = payload =>
                        val sourceData: D                     = payload.source.data.value
                        val targetDataEdge: Option[Target[D]] =
                          payload.location.current.dropTargets.toList.headTarget
                        onDrop(sourceData, targetDataEdge)
                    )
    yield
      val draggingInfo: RowDraggingInfo[D] =
        RowDraggingInfo(dndScope.dragging.map(_.value), dndScope.dragOver.headTarget)

      val rowMod =
        (tagMod: (Row[T, TM, CM, TF], RowDraggingInfo[D]) => TagMod) =>
          (
            row:    Row[T, TM, CM, TF],
            render: Option[Ref.ToVdom[HTMLElement]] => TagOf[HTMLElement]
          ) =>
            val rowData: D = getData(row)
            dndScope.dragging.map(_.value) match
              case Some(data) if data === rowData => EmptyVdom
              case _                              =>
                DraggableDropTargetWithHandle[D, D](
                  handleRef => render(Some(handleRef))(tagMod(row, draggingInfo)),
                  getInitialData = _ => Data(rowData),
                  getData = args => Data(rowData).attachClosestEdge(args, Axis.Vertical.edges),
                  canDrag = canDrag.map(f => args => f(rowData, args)),
                  canDrop = canDrop.map(f => args => f(rowData, args))
                ).withKey(s"draggable-${row.id.value}").toUnmounted: VdomNode

      val cellMod =
        (tagMod: (Cell[T, Any, TM, CM, TF, Any, Any], RowDraggingInfo[D]) => TagMod) =>
          (
            cell:    Cell[T, Any, TM, CM, TF, Any, Any],
            context: Option[Ref.ToVdom[HTMLElement]],
            render:  TagOf[HTMLElement]
          ) =>
            val rowData: D = getData(cell.row)
            context
              .filter(_ => cell.column.id == handleColId)
              .map(handleRef => render.withRef(handleRef))
              .getOrElse(render)(
                (draggingInfo.isDraggingOver, dndScope.dragging.map(_.value)) match
                  case (Some((data, edge)), Some(sData)) if data === rowData && data =!= sData =>
                    dragOverStyle(gapHeight, edge)
                  case _                                                                       => TagMod.empty
              )(tagMod(cell, draggingInfo))

      UseTableDragAndDrop[D, T, TM, CM, TF](rowMod, cellMod, dndScope.context)

  def useVirtualizedTableDragAndDrop[D: cats.Eq, T, TM, CM, TF](
    @unused table: Table[T, TM, CM, TF], // Not used, just to infer type parameters.
    handleColId:   ColumnId,
    getData:       Row[T, TM, CM, TF] => D,
    canDrag:       js.UndefOr[(D, DraggableGetFeedbackArgs) => Boolean] = js.undefined,
    canDrop:       js.UndefOr[(D, DropTargetGetFeedbackArgs[D]) => Boolean] = js.undefined,
    onDrop:        (D, Option[Target[D]]) => Callback = (_: D, _: Option[Target[D]]) => Callback.empty,
    containerRef:  js.UndefOr[Ref.ToVdom[HTMLElement]] = js.undefined
  ): HookResult[UseVirtualizedTableDragAndDrop[D, T, TM, CM, TF]] =
    for
      tableDnd     <- useTableDragAndDrop(table, handleColId, getData, canDrag, canDrop, onDrop)
      containerRef <-
        useAutoScrollRef(getAllowedAxis = _ => Axis.Vertical, containerRef = containerRef)
    yield UseVirtualizedTableDragAndDrop(tableDnd, containerRef)
