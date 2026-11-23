// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.SizePx
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.virtual.*
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.utils.*
import org.scalajs.dom
import org.scalajs.dom.HTMLDivElement

import scala.collection.immutable.SortedSet

import scalajs.js

final case class TelescopeConfigsEditor(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  pEnabled:         Boolean = true,
  maxOffsets:       Int = Int.MaxValue,
  readonly:         Boolean = false,
  idPrefix:         String = "explicit-offsets"
) extends ReactFnProps(TelescopeConfigsEditor)

/**
 * The rows are shown in a virtualized list that grows up to `VisibleRows` rows and then scrolls, so
 * that a long list of offsets neither pushes the rest of the form out of view nor mounts every row.
 */
object TelescopeConfigsEditor
    extends ReactFnComponent[TelescopeConfigsEditor](props =>
      for
        listRef     <- useRefToVdom[HTMLDivElement]
        // Set when the user adds an offset, so that the list scrolls to it once it's rendered.
        scrollToEnd <- useRef(false)
        count        = props.telescopeConfigs.get.length
        virtualizer <- useVirtualizer:
                         VirtualOptions(
                           count = count,
                           getScrollElement = listRef.get,
                           estimateSize = _ => SizePx(TelescopeConfigsEditor.RowHeight),
                           overscan = TelescopeConfigsEditor.Overscan,
                           // The focused row is kept rendered, so that scrolling doesn't lose an
                           // edit in progress. It's looked up on every call, since focus changes
                           // don't re-render.
                           rangeExtractor = range =>
                             TelescopeConfigsEditor.rangeIncluding(
                               TelescopeConfigsEditor.focusedRow(listRef.get.runNow())
                             )(range)
                         )
        _           <- useEffectWithDeps(count): n =>
                         val scroll: Callback = Callback:
                           virtualizer.scrollToIndex(n - 1, TelescopeConfigsEditor.ScrollToEnd)
                         scrollToEnd.get.flatMap(b => (scroll >> scrollToEnd.set(false)).when_(b))
      yield
        val views: Vector[View[TelescopeConfig]] =
          props.telescopeConfigs.toNelOfViews.toList.toVector

        val (_, paddingBefore, paddingAfter) = virtualOffsets(virtualizer, js.undefined)

        // Wide enough for the longest label, so the label column doesn't change width on scrolling.
        val labelWidth: String = s"${count.toString.length + 1}ch"

        React.Fragment(
          // An explicit height, not one coming from the rows: the virtualizer renders rows only
          // once it has measured a non-empty list.
          <.div.withRef(listRef)(TelescopeConfigEditorStyles.List)(
            ^.height := s"${TelescopeConfigsEditor.height(count)}px"
          )(
            // The padding, which stands in for the rows that aren't rendered, goes on this inner
            // element so that it adds to what is scrolled rather than to the list's own height.
            <.div(TelescopeConfigEditorStyles.ListRows)(
              VdomStyle("gridAutoRows") := s"minmax(${TelescopeConfigsEditor.RowHeight}px, auto)",
              ^.paddingTop              := s"${paddingBefore}px",
              ^.paddingBottom           := s"${paddingAfter}px"
            )(
              virtualizer
                .getVirtualItems()
                .toList
                .map: item =>
                  val idx = item.index.toInt
                  <.div(TelescopeConfigEditorStyles.ListItem)(
                    ^.key                                    := s"${props.idPrefix}-row-$idx",
                    VdomAttr(TelescopeConfigsEditor.RowAttr) := idx
                  )(
                    <.label(^.htmlFor := s"${props.idPrefix}-$idx-p", ^.minWidth := labelWidth)(
                      s"${idx + 1}:"
                    ),
                    TelescopeConfigEditorRow(
                      telescopeConfig = views(idx),
                      idx = idx,
                      idPrefix = props.idPrefix,
                      pEnabled = props.pEnabled,
                      remove = props.telescopeConfigs.mod: configs =>
                        NonEmptyList
                          .fromList:
                            configs.take(idx) ++ configs.toList.drop(idx + 1)
                          .getOrElse(NonEmptyList.one(TelescopeConfig.Default)),
                      readonly = props.readonly
                    )
                  )
                .toVdomArray
            )
          ),
          Button(
            icon = Icons.ThinPlus,
            severity = Button.Severity.Success,
            disabled = props.readonly || count >= props.maxOffsets,
            tooltip = "Add Offset",
            text = true,
            clazz = LucumaPrimeStyles.FormField |+| TelescopeConfigEditorStyles.AddOffset,
            onClick =
              scrollToEnd.set(true) >> props.telescopeConfigs.mod(_.append(TelescopeConfig.Default))
          ).mini.compact
        )
    ):
  private val VisibleRows: Int = 8
  private val RowHeight: Int   = 30
  private val Overscan: Int    = 4

  private def height(count: Int): Int = count.min(VisibleRows) * RowHeight

  private val ScrollToEnd =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.auto)
      .setAlign(rawVirtual.mod.ScrollAlignment.end)

  /**
   * The visible rows plus overscan, extended to include `row` if given. The range is kept
   * contiguous, since the space taken by the rows that aren't rendered is given as padding before
   * and after the rendered ones.
   */
  private val RowAttr: String = "data-offset-row"

  /** The index of the row holding the focus, if the focus is in `list`. */
  private def focusedRow(list: Option[HTMLDivElement]): Option[Int] =
    for
      l   <- list
      el  <- Option(dom.document.activeElement).filter(l.contains)
      row <- Option(el.closest(s"[$RowAttr]"))
      idx <- Option(row.getAttribute(RowAttr)).flatMap(_.toIntOption)
    yield idx

  private def rangeIncluding(row: Option[Int])(range: VirtualRange): SortedSet[Int] =
    val visible = range.withOverscan
    row
      .filter(_ < range.count)
      .fold(visible): r =>
        SortedSet.from(visible.headOption.fold(r)(_.min(r)) to visible.lastOption.fold(r)(_.max(r)))
