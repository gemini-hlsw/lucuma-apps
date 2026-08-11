// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.*
import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.Throttler
import crystal.react.*
import crystal.react.hooks.*
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.enums.GridLayoutSection
import explore.model.enums.TileSizeState
import explore.model.layout.*
import explore.model.layout.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.gridlayout.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Traversal
import org.scalajs.dom
import queries.schemas.UserPreferencesDB

import scala.concurrent.duration.*
import scala.scalajs.js.JSConverters.*

case class TileController(
  userId:           Option[User.Id],
  gridWidth:        Int,
  defaultLayout:    LayoutsMap,
  layoutMap:        LayoutsMap,
  tileDefs:         List[Tile[?]],
  section:          GridLayoutSection,
  renderBackButton: Option[VdomNode] = None,
  clazz:            Option[Css] = None,
  storeLayout:      Boolean = true
) extends ReactFnProps(TileController.component):
  val tiles: List[TileState[?]] =
    tileDefs.map: t =>
      TileState(
        t.asInstanceOf[t.Type],
        t.renderBackButton,
        t.canMinimize,
        t.canMaximize,
        t.initialSizeState
      )

object TileController:
  private type Props = TileController

  private def storeLayouts(
    userId:  Option[User.Id],
    section: GridLayoutSection,
    layouts: ResponsiveLayouts
  )(using FetchClient[IO, UserPreferencesDB]): IO[Unit] =
    GridLayouts.storeLayoutsPreference[IO](userId, section, layouts)

  // Calculate the state out of the height
  private def unsafeSizeToState(
    layoutsMap: LayoutsMap,
    tileId:     Tile.TileId
  ): TileSizeState = {
    val k = allTiles
      .filter(s => s.i === tileId.value)
      .getAll(layoutsMap)
      .headOption

    val h = k.map(layoutItemHeight.get)
    if (h.exists(_ === 1)) TileSizeState.Minimized else TileSizeState.Maximized
  }

  private val allTiles: Traversal[LayoutsMap, LayoutItem] =
    allLayouts.andThen(layoutItems)

  private def unsafeTileHeight(id: Tile.TileId): Traversal[LayoutsMap, Int] =
    allTiles
      .filter(_.i === id.value)
      .andThen(layoutItemHeight)

  private def isAutoHeight(tiles: List[TileState[?]], id: String): Boolean =
    tiles.exists(t => t.tileProps.id.value === id && t.tileProps.autoHeight)

  private def isHidden(tiles: List[TileState[?]], id: String): Boolean =
    tiles.exists(t => t.tileProps.id.value === id && t.tileProps.hidden)

  // The grid's own container is auto-sized to fit its rows (`autoSize = true` below), so it
  // can't be the ceiling's source too: an auto-height tile capped by its own container would
  // collapse the container, which would tighten the cap, indefinitely. The browser viewport
  // can't be circular in that way, at the cost of not excluding other on-screen chrome (tab
  // bars, headers) from the budget.
  private def viewportPx: Int = dom.window.innerHeight.toInt

  // An auto-height tile's local row span is authoritative: its stored height is dead data, and
  // the preferences subscription echoes every store back ~1s later, so adopting the echoed
  // height would collapse a tile the user just maximized (a stale h of 1 reads as "minimized")
  // or floor one they are looking at.
  private def preserveAutoHeights(
    tiles:   List[TileState[?]],
    current: LayoutsMap,
    updated: LayoutsMap
  ): LayoutsMap =
    updated.map { case (bp, (w, c, layout)) =>
      val currentItems: Map[String, LayoutItem] =
        current.get(bp).map(_._3.asList.map(i => i.i -> i).toMap).getOrElse(Map.empty)
      bp -> ((w,
              c,
              Layout(layout.asList.map { item =>
                if isAutoHeight(tiles, item.i) && !isHidden(tiles, item.i) then
                  currentItems
                    .get(item.i)
                    .fold(item)(cur => item.copy(h = cur.h, minH = cur.minH, maxH = cur.maxH))
                else item
              })
      ))
    }

  // Re-derives auto-height row spans from the last reported measurements. Needed wherever a
  // measurement was dropped rather than applied — while a gesture was in flight, or when
  // normalization reset the row span to the floor — because the resize detector only fires when
  // the content's height changes and will not repeat its last value.
  private def applyMeasuredHeights(
    tiles:    List[TileState[?]],
    measured: Map[String, Int],
    layouts:  LayoutsMap
  ): LayoutsMap =
    allTiles.modify { l =>
      if isAutoHeight(tiles, l.i) && !isHidden(tiles, l.i) then
        measured.get(l.i).fold(l)(px => resolveAutoHeight(l, px, viewportPx, l.h === 1))
      else l
    }(layouts)

  private def updateResizableState(tiles: List[TileState[?]], p: LayoutsMap): LayoutsMap =
    allLayouts
      .andThen(layoutItems)
      .modify { r =>
        val hidden     = isHidden(tiles, r.i)
        val autoHeight = isAutoHeight(tiles, r.i)
        if hidden then
          // height to 0 for hidden tiles
          r.copy(minH = 0, h = 0, isResizable = false)
        else if autoHeight then
          // The pinned minH/maxH restrict the regular corner handle to horizontal resizing.
          // A stored h of 1 is a legitimately minimized tile; leave it alone. Any other stored
          // h is stale (it belongs to the previous render, not the current content) and is
          // reset to the floor until the tile mounts and reports its real measurement.
          if r.h === 1 then r.copy(minH = 1, maxH = 1)
          else r.copy(h = AutoHeightMinRows, minH = AutoHeightMinRows, maxH = AutoHeightMinRows)
        else if r.h === 1 then r.copy(minH = 1)
        else r
      }(p)

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx            <- useContext(AppContext.ctx)
        // Get the breakpoint from the layout
        breakpoint     <- useState(
                            getBreakpointFromWidth(
                              props.layoutMap.map { case (x, (w, _, _)) => x -> w },
                              props.gridWidth
                            )
                          )
        // Make a local copy of the layout fixing the state of minimized layouts
        currentLayout  <- useStateView(updateResizableState(props.tiles, props.layoutMap))
        // The last content height an auto-height tile reported, keyed by tile id. Kept across
        // minimize/maximize (unlike `h`, which is overwritten to encode the minimized state) so
        // maximizing can restore the height the content actually wants instead of the floor.
        lastMeasuredPx <- useStateView(Map.empty[String, Int])
        // Update the current layout if it changes upstream. Auto-height tiles keep their local
        // row span and then replay the last measurements on top: the incoming stored heights are
        // dead data for them, and the update is not a content change, so nothing else would
        // restore the derived height.
        _              <- useEffectWithDeps((props.tiles.map(_.tileProps.hidden), props.layoutMap)):
                            (_, layout) =>
                              currentLayout.mod: current =>
                                applyMeasuredHeights(
                                  props.tiles,
                                  lastMeasuredPx.get,
                                  preserveAutoHeights(
                                    props.tiles,
                                    current,
                                    updateResizableState(props.tiles, layout)
                                  )
                                )
        // Suppressed while a drag or resize gesture is in flight, so an auto-height measurement
        // doesn't fight the pointer mid-gesture.
        gesturing      <- useStateView(false)
        storeThrottler <- useMemo(())(_ => Throttler.unsafe[IO](1.second))
      yield
        import ctx.given

        def setSizeState(id: Tile.TileId) = (st: TileSizeState) =>
          currentLayout
            .zoom(allTiles)
            .mod:
              case l if l.i === id.value =>
                if (st === TileSizeState.Minimized)
                  if isAutoHeight(props.tiles, id.value) then l.copy(h = 1, minH = 1, maxH = 1)
                  else l.copy(h = 1, minH = 1)
                else if (st === TileSizeState.Maximized)
                  if isAutoHeight(props.tiles, id.value) then
                    val measuredPx = lastMeasuredPx.get.getOrElse(id.value, 0)
                    resolveAutoHeight(l, measuredPx, viewportPx, false)
                  else
                    val defaultHeight =
                      unsafeTileHeight(id).headOption(props.defaultLayout).getOrElse(1)
                    l.copy(
                      h = defaultHeight,
                      minH = scala.math.max(l.minH.getOrElse(1), defaultHeight)
                    )
                else l
              case l                     => l

        // A minimized tile's body is hidden, so any measurement arriving then — such as the
        // observer's final 0 for a node that just lost its box — is garbage and must not
        // overwrite the last real measurement, which maximize restores from.
        def autoHeightCallback(id: Tile.TileId): Int => Callback = measuredPx =>
          val minimized = unsafeSizeToState(currentLayout.get, id) === TileSizeState.Minimized
          (lastMeasuredPx.mod(_.updated(id.value, measuredPx)) *>
            currentLayout
              .zoom(allTiles)
              .mod:
                case l if l.i === id.value =>
                  resolveAutoHeight(l, measuredPx, viewportPx, false)
                case l                     => l
              .when_(!gesturing.get)).unless_(minimized)

        val tilesWithBackButton: List[TileState[?]] = {
          val topTile =
            currentLayout.get.get(breakpoint.value).flatMap(_._3.asList.sortBy(_.y).headOption)
          (topTile, props.renderBackButton)
            .mapN: (t, _) =>
              props.tiles
                .map:
                  case ti if t.i === ti.tileProps.id.value =>
                    ti.withBackButton(props.renderBackButton)
                  case ti                                  => ti
            .getOrElse(props.tiles)
        }

        val currentLayouts = currentLayout.get

        // react-grid-layout keeps its own copy of the layout, synchronized against its children:
        // a tile that disappears from `tileDefs` is dropped from that copy, and if it comes back
        // rgl has no entry for it anymore, so it invents a collapsed 1x1 one (which then gets
        // persisted).
        //
        // It only re-derives from the `layouts` prop when that prop actually changes,
        // so we restrict it to the rendered tiles: adding or removing a tile then changes the
        // prop and forces rgl to re-derive from `currentLayout`, which is authoritative.
        val renderedIds: Set[String] = props.tiles.map(_.tileProps.id.value).toSet

        val renderedLayouts: Map[BreakpointName, Layout] =
          currentLayouts.view
            .mapValues(e => Layout(e._3.asList.filter(i => renderedIds.contains(i.i))))
            .toMap

        // rgl only ever reports the tiles it renders, so merge instead of replacing, otherwise a
        // drag or resize would drop the entries of the tiles that aren't currently rendered.
        def mergeIntoCurrentLayout(m: Layout): Callback =
          currentLayout.mod(breakpointLayout(breakpoint.value).modify(mergeLayouts(_, m)))

        // Measurements that arrived mid-gesture were recorded but not applied; apply the latest
        // one now, since the content will not re-report a height it already has.
        val endGesture: Callback =
          gesturing.set(false) *>
            currentLayout
              .mod(applyMeasuredHeights(props.tiles, lastMeasuredPx.get, _))
              .when_(props.tiles.exists(_.tileProps.autoHeight))

        ResponsiveReactGridLayout(
          width = props.gridWidth.toDouble,
          breakpoints = currentLayouts.view.mapValues(_._1).toMap,
          cols = currentLayouts.view.mapValues(_._2).toMap,
          layouts = renderedLayouts,
          autoSize = true,
          // Position strategy: we use react-grid-layout's default (CSS transforms).
          // rgl v1 forced us to set `useCSSTransforms = false` because a CSS transform on a grid
          // item creates a containing block that breaks abosule positioning breaking things like
          // the combo boxes or the date picker that renders above the tiles.
          //
          // See
          // https://github.com/react-grid-layout/react-grid-layout/issues/858#issuecomment-426346399
          //
          // In rgl v2 those overlays are portaled out of the grid-item subtree, so the z-inde
          // issue seems to be gone and in fact using the default wors and it should be more performant.
          // If you need to restore the old layout use:
          // `positionStrategy = PositionStrategy.absolute`.
          margin = (Constants.GridRowPadding, Constants.GridRowPadding),
          containerPadding = (Constants.GridRowPadding, 0),
          rowHeight = Constants.GridRowHeight,
          dragConfig = DragConfig(handle = s".${ExploreStyles.TileDraggable.htmlClass}"),
          onBreakpointChange = (bk: BreakpointName, _: Int) =>
            currentLayout
              .mod(_.breakpointProportionalWidth(breakpoint.value, bk))
              .when_(breakpoint.value =!= bk) *>
              breakpoint
                .setState(bk),
          // We deliberately do NOT feed `onLayoutChange` back into `currentLayout`.
          // `onLayoutChange` fires both on user gestures and on react-grid-layout's internal
          // compaction, which isn't idempotent for mixed-width layouts: it alternates between two
          // equivalent layouts. Writing those back into the `layouts` prop re-triggers compaction
          // forever ("Maximum update depth exceeded"). Persisting is fine here (it's an async,
          // non-rendering DB write); we capture genuine user changes via onDragStop/onResizeStop.
          // Throttled: an auto-height tile fires this on every content change, for a value
          // that's overridden on read by `updateResizableState`, so there's no need to persist
          // each one individually.
          onLayoutChange = (_: Layout, newLayouts: ResponsiveLayouts) =>
            storeThrottler
              .submit(storeLayouts(props.userId, props.section, newLayouts))
              .runAsyncAndForget
              .when_(props.storeLayout),
          onDragStart = (_, _, _, _, _, _) => gesturing.set(true),
          onDragStop = (m: Layout, _, _, _, _, _) => mergeIntoCurrentLayout(m) *> endGesture,
          onResizeStart = (_, _, _, _, _, _) => gesturing.set(true),
          onResizeStop = (m: Layout, _, _, _, _, _) => mergeIntoCurrentLayout(m) *> endGesture,
          className = props.clazz.map(_.htmlClass).orUndefined
        )(
          tilesWithBackButton.map { tile =>
            <.div(
              ^.key := tile.tileProps.id.value,
              // Show tile properties on the title if enabled
              currentLayout.get
                .get(breakpoint.value)
                .flatMap { case (p, c, l) =>
                  l.asList
                    .find(_.i === tile.tileProps.id.value)
                    .flatMap { i =>
                      TagMod
                        .devOnly(
                          <.div(
                            ^.cls := "rgl-tile-overlay",
                            s"id: ${i.i} width: ${p} cols: $c bp: ${breakpoint.value} x: ${i.x} y: ${i.y} w: ${i.w} h: ${i.h}${i.minH.toOption
                                .foldMap(m => s" minH: $m")}${i.maxH.toOption
                                .foldMap(m => s" maxH: $m")}${i.minW.toOption
                                .foldMap(m => s" minW: $m")}${i.maxW.toOption
                                .foldMap(m => s" maxW: $m")}${i.isResizable.toOption
                                .foldMap(m => s" isResizable: $m")}"
                          )
                        )
                        .some
                    }
                }
                .getOrElse(EmptyVdom),
              tile.tileProps.controllerClass,
              tile
                .withState(
                  unsafeSizeToState(currentLayout.get, tile.tileProps.id),
                  setSizeState(tile.tileProps.id)
                )
                .withAutoHeightCallback(autoHeightCallback(tile.tileProps.id))
            )
          }.toVdomArray
        )
