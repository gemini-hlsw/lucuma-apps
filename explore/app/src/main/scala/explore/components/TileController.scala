// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.*
import cats.Order.*
import cats.syntax.all.*
import clue.FetchClient
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
import japgolly.scalajs.react.util.Effect.Dispatch
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.gridlayout.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Traversal
import queries.schemas.UserPreferencesDB

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
  storeLayout:      Boolean = true,
  onLayoutPersist:  Option[LayoutsMap => Callback] = None
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

  private def storeLayouts[F[_]: {MonadThrow, Dispatch}](
    userId:  Option[User.Id],
    section: GridLayoutSection,
    layouts: ResponsiveLayouts
  )(using FetchClient[F, UserPreferencesDB]): Callback =
    GridLayouts.storeLayoutsPreference[F](userId, section, layouts).runAsyncAndForget

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

  private def updateResizableState(tiles: List[TileState[?]], p: LayoutsMap): LayoutsMap =
    allLayouts
      .andThen(layoutItems)
      .modify {
        case r if tiles.exists(t => t.tileProps.id.value === r.i && t.tileProps.hidden) =>
          // height to 0 for hidden tiles
          r.copy(minH = 0, h = 0, isResizable = false)
        case r if r.h === 1                                                             => r.copy(minH = 1)
        case r                                                                          => r
      }(p)

  // Geometry equality (i/x/y/w/h), ignoring rgl-managed constraints like `minH`.
  private def layoutEquiv(a: Layout, b: Layout): Boolean =
    a.asList
      .map(i => (i.i, i.x, i.y, i.w, i.h))
      .corresponds(b.asList.map(i => (i.i, i.x, i.y, i.w, i.h)))(_ === _)

  private def layoutEquiv(a: Option[Layout], b: Option[Layout]): Boolean =
    (a, b).mapN(layoutEquiv).getOrElse(false)

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx           <- useContext(AppContext.ctx)
        // Get the breakpoint from the layout
        breakpoint    <- useState(
                           getBreakpointFromWidth(
                             props.layoutMap.map { case (x, (w, _, _)) => x -> w },
                             props.gridWidth
                           )
                         )
        // Make a local copy of the layout fixing the state of minimized layouts
        currentLayout <- useStateView(updateResizableState(props.tiles, props.layoutMap))
        // Update the current layout if it changes upstream
        _             <- useEffectWithDeps((props.tiles.map(_.tileProps.hidden), props.layoutMap)):
                           (_, layout) => currentLayout.set(updateResizableState(props.tiles, layout))
        // Layout replaced by the last `onLayoutChange`
        lastReplaced  <- useState(none[(BreakpointName, Layout)])
      yield
        import ctx.given

        def setSizeState(id: Tile.TileId) = (st: TileSizeState) =>
          // Compute the new layout once so the local write and optimistic persist match.
          val f: LayoutItem => LayoutItem = {
            case l if l.i === id.value =>
              if (st === TileSizeState.Minimized) l.copy(h = 1, minH = 1)
              else if (st === TileSizeState.Maximized)
                val defaultHeight =
                  unsafeTileHeight(id).headOption(props.defaultLayout).getOrElse(1)
                l.copy(
                  h = defaultHeight,
                  minH = scala.math.max(l.minH.getOrElse(1), defaultHeight)
                )
              else l
            case l                     => l
          }
          val newLayoutsMap: LayoutsMap   = allTiles.modify(f)(currentLayout.get)
          currentLayout.set(newLayoutsMap) >>
            props.onLayoutPersist.fold(Callback.empty)(_(newLayoutsMap))

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

        ResponsiveReactGridLayout(
          width = props.gridWidth.toDouble,
          breakpoints = currentLayouts.view.mapValues(_._1).toMap,
          cols = currentLayouts.view.mapValues(_._2).toMap,
          layouts = currentLayouts.view.mapValues(_._3).toMap,
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
          onLayoutChange = (m: Layout, newLayouts: ResponsiveLayouts) =>
            // ileController handles how we use react-grid-layout, it passes the layout in, and gets
            // changes back via onLayoutChange. when we remove a tile (like for visitors) it compacts
            // the layout (for example changes the y if a tile is removed), then reports the result
            // via onLayoutChange
            //
            // TileController writes this new layout back into its own state, what seems to happen
            // is we get into a loop where the tile fed back is different (after compactation) so
            // it is written back as state and that gets into this loop that show itself as
            // a React maximum update cycle
            //
            // The fix is to skip the write-back when the reported layout is the same as we have
            //
            // react-grid-layout's compaction isn't idempotent for mixed-width layouts, it
            // re-emits two equivalent layouts in alternation.
            // Storing each verbatim feeds the oscillation back as a new `layouts` prop -> infinite
            // re-compaction ("Maximum update depth exceeded").
            val bp            = breakpoint.value
            val current       = currentLayout.get.get(bp).map(_._3)
            // compare the reported layout with the current one to avoid storing the same layout twice
            val isFixedPoint  = layoutEquiv(current, m.some)
            // detect if there is a cycle and ignore, avoiding to update the local state
            val isCycle       =
              lastReplaced.value.exists((b, l) => b === bp && layoutEquiv(l.some, m.some))
            val newLayoutsMap = breakpointLayout(bp).replace(m).apply(currentLayout.get)
            Callback.when(!isFixedPoint && !isCycle)(
              current.map(c => lastReplaced.setState((bp, c).some)).getOrEmpty >>
                currentLayout.set(newLayoutsMap) >>
                props.onLayoutPersist.map(_(newLayoutsMap)).getOrEmpty
            ) >> storeLayouts(props.userId, props.section, newLayouts)
              .when_(props.storeLayout)
          ,
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
            )
          }.toVdomArray
        )
