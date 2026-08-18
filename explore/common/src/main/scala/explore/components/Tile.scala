// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.enums.TileHeightPreset
import explore.model.enums.TileSizeState
import explore.model.layout.AutoHeightMinRows
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Slider
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.syntax.all.given

/**
 * In explore we have a concept of a Tile which is a draggable and resizable window that can contain
 * any content. Tiles can be minimized, maximized and resized and dragged from the title Often we
 * want to render content in the body and on the title of the tile, thus you can pass a tileBody and
 * tileTitle function to the Tile constructor returning a VdomNode. The body and title may need to
 * share some state of type A, you can pass an initial value and the body and title will get a
 * View[A] to access the state and modify it.
 *
 * @param id
 *   a unique identifier for the tile
 * @param initialState
 *   the initial state of the tile
 * @param title
 *   the title of the tile to be rendered in the title bar
 * @param backButton
 *   an optional back button to be rendered in the title bar
 * @param canMinimize
 *   whether the tile can be minimized
 * @param canMaximize
 *   whether the tile can be maximized
 * @param hidden
 *   whether the tile is hidden
 * @param autoHeight
 *   whether the tile's row span is derived from its content instead of stored; disables vertical
 *   resizing
 * @param autoHeightMinRows
 *   the row span an auto-height tile may never shrink below, e.g. so an empty tile keeps a
 *   presentable body
 * @param heightPresets
 *   whether to offer height preset slider in the title bar, Ignored for autoHeight tiles
 * @param sizeState
 *   the initial size state of the tile
 * @param sizeStateCallback
 *   a callback to be called when the size state changes
 * @param controllerClass
 *   a css class to be applied to the wrapping div when in a TileController
 * @param bodyClass
 *   a css class to be applied to the tile body
 * @param tileClass
 *   a css class to be applied to the tile
 * @param tileTitleClass
 *   a css class to be applied to the title
 */
// `Tile` needs to know the concrete type so that it can be used to match with the `TileComponent`'s props.
// Since in Scala there's no way for a subtype to know the concrete type of an instance, we need to specify it in a type parameter.
abstract class Tile[This <: Tile[This]](
  val id:                Tile.TileId,
  val title:             VdomNode,
  val renderBackButton:  Option[VdomNode] = None,
  val canMinimize:       Boolean = true,
  val canMaximize:       Boolean = true,
  val hidden:            Boolean = false,
  val autoHeight:        Boolean = false,
  val autoHeightMinRows: Int = AutoHeightMinRows,
  val heightPresets:     Boolean = false,
  val initialSizeState:  TileSizeState = TileSizeState.Maximized,
  val controllerClass:   Css = Css.Empty, // applied to wrapping div when in a TileController.
  val bodyClass:         Css = Css.Empty, // applied to tile body
  val tileClass:         Css = Css.Empty, // applied to the tile
  val tileTitleClass:    Css = Css.Empty  // applied to the title
)(val component: TileComponent[This]):
  type Type = This

protected[components] case class TileState[P <: Tile[P]](
  tileProps:            P,
  renderBackButton:     Option[VdomNode],
  canMinimize:          Boolean = true,
  canMaximize:          Boolean = true,
  sizeState:            TileSizeState,
  sizeStateCallback:    TileSizeState => Callback = (_ => Callback.empty),
  autoHeightCallback:   Int => Callback = (_ => Callback.empty),
  heightPreset:         Option[TileHeightPreset] = None,
  heightPresetCallback: TileHeightPreset => Callback = (_ => Callback.empty)
) extends ReactFnProps(tileProps.component.component):
  val fullSize: Boolean = !tileProps.canMinimize && !tileProps.canMaximize

  val showMaximize: Boolean =
    tileProps.canMaximize && canMaximize && sizeState === TileSizeState.Minimized

  val showMinimize: Boolean =
    tileProps.canMinimize && canMinimize && sizeState === TileSizeState.Maximized

  def withState(
    state:             TileSizeState,
    sizeStateCallback: TileSizeState => Callback
  ): TileState[P] =
    copy(sizeState = state, sizeStateCallback = sizeStateCallback)

  def withAutoHeightCallback(autoHeightCallback: Int => Callback): TileState[P] =
    copy(autoHeightCallback = autoHeightCallback)

  def withHeightPresets(
    heightPreset:         Option[TileHeightPreset],
    heightPresetCallback: TileHeightPreset => Callback
  ): TileState[P] =
    copy(heightPreset = heightPreset, heightPresetCallback = heightPresetCallback)

  def withFullSize: TileState[P] =
    copy(canMinimize = false, canMaximize = false)

  def withBackButton(backButton: Option[VdomNode]): TileState[P] =
    copy(renderBackButton = backButton)

trait TileComponent[P <: Tile[P]](
  useTileContents: (P, TileSizeState) => HookResult[TileContents]
) {
  val component =
    ScalaFnComponent[TileState[P]]: tileState =>
      val props: P = tileState.tileProps

      for
        tileContents  <- useTileContents(props, tileState.sizeState)
        titleResize   <- useResizeDetector
        contentResize <- useResizeDetector
        _             <-
          useEffectWithDeps((props.autoHeight, titleResize.height, contentResize.height)):
            case (true, Some(titleH), Some(contentH)) =>
              tileState.autoHeightCallback(titleH + contentH)
            case _                                    =>
              Callback.empty
      yield
        val maximizeButton: Button =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Maximize,
            onClick = tileState
              .sizeStateCallback(TileSizeState.Maximized)
              .when_(tileState.sizeState.isMinimized)
          )

        val minimizeButton: Button =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Minimize,
            onClick = tileState
              .sizeStateCallback(TileSizeState.Minimized)
              .when_(tileState.sizeState.isMaximized)
          )

        val blankButton = Button(
          text = true,
          clazz = ExploreStyles.TileStateButton,
          icon = Icons.Maximize(^.visibility.hidden),
          disabled = true
        )

        // The handle sits at the preset closest to the tile's current row span.
        val heightPresetSlider: VdomNode =
          val lastOrdinal: Int = TileHeightPreset.values.length - 1
          val stopSpan: Double = 100d / lastOrdinal

          def presetFromFraction(frac: Double): TileHeightPreset =
            TileHeightPreset.fromOrdinal(math.round(frac.max(0d).min(1d) * lastOrdinal).toInt)

          <.div(ExploreStyles.TileHeightPresetSlider, ^.title := "Tile height")(
            <.span(ExploreStyles.TileHeightPresetLabel)(
              tileState.heightPreset.fold("")(_.toString)
            ),
            // Improve the default click behavior
            <.div(
              ExploreStyles.TileHeightPresetTrack,
              ^.onClick ==> { (e: ReactMouseEventFromHtml) =>
                val rect = e.currentTarget.getBoundingClientRect()
                tileState.heightPresetCallback(
                  presetFromFraction((e.clientX - rect.left) / rect.width)
                )
              }
            )(
              Slider(
                value = tileState.heightPreset.fold(0d)(_.ordinal * stopSpan),
                onChange = v => tileState.heightPresetCallback(presetFromFraction(v / 100d))
              ),
              // Step indicators
              <.div(ExploreStyles.TileHeightPresetTicks)(
                TileHeightPreset.values.toList
                  .map(p => <.span(^.key := p.toString, ^.left := s"${p.ordinal * stopSpan}%"))
                  .toTagMod
              )
            )
          )

        val showHeightPresets: Boolean =
          props.heightPresets && !props.autoHeight && !tileState.fullSize

        val body: TagMod =
          if props.autoHeight then
            <.div.withRef(contentResize.ref)(ExploreStyles.AutoHeightTileContent, tileContents.body)
          else tileContents.body

        val bodyClass: Css =
          ExploreStyles.TileBody |+| props.bodyClass |+|
            (if props.autoHeight then ExploreStyles.AutoHeightTileBody else Css.Empty)

        // Tile wrapper
        if (!props.hidden) {
          <.div(
            ExploreStyles.Tile |+| ExploreStyles.FadeIn |+| props.tileClass,
            ^.key := "tile-${props.id.value}"
          )(
            // Tile title. The resize detector only observes auto-height tiles
            <.div.withOptionalRef(Option.when(props.autoHeight)(titleResize.ref))(
              ExploreStyles.TileTitle,
              ^.key := s"tileTitle-${props.id.value}"
            )(
              // Title and optional back button
              <.div(ExploreStyles.TileTitleMenu |+| props.tileTitleClass)(
                tileState.renderBackButton.map(b => <.div(ExploreStyles.TileButton, b)),
                <.div(ExploreStyles.TileTitleText |+| ExploreStyles.TileDraggable, props.title)
              ),
              tileContents.title,
              // Size control buttons
              <.div(ExploreStyles.TileControlButtons)(
                heightPresetSlider.when(showHeightPresets),
                minimizeButton.when(tileState.showMinimize).unless(tileState.fullSize),
                maximizeButton.when(tileState.showMaximize).unless(tileState.fullSize),
                blankButton.when(!tileState.showMinimize && !tileState.showMaximize)
              )
            ),
            // An auto-height tile's body stays mounted while minimized
            <.div(
              ^.key := s"tileBody-${props.id.value}",
              bodyClass,
              ExploreStyles.TileBodyHidden.when(tileState.sizeState === TileSizeState.Minimized)
            )(
              body
            )
              .unless(tileState.sizeState === TileSizeState.Minimized && !props.autoHeight)
          )
        } else EmptyVdom
}

object Tile:
  type TileId = NonEmptyString

  final class Dummy(id: TileId) extends Tile[Dummy](id, "", hidden = true)(Dummy)

  object Dummy
      extends TileComponent[Dummy]((_, _) => HookResult(TileContents(EmptyVdom, EmptyVdom)))
