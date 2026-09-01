// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.SelectedPanel
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.Css
import lucuma.react.draggable.Axis
import lucuma.react.primereact.Button
import lucuma.react.resizable.*
import lucuma.react.resizeDetector.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.window

enum RightSideCardinality:
  case Single, Multi

trait TwoPanels {

  def makeBackButton(
    programId: Program.Id,
    appTab:    AppTab,
    pv:        View[SelectedPanel],
    ctx:       AppContext[IO]
  ): VdomNode =
    Button(
      icon = Icons.ChevronLeft,
      severity = Button.Severity.Secondary,
      text = true,
      clazz = ExploreStyles.TileBackButton,
      onClick = ctx.pushPage((appTab, programId, Focused.None).some) >>
        pv.set(SelectedPanel.Tree)
    ).mini.compact

  private def tree(
    panel:        VdomNode,
    extraCss:     Css,
    width:        Option[View[Int]],
    onWidthMoved: Int => Callback
  ): VdomNode =
    width.fold(
      <.div(ExploreStyles.Tree, treeInner(panel, extraCss, none))
    ): widthView =>
      // `Resizable` clones its content, merging in the `react-resizable` class (which makes it
      // `position: relative`) and appending an absolutely positioned handle.
      Resizable(
        axis = Axis.X,
        resizeHandles = List(ResizeHandleAxis.East),
        width = widthView.get.toDouble,
        height = 0,
        minConstraints = (Constants.MinTreeWidth, 0),
        maxConstraints = (Constants.MaxTreeWidth, 0),
        onResize = (_, data) => widthView.set(data.size.width),
        onResizeStop = (_, data) => onWidthMoved(data.size.width),
        content = <.div(
          ExploreStyles.Tree |+| ExploreStyles.ResizableTree,
          treeInner(panel, extraCss, widthView.get.some)
        )
      )

  // An inline width overrides the `$tree-section-width` media query rule in explore.scss.
  private def treeInner(panel: VdomNode, extraCss: Css, width: Option[Int]): VdomNode =
    <.div(ExploreStyles.TreeBody |+| extraCss, width.map(w => ^.width := s"${w}px").whenDefined)(
      panel
    )

  def makeOneOrTwoPanels(
    pv:               View[SelectedPanel],
    leftPanel:        VdomNode,
    rightSide:        UseResizeDetectorReturn => VdomNode,
    cardinality:      RightSideCardinality,
    resize:           UseResizeDetectorReturn,
    bodyExtraCss:     Css = Css.Empty,
    treeWidth:        Option[View[Int]] = None,
    onTreeWidthMoved: Int => Callback = _ => Callback.empty
  ): VdomNode =
    if (window.canFitTwoPanels)
      <.div(
        ExploreStyles.TreeRGL,
        tree(leftPanel, bodyExtraCss, treeWidth, onTreeWidthMoved),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).withRef(resize.ref) // we want to measure the grid layout
      )
    else
      <.div(ExploreStyles.TreeRGL)(
        <.div(ExploreStyles.Tree, treeInner(leftPanel, bodyExtraCss, none))
          .when(pv.get.leftPanelVisible),
        <.div(
          ExploreStyles.SinglePanelTile.when(cardinality == RightSideCardinality.Single),
          ExploreStyles.MultiPanelTile.when(cardinality == RightSideCardinality.Multi)
        )(
          rightSide(resize)
        ).when(pv.get.rightPanelVisible)
      ).withRef(resize.ref)
}
