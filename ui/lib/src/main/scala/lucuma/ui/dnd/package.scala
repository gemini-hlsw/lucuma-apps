// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.dnd

import cats.Endo
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.SizePx
import lucuma.react.pragmaticdnd.facade.Edge

private val OpeningColor = "var(--dragging-background-color)"

def dragOverStyle(height: SizePx, edge: Edge): TagMod =
  edge match
    case Edge.Top    =>
      TagMod(
        ^.paddingTop         := height.render,
        ^.transitionDuration := "0.2s",
        ^.backgroundImage    := s"linear-gradient(to bottom, $OpeningColor 0px, $OpeningColor ${height.render}, transparent ${height.render})"
      )
    case Edge.Bottom =>
      TagMod(
        ^.paddingBottom      := height.render,
        ^.transitionDuration := "0.2s",
        ^.backgroundImage    := s"linear-gradient(to top, $OpeningColor 0px, $OpeningColor ${height.render}, transparent ${height.render})"
      )
    case _           => TagMod.empty

def computeIndexInList[A](nextTo: A => Boolean, position: Edge)(list: List[A]): Option[Int] =
  list.zipWithIndex
    .collectFirst { case (a, idx) if nextTo(a) => idx }
    .map: idx =>
      position match
        case Edge.Top | Edge.Left     => idx
        case Edge.Bottom | Edge.Right => idx + 1

def insertIntoList[A](elem: A, nextTo: A => Boolean, position: Edge): Endo[List[A]] =
  list =>
    computeIndexInList(nextTo, position)(list).fold(list): idx =>
      val (before, after) = list.splitAt(idx)
      before ++ (elem :: after)
