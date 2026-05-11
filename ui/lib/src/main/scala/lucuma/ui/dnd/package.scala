// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.dnd

import cats.Endo
import cats.syntax.option.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.SizePx
import lucuma.react.pragmaticdnd.facade.Edge
import lucuma.react.pragmaticdnd.facade.Operation

import scala.annotation.targetName

private val OpeningColor = "var(--dragging-background-color)"

private def commonStyle(height: SizePx, direction: String): TagMod =
  TagMod(
    ^.backgroundImage    := s"linear-gradient(to $direction, $OpeningColor 0px, $OpeningColor ${height.render}, transparent ${height.render})",
    ^.transitionDuration := "0.2s"
  )

private def slideDownStyle(height: SizePx): TagMod =
  TagMod(^.paddingTop := height.render, commonStyle(height, "bottom"))

private def slideUpStyle(height: SizePx): TagMod =
  TagMod(^.paddingBottom := height.render, commonStyle(height, "top"))

def dragOverStyle(height: SizePx, edge: Edge): TagMod =
  edge match
    case Edge.Top    => slideDownStyle(height)
    case Edge.Bottom => slideUpStyle(height)
    case _           => TagMod.empty

def computeIndexInList[A](nextTo: A => Boolean, position: Edge)(list: List[A]): Option[Int] =
  list.zipWithIndex
    .collectFirst { case (a, idx) if nextTo(a) => idx }
    .map: idx =>
      position match
        case Edge.Top | Edge.Left     => idx
        case Edge.Bottom | Edge.Right => idx + 1

@targetName("computeIndexInListByOperation")
def computeIndexInList[A](nextTo: A => Boolean, operation: Operation)(list: List[A]): Option[Int] =
  list.zipWithIndex
    .collectFirst { case (a, idx) if nextTo(a) => idx }
    .flatMap: idx =>
      println(s"Found element at index: $idx, Operation: $operation") // DEBUG
      operation match
        case Operation.ReorderBefore => idx.some
        case Operation.ReorderAfter  => (idx + 1).some
        case Operation.Combine       => none

def insertIntoList[A](elem: A, nextTo: A => Boolean, position: Edge): Endo[List[A]] =
  list =>
    computeIndexInList(nextTo, position)(list).fold(list): idx =>
      val (before, after) = list.splitAt(idx)
      before ++ (elem :: after)
