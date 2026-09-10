// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
      operation match
        case Operation.ReorderBefore => idx.some
        case Operation.ReorderAfter  => (idx + 1).some
        case Operation.Combine       => none

def insertIntoList[A](elem: A, nextTo: A => Boolean, position: Edge): Endo[List[A]] =
  list =>
    computeIndexInList(nextTo, position)(list).fold(list): idx =>
      val (before, after) = list.splitAt(idx)
      before ++ (elem :: after)

/**
 * Moves the element at `from` to the `position` side of the element at `nextTo`. Both are positions
 * in the original list.
 *
 * The position-based counterpart of [[insertIntoList]], for lists whose elements are not unique --
 * where a predicate on the element cannot identify a single row. Indices are zipped on before the
 * source is removed, so there is no index-shift arithmetic to get wrong.
 *
 * Returns the list unchanged when the two positions are the same, or when either is out of range --
 * a drag whose endpoints cannot be located moves nothing.
 */
def moveInList[A](from: Int, nextTo: Int, position: Edge): Endo[List[A]] =
  list =>
    // Both guards protect against losing the dragged element.  `insertIntoList` returns
    // its input unchanged when it cannot locate the target, and the input here is the list
    // with the source *already removed*, so anything that stops the target being found --
    // the element's own position, or an index that isn't in the list, which a stale drag
    // can produce -- would otherwise drop the element altogether.  Past these guards the
    // source-removed list still holds `nextTo`, so the predicate always matches.
    if from == nextTo || !list.indices.contains(nextTo) then list
    else
      list
        .lift(from)
        .fold(list): elem =>
          insertIntoList[(A, Int)](
            (elem, from),
            (p: (A, Int)) => p._2 == nextTo,
            position
          )(list.zipWithIndex.filterNot(_._2 == from)).map(_._1)
