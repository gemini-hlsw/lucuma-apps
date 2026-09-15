// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import explore.model.layout.*
import lucuma.react.gridlayout.*
import munit.FunSuite

class ObsLayoutMergeSuite extends FunSuite:

  // The layout a user who last saved before the details tile existed would have stored.
  private val storedBeforeDetails: Layout =
    Layout(
      List(
        LayoutItem(x = 0, y = 0, w = 16, h = 5, i = ObsTabTileIds.NotesId.id.value),
        LayoutItem(x = 0, y = 5, w = 16, h = 18, i = ObsTabTileIds.TargetId.id.value),
        LayoutItem(x = 0, y = 23, w = 16, h = 9, i = ObsTabTileIds.FinderChartsId.id.value),
        LayoutItem(x = 0, y = 32, w = 16, h = 9, i = ObsTabTileIds.PlotId.id.value),
        LayoutItem(x = 0, y = 41, w = 16, h = 7, i = ObsTabTileIds.ConstraintsId.id.value),
        LayoutItem(x = 0, y = 48, w = 16, h = 12, i = ObsTabTileIds.TimingWindowsId.id.value),
        LayoutItem(x = 0, y = 60, w = 16, h = 10, i = ObsTabTileIds.ConfigurationId.id.value),
        LayoutItem(x = 0, y = 70, w = 16, h = 9, i = ObsTabTileIds.ItcId.id.value)
      )
    )

  private def merged(stored: Layout): List[LayoutItem] =
    val default = ExploreGridLayouts.observations.defaultObsLayouts(BreakpointName.md)._3
    mergeLayouts(default, stored).asList.sortBy(_.y)

  test("the details tile lands directly below notes for a pre-existing layout"):
    val items = merged(storedBeforeDetails)
    val ids   = items.map(_.i)
    assertEquals(ids.head, ObsTabTileIds.NotesId.id.value)
    assertEquals(ids(1), ObsTabTileIds.DetailsId.id.value, clue = items.map(i => (i.i, i.y)))

  test("the details tile lands below notes even when notes was dragged down"):
    val moved  =
      Layout(storedBeforeDetails.asList.map {
        case i if i.i === ObsTabTileIds.NotesId.id.value => i.copy(y = 79)
        case i                                           => i.copy(y = i.y - 5)
      })
    val items  = merged(moved)
    val notesY = items.find(_.i === ObsTabTileIds.NotesId.id.value).map(_.y)
    val detY   = items.find(_.i === ObsTabTileIds.DetailsId.id.value).map(_.y)
    assertEquals(detY, notesY.map(_ + 5), clue = items.map(i => (i.i, i.y)))

  test("the merged layout has no overlapping tiles"):
    val items    = merged(storedBeforeDetails)
    val overlaps =
      items.tails.toList
        .collect { case a :: rest => rest.map((a, _)) }
        .flatten
        .filter: (a, b) =>
          a.y < b.y + b.h && b.y < a.y + a.h
    assertEquals(overlaps.map((a, b) => (a.i, b.i)), Nil, clue = items.map(i => (i.i, i.y, i.h)))

  // The notes tile is hidden before proposal review, and a hidden tile is never persisted.
  test("the details tile still lands at the top when notes was never stored"):
    val withoutNotes = Layout(
      storedBeforeDetails.asList.filterNot(_.i === ObsTabTileIds.NotesId.id.value)
    )
    val items        = merged(withoutNotes)
    val detY         = items.find(_.i === ObsTabTileIds.DetailsId.id.value).map(_.y)
    val targetY      = items.find(_.i === ObsTabTileIds.TargetId.id.value).map(_.y)
    assertEquals(detY, Some(5), clue = items.map(i => (i.i, i.y, i.h)))
    assertEquals(targetY, Some(9), clue = items.map(i => (i.i, i.y, i.h)))

  // Settling must not disturb a layout the user already arranged, gaps included.
  test("a stored layout that knows every tile is returned untouched"):
    val complete =
      Layout(
        storedBeforeDetails.asList.map(i => i.copy(y = i.y + 4)) :+
          LayoutItem(x = 0, y = 0, w = 16, h = 4, i = ObsTabTileIds.DetailsId.id.value)
      )
    val items    = merged(complete)
    assertEquals(
      items.map(i => (i.i, i.y)).toSet,
      complete.asList.map(i => (i.i, i.y)).toSet,
      clue = items.map(i => (i.i, i.y, i.h))
    )

  // A user who reordered the whole tab: the tile follows its neighbour, nothing is displaced.
  test("a scrambled stored order keeps the details tile under notes"):
    val order     = List(
      ObsTabTileIds.ConfigurationId,
      ObsTabTileIds.ItcId,
      ObsTabTileIds.TargetId,
      ObsTabTileIds.NotesId,
      ObsTabTileIds.ConstraintsId,
      ObsTabTileIds.PlotId,
      ObsTabTileIds.FinderChartsId,
      ObsTabTileIds.TimingWindowsId
    ).map(_.id.value)
    val byId      = storedBeforeDetails.asList.map(i => i.i -> i).toMap
    val scrambled = Layout(
      order
        .foldLeft((List.empty[LayoutItem], 0)): (acc, id) =>
          val item = byId(id)
          (item.copy(y = acc._2) :: acc._1, acc._2 + item.h)
        ._1
        .reverse
    )
    val items     = merged(scrambled)
    val notes     = items.find(_.i === ObsTabTileIds.NotesId.id.value).get
    val details   = items.find(_.i === ObsTabTileIds.DetailsId.id.value).get
    assertEquals(details.y, notes.y + notes.h, clue = items.map(i => (i.i, i.y, i.h)))
    // the scrambled order itself survives, everything below notes just shifts down by 4
    assertEquals(
      items.sortBy(_.y).map(_.i).filterNot(_ === ObsTabTileIds.DetailsId.id.value),
      order,
      clue = items.map(i => (i.i, i.y, i.h))
    )
