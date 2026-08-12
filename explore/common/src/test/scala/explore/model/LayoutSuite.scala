// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enums.GridLayoutSection
import explore.model.layout.*
import lucuma.react.gridlayout.*
import munit.FunSuite

import scala.collection.immutable.SortedMap

class LayoutSuite extends FunSuite {

  val observationMdXLayoutItem =
    LayoutItem(i = "X", x = 0, y = 0, w = 8, h = 5, minH = 3, maxH = 10, minW = 2, maxW = 12)

  val dbObservationMdXLayoutItem =
    LayoutItem(i = "X", x = 1, y = 2, w = 7, h = 9, minH = 0, maxH = 15, minW = 5, maxW = 11)

  val expectedObservationMdXLayoutItem =
    observationMdXLayoutItem.copy(x = 1, y = 2, w = 7, h = 9)

  val observationMdLayout = Layout(
    List[LayoutItem](
      observationMdXLayoutItem,
      LayoutItem(i = "Y", x = 5, y = 9, w = 7, h = 8, minH = 6, maxH = 9, minW = 4, maxW = 11),
      LayoutItem(i = "Z", x = 9, y = 15, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val dbObservationMdLayout = Layout(
    List[LayoutItem](
      dbObservationMdXLayoutItem,
      LayoutItem(i = "Y", x = 3, y = 2, w = 8, h = 6, minH = 5, maxH = 11, minW = 3, maxW = 12),
      LayoutItem(i = "Q", x = 7, y = 14, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val expectedObservationMdLayout = Layout(
    List[LayoutItem](
      expectedObservationMdXLayoutItem,
      LayoutItem(i = "Y", x = 3, y = 2, w = 8, h = 6, minH = 6, maxH = 9, minW = 4, maxW = 11),
      LayoutItem(i = "Z", x = 9, y = 15, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val originalMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md -> (8, 10, observationMdLayout),
      BreakpointName.lg -> (7,
                            11,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "X",
                                           x = 2,
                                           y = 1,
                                           w = 6,
                                           h = 6,
                                           minH = 5,
                                           maxH = 11,
                                           minW = 3,
                                           maxW = 10
                                )
                              )
                            )
      )
    ),
    GridLayoutSection.ConstraintsLayout  -> SortedMap(
      BreakpointName.md -> (9,
                            13,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "Z",
                                           x = 0,
                                           y = 0,
                                           w = 8,
                                           h = 5,
                                           minH = 3,
                                           maxH = 10,
                                           minW = 2,
                                           maxW = 12
                                )
                              )
                            )
      )
    )
  )

  val dbMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md  -> (9, 20, dbObservationMdLayout),
      BreakpointName.xxs -> (7,
                             12,
                             Layout(
                               List[LayoutItem](
                                 LayoutItem(i = "X",
                                            x = 5,
                                            y = 1,
                                            w = 6,
                                            h = 6,
                                            minH = 5,
                                            maxH = 11,
                                            minW = 3,
                                            maxW = 10
                                 )
                               )
                             )
      )
    ),
    GridLayoutSection.SchedulingLayout   -> SortedMap(
      BreakpointName.md -> (
        8,
        10,
        Layout(
          List[LayoutItem](
            LayoutItem(i = "Y", x = 5, y = 9, w = 9, h = 7, minH = 4, maxH = 9, minW = 4, maxW = 11)
          )
        )
      )
    )
  )

  val expectedMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md -> (8, 10, expectedObservationMdLayout),
      BreakpointName.lg -> (7,
                            11,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "X",
                                           x = 2,
                                           y = 1,
                                           w = 6,
                                           h = 6,
                                           minH = 5,
                                           maxH = 11,
                                           minW = 3,
                                           maxW = 10
                                )
                              )
                            )
      )
    ),
    GridLayoutSection.ConstraintsLayout  -> SortedMap(
      BreakpointName.md -> (9,
                            13,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "Z",
                                           x = 0,
                                           y = 0,
                                           w = 8,
                                           h = 5,
                                           minH = 3,
                                           maxH = 10,
                                           minW = 2,
                                           maxW = 12
                                )
                              )
                            )
      )
    )
  )

  test("LayoutItems merge correctly") {
    val mergedLayoutItem = mergeLayoutItems(observationMdXLayoutItem, dbObservationMdXLayoutItem)
    assertEquals(mergedLayoutItem, expectedObservationMdXLayoutItem)
  }

  test("Layouts merge correctly") {
    val mergedLayout = mergeLayouts(observationMdLayout, dbObservationMdLayout)
    assertEquals(mergedLayout, expectedObservationMdLayout)
  }

  test("SectionLayoutsMaps merge correctly") {
    val mergedMap = mergeSectionLayoutsMaps(originalMap, dbMap)
    assertEquals(mergedMap, expectedMap)
  }

  // A tile that disappears from the grid and comes back can be stored with the 1x1 size
  // react-grid-layout invents for items it doesn't know about. It must not come back collapsed.
  test("A stored width below minW falls back to the default width") {
    val collapsed = dbObservationMdXLayoutItem.copy(w = 1, h = 1)

    val mergedLayoutItem = mergeLayoutItems(observationMdXLayoutItem, collapsed)

    assertEquals(mergedLayoutItem.w, observationMdXLayoutItem.w)
    assertEquals(mergedLayoutItem.h, 1) // minimized tiles are a legit stored state
  }

  test("A stored width below minW falls back to the default width when merging layouts") {
    val collapsedDbLayout =
      Layout(dbObservationMdLayout.asList.map(i => i.copy(w = 1)))

    val mergedLayout = mergeLayouts(observationMdLayout, collapsedDbLayout)

    assertEquals(
      mergedLayout.asList.map(i => (i.i, i.w)),
      // X and Y go back to their default widths, Z is not in the db layout
      List(("X", 8), ("Y", 7), ("Z", 8))
    )
  }

  test("A stored width is kept when there is no minW") {
    val noMinW = LayoutItem(i = "X", x = 0, y = 0, w = 8, h = 5)

    val mergedLayoutItem = mergeLayoutItems(noMinW, noMinW.copy(w = 1))

    assertEquals(mergedLayoutItem.w, 1)
  }

  test("A stored width at or above minW is kept") {
    val atMinW = dbObservationMdXLayoutItem.copy(w = observationMdXLayoutItem.minW.get)

    assertEquals(mergeLayoutItems(observationMdXLayoutItem, atMinW).w, atMinW.w)
    assertEquals(mergeLayoutItems(observationMdXLayoutItem, dbObservationMdXLayoutItem).w, 7)
  }

  // resolveAutoHeight
  // rowHeight = 36, rowPadding = 5, so a row span of `h` renders as `41h - 5` px.
  // 1 row = 36px, 2 rows = 77px, 3 rows = 118px, 4 rows = 159px, 5 rows = 200px.

  val autoItem = LayoutItem(i = "Auto", x = 0, y = 0, w = 8, h = 4)

  val LargeViewport = 10000

  test("Quantization: content exactly at a row boundary needs exactly that many rows") {
    assertEquals(resolveAutoHeight(autoItem, 118, LargeViewport, false).h, 3)
  }

  test("Quantization: one pixel over a row boundary needs one more row") {
    assertEquals(resolveAutoHeight(autoItem, 119, LargeViewport, false).h, 4)
  }

  test("Quantization: one pixel under a row boundary still needs that many rows") {
    assertEquals(resolveAutoHeight(autoItem, 117, LargeViewport, false).h, 3)
  }

  test("Floor: content wanting fewer than two rows still yields two") {
    assertEquals(resolveAutoHeight(autoItem, 1, LargeViewport, false).h, AutoHeightMinRows)
    assertEquals(resolveAutoHeight(autoItem, 0, LargeViewport, false).h, AutoHeightMinRows)
  }

  test("Ceiling: content taller than the viewport is capped at the rows that fit") {
    // Viewport fits 3 rows (118px), content wants 5 (200px).
    assertEquals(resolveAutoHeight(autoItem, 200, 118, false).h, 3)
  }

  test("Ceiling: a viewport smaller than the floor still respects the floor") {
    assertEquals(resolveAutoHeight(autoItem, 200, 1, false).h, AutoHeightMinRows)
  }

  test("Deadband: a measurement matching the current row span leaves the item unchanged") {
    val stable = autoItem.copy(h = 3, minH = 3, maxH = 3)
    assertEquals(resolveAutoHeight(stable, 118, LargeViewport, false), stable)
  }

  test("Deadband: a measurement that crosses a row boundary updates the item") {
    val stable = autoItem.copy(h = 3, minH = 3, maxH = 3)
    assertEquals(resolveAutoHeight(stable, 119, LargeViewport, false).h, 4)
  }

  test("Pinning: minH and maxH are pinned to the derived row span") {
    val result = resolveAutoHeight(autoItem, 118, LargeViewport, false)
    assertEquals(result.minH.toOption, Some(3))
    assertEquals(result.maxH.toOption, Some(3))
  }

  test("Growth: a taller measurement increases h") {
    assertEquals(resolveAutoHeight(autoItem.copy(h = 3), 200, LargeViewport, false).h, 5)
  }

  test("Shrink: a shorter measurement decreases h") {
    assertEquals(resolveAutoHeight(autoItem.copy(h = 5), 118, LargeViewport, false).h, 3)
  }

  test("Minimize suppression: a minimized item is returned unchanged regardless of measurement") {
    val minimized = autoItem.copy(h = 1)
    assertEquals(resolveAutoHeight(minimized, 500, LargeViewport, true), minimized)
  }

  test("Non-height fields are preserved") {
    val result = resolveAutoHeight(autoItem, 500, LargeViewport, false)
    assertEquals(result.x, autoItem.x)
    assertEquals(result.y, autoItem.y)
    assertEquals(result.w, autoItem.w)
    assertEquals(result.i, autoItem.i)
  }
}
