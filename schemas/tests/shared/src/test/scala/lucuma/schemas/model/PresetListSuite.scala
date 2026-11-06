// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.enums.GmosSlitOffsetPreset
import lucuma.core.util.Enumerated
import munit.FunSuite

class PresetListSuite extends FunSuite:
  private val presets: PresetList[String, Int] =
    PresetList(NonEmptyList.of("one" -> 1, "two" -> 2))

  test("keys and default follow the list order"):
    assertEquals(presets.keys, NonEmptyList.of("one", "two"))
    assertEquals(presets.default, 1)

  test("value and keyOf look up both ways, None when absent"):
    assertEquals(presets.value("two"), 2.some)
    assertEquals(presets.value("three"), none)
    assertEquals(presets.keyOf(1), "one".some)
    assertEquals(presets.keyOf(3), none)

  test("contains matches preset values only"):
    assert(presets.contains(2))
    assert(!presets.contains(3))

  test("a preset or the default is not a customization"):
    assert(!presets.isCustomized(none, 1))
    assert(!presets.isCustomized(1.some, 1))
    assert(!presets.isCustomized(2.some, 1))

  test("a default outside the list is still not a customization"):
    assert(!presets.isCustomized(9.some, 9))

  test("a value matching no preset is a customization"):
    assert(presets.isCustomized(3.some, 1))

  test("fromEnumerated lists every case in order"):
    val list: PresetList[GmosSlitOffsetPreset, String] =
      PresetList.fromEnumerated[GmosSlitOffsetPreset, String](_.tag)
    assertEquals(list.keys.toList, Enumerated[GmosSlitOffsetPreset].all)
    assertEquals(list.default, Enumerated[GmosSlitOffsetPreset].all.head.tag)
