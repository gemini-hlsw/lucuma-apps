// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.util.Enumerated

/**
 * The preset values a setting offers, keyed by what the editor shows for each one. Selecting a
 * preset is a supported choice, so a value found here is never a customization. The head is the
 * value the setting is created with.
 */
opaque type PresetList[K, V] = NonEmptyList[(K, V)]

object PresetList:
  def apply[K, V](presets: NonEmptyList[(K, V)]): PresetList[K, V] = presets

  /** One preset per enum case, in enumeration order. */
  def fromEnumerated[K: Enumerated, V](value: K => V): PresetList[K, V] =
    // An enumeration always has at least one case.
    NonEmptyList.fromListUnsafe(Enumerated[K].all).map(k => k -> value(k))

  extension [K, V](presets: PresetList[K, V])
    def keys: NonEmptyList[K] = presets.map(_._1)

    def default: V = presets.head._2

    def value(key: K)(using Eq[K]): Option[V] = presets.find(_._1 === key).map(_._2)

    def keyOf(value: V)(using Eq[V]): Option[K] = presets.find(_._2 === value).map(_._1)

    def contains(value: V)(using Eq[V]): Boolean = presets.exists(_._2 === value)

    /**
     * An explicit value customizes the setting only when it is neither the default nor a preset.
     * The default is normally the head of the list, so the first check is a guard for the case
     * where the server's default has drifted from the presets.
     */
    def isCustomized(explicitValue: Option[V], defaultValue: V)(using Eq[V]): Boolean =
      explicitValue.exists(v => v =!= defaultValue && !presets.exists(_._2 === v))
