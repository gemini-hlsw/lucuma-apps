// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.util.NewBoolean
import lucuma.react.table.ColumnFilters
import lucuma.react.table.ColumnId

object IsAudioActivated extends NewBoolean
type IsAudioActivated = IsAudioActivated.Type

given Eq[ColumnFilters] = Eq.instance((a, b) => a.value.toSet === b.value.toSet)

given Encoder[ColumnFilters] =
  Encoder[Map[String, String]].contramap(_.value.map { case (k, v) => k.value -> v.toString })

given Decoder[ColumnFilters] =
  Decoder[Map[String, String]].map: m =>
    val entries: Map[ColumnId, Any] = m.map { case (k, v) => ColumnId(k) -> v }
    ColumnFilters(entries)
