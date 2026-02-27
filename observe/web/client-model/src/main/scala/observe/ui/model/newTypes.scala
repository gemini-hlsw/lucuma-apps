// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import lucuma.core.util.NewBoolean
import cats.Eq
import cats.syntax.eq.*
import lucuma.react.table.ColumnFilters

object IsAudioActivated extends NewBoolean
type IsAudioActivated = IsAudioActivated.Type

given Eq[ColumnFilters] = Eq.instance((a, b) => a.value.toSet === b.value.toSet)
