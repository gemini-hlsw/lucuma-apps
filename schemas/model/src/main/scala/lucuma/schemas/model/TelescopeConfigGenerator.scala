// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.geom.OffsetGenerator
import lucuma.core.model.sequence.TelescopeConfig

enum TelescopeConfigGenerator derives Eq:
  case Enumerated(values: NonEmptyList[TelescopeConfig])     extends TelescopeConfigGenerator
  case FromOffsetGenerator(offsetGenerator: OffsetGenerator) extends TelescopeConfigGenerator
