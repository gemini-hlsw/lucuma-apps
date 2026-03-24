// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import explore.modes.ItcInstrumentConfig
import lucuma.core.model.ConstraintSet
import lucuma.core.util.Timestamp

case class ItcRequestParams(
  constraints:         ConstraintSet,
  asterism:            NonEmptyList[ItcTarget],
  customSedTimestamps: List[Timestamp],
  mode:                ItcInstrumentConfig
)

case class ItcGraphRequestParams(
  constraints:         ConstraintSet,
  asterism:            NonEmptyList[ItcTarget],
  customSedTimestamps: List[Timestamp],
  mode:                ItcInstrumentConfig
)
