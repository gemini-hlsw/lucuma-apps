// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import lucuma.core.math.Coordinates
import navigate.model.FocalPlaneOffset

case class TargetData(
  coordinates:  Coordinates,
  adjOffset:    FocalPlaneOffset,
  origin:       FocalPlaneOffset,
  originOffset: FocalPlaneOffset
)
