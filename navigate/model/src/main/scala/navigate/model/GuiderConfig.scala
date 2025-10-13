// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import navigate.model.Target

case class GuiderConfig(
  target:   Target,
  tracking: TrackingConfig
) derives Show
