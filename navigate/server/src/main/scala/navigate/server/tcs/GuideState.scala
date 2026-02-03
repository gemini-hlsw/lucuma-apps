// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.MountGuideOption
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.ProbeGuide

case class GuideState(
  mountOffload:  MountGuideOption,
  m1Guide:       M1GuideConfig,
  m2Guide:       M2GuideConfig,
  probeGuide:    Option[ProbeGuide],
  p1Integrating: Boolean,
  p2Integrating: Boolean,
  oiIntegrating: Boolean,
  acIntegrating: Boolean
) derives Eq

object GuideState {
  lazy val default: GuideState = GuideState(
    MountGuideOption.MountGuideOff,
    M1GuideConfig.M1GuideOff,
    M2GuideConfig.M2GuideOff,
    None,
    false,
    false,
    false,
    false
  )
}
