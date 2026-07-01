// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig

object Presets:
  val GnirsIfuLr: List[(String, NonEmptyList[TelescopeConfig])] =
    List(
      "Extended" -> NonEmptyList.of(
        TelescopeConfig(Offset(0.15.pArcsec, 0.15.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(10.pArcsec, 10.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(-0.15.pArcsec, -0.15.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(-10.pArcsec, -10.qArcsec), StepGuideState.Disabled)
      ),
      "Point"    -> NonEmptyList.of(
        TelescopeConfig(Offset(0.75.pArcsec, -1.5.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(-0.75.pArcsec, -1.5.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(-0.75.pArcsec, 1.5.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(0.75.pArcsec, -1.5.qArcsec), StepGuideState.Enabled)
      )
    )

  val GnirsIfuHr: List[(String, NonEmptyList[TelescopeConfig])] =
    List(
      "Science"  -> NonEmptyList.of(
        TelescopeConfig(Offset(0.1.pArcsec, -0.1.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(10.pArcsec, 10.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(-0.1.pArcsec, 0.1.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(-10.pArcsec, -10.qArcsec), StepGuideState.Disabled)
      ),
      "Telluric" -> NonEmptyList.of(
        TelescopeConfig(Offset(0.pArcsec, 0.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(5.pArcsec, 5.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec, 0.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(0.pArcsec, 0.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(-5.pArcsec, -5.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec, 0.qArcsec), StepGuideState.Enabled)
      )
    )
