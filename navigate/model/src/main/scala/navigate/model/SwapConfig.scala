// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import lucuma.core.enums.Instrument

case class SwapConfig(
  guideTarget:        Target,
  acSpecifics:        InstrumentSpecifics,
  rotatorTrackConfig: RotatorTrackConfig
) derives Show {
  lazy val toTcsConfig: TcsConfig = TcsConfig(
    guideTarget,
    acSpecifics,
    None,
    None,
    None,
    rotatorTrackConfig,
    Instrument.AcqCam,
    None
  )
}
