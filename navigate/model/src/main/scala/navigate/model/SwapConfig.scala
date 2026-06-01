// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import mouse.boolean.given
import lucuma.core.enums.Site
import navigate.model.enums.LightSink

case class SwapConfig(
  guideTarget:        Target,
  acSpecifics:        InstrumentSpecifics,
  rotatorTrackConfig: RotatorTrackConfig
) derives Show {
  def toTcsConfig(site: Site): TcsConfig = TcsConfig(
    guideTarget,
    acSpecifics,
    None,
    None,
    None,
    rotatorTrackConfig,
    (site === Site.GS).fold(LightSink.AcqCamSouth, LightSink.AcqCamNorth),
    None
  )
}
