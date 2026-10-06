// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import navigate.model.enums.LightSink

case class TcsConfig(
  sourceATarget:       Target,
  instrumentSpecifics: InstrumentSpecifics,
  pwfs1:               Option[GuiderConfig],
  pwfs2:               Option[GuiderConfig],
  oiwfs:               Option[GuiderConfig],
  rotatorTrackConfig:  RotatorTrackConfig,
  instrumentVariant:   LightSink,
  baffles:             Option[BafflesConfig]
) derives Show:
  def bafflesState: Option[BafflesState] =
    (baffles, sourceATarget.wavelength).mapN: (b, w) =>
      val instrument = instrumentVariant.instrument
      BafflesState(b.central(w, instrument), b.deployable(w, instrument))
