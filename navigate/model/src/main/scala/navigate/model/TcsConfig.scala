// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import lucuma.core.enums.Instrument

case class TcsConfig(
  sourceATarget:       Target,
  instrumentSpecifics: InstrumentSpecifics,
  pwfs1:               Option[GuiderConfig],
  pwfs2:               Option[GuiderConfig],
  oiwfs:               Option[GuiderConfig],
  rotatorTrackConfig:  RotatorTrackConfig,
  instrument:          Instrument,
  baffles:             Option[BafflesConfig]
) derives Show
