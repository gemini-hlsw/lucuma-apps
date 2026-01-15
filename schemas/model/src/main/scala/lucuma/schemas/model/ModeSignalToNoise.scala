// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.itc.SignalToNoiseAt

enum ModeSignalToNoise derives Eq:
  case Undefined
  case Spectroscopy(acquisition: Option[SignalToNoiseAt], science: Option[SignalToNoiseAt])
  case GmosNorthImaging(science: Map[GmosNorthFilter, SignalToNoiseAt])
  case GmosSouthImaging(science: Map[GmosSouthFilter, SignalToNoiseAt])
