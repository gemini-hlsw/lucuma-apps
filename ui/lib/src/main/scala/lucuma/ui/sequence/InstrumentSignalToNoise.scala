// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN

enum InstrumentSignalToNoise derives Eq:
  case Undefined
  case Spectroscopy(acquisition: Option[(SingleSN, TotalSN)], science: Option[(SingleSN, TotalSN)])
  case GmosNorthImaging(science: Map[GmosNorthFilter, (SingleSN, TotalSN)])
  case GmosSouthImaging(science: Map[GmosSouthFilter, (SingleSN, TotalSN)])
