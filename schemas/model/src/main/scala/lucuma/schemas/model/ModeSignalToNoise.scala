// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsFilter

enum ModeSignalToNoise derives Eq:
  case Undefined
  case Spectroscopy(acquisition: ItcResultValues, science: ItcResultValues)
  case GmosNorthImaging(science: Map[GmosNorthFilter, ItcResultValues])
  case GmosSouthImaging(science: Map[GmosSouthFilter, ItcResultValues])
  case Flamingos2Imaging(science: Map[Flamingos2Filter, ItcResultValues])
  case GnirsImaging(science: Map[GnirsFilter, ItcResultValues])
  case GhostIfu(red: ItcResultValues, blue: ItcResultValues)
