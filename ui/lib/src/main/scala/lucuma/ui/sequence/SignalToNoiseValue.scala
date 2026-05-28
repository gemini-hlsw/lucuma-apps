// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import lucuma.core.math.SignalToNoise

enum SignalToNoiseValue derives Eq:
  case Value(sn: SignalToNoise)
  case Saturated
