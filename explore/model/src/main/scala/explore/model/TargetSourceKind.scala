// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.Enumerated

enum TargetSourceKind(val tag: String) derives Enumerated:
  case Program  extends TargetSourceKind("program")
  case Simbad   extends TargetSourceKind("simbad")
  case Horizons extends TargetSourceKind("horizons")
