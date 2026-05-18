// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.enums.ScienceMode
import lucuma.core.util.Enumerated

enum ConfigurationKind(val tag: String) derives Enumerated:
  case Spectroscopy extends ConfigurationKind("spectroscopy")
  case Imaging      extends ConfigurationKind("imaging")
  case Visitor      extends ConfigurationKind("visitor")

object ConfigurationKind:
  def fromScienceMode(m: ScienceMode): ConfigurationKind = m match
    case ScienceMode.Spectroscopy => ConfigurationKind.Spectroscopy
    case ScienceMode.Imaging      => ConfigurationKind.Imaging
