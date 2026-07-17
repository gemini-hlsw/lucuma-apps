// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.enums.ScienceMode
import lucuma.core.util.Enumerated

enum ConfigurationMode(val tag: String) derives Enumerated:
  case Spectroscopy extends ConfigurationMode("spectroscopy")
  case Imaging      extends ConfigurationMode("imaging")
  case Visitor      extends ConfigurationMode("visitor")
  case Keck         extends ConfigurationMode("keck")
  case Subaru       extends ConfigurationMode("subaru")

object ConfigurationMode:
  def fromScienceMode(m: ScienceMode): ConfigurationMode = m match
    case ScienceMode.Spectroscopy => ConfigurationMode.Spectroscopy
    case ScienceMode.Imaging      => ConfigurationMode.Imaging
