// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/** An Observe subsystem that is not an instrument. It can be used by only one single agent. */
enum ObservatorySubsystem(val tag: String, val name: String) derives Enumerated, Display:
  case Tcs    extends ObservatorySubsystem("Tcs", "TCS")
  case Gcal   extends ObservatorySubsystem("Gcal", "GCAL")
  case Altair extends ObservatorySubsystem("Altair", "Altair")
  case Gems   extends ObservatorySubsystem("Gems", "GeMS")
  case Dhs    extends ObservatorySubsystem("Dhs", "DHS")
  case Gws    extends ObservatorySubsystem("Gws", "GWS")
