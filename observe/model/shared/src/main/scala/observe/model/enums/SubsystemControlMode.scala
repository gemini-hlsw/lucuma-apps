// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated

enum SubsystemControlMode(val tag: String) derives Enumerated:

  // Subsystem will be fully controlled by Observe
  case Full      extends SubsystemControlMode("full")
  // Observe connects to subsystem, but only to read values
  case ReadOnly  extends SubsystemControlMode("readOnly")
  // All subsystem interactions are internally simulated
  case Simulated extends SubsystemControlMode("simulated")

object SubsystemControlMode:
  extension (v: SubsystemControlMode)
    inline def connect: Boolean      = v =!= SubsystemControlMode.Simulated
    // If connected, then use real values for keywords
    inline def realKeywords: Boolean = connect
    inline def command: Boolean      = v === SubsystemControlMode.Full
