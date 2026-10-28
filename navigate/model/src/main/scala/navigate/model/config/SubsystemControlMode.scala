// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.config

import cats.syntax.all.*
import lucuma.core.util.Enumerated

enum SubsystemControlMode(val tag: String) extends Product with Serializable derives Enumerated {
  // Subsystem will be fully controlled by Navigate
  case Full      extends SubsystemControlMode("full")
  // Navigate connects to subsystem, but only to read values
  case ReadOnly  extends SubsystemControlMode("readOnly")
  // All subsystem interactions are internally simulated
  case Simulated extends SubsystemControlMode("simulated")
}

object SubsystemControlMode {

  def fromString(v: String): Option[SubsystemControlMode] = v match {
    case "full"      => Some(Full)
    case "readOnly"  => Some(ReadOnly)
    case "simulated" => Some(Simulated)
    case _           => None
  }

  extension (v: SubsystemControlMode) {
    def connect: Boolean      = v =!= SubsystemControlMode.Simulated
    // If connected, then use real values for keywords
    def realKeywords: Boolean = connect
    def command: Boolean      = v === SubsystemControlMode.Full
  }

}
