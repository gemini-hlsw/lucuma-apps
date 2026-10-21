// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.config

import cats.*
import cats.derived.*

/**
 * Indicates how each subsystems is treated, e.g. full connection or simulated
 */
case class SubsystemControlModes(
  altair:  SubsystemControlMode,
  gems:    SubsystemControlMode,
  gcal:    SubsystemControlMode,
  gpi:     SubsystemControlMode,
  gsaoi:   SubsystemControlMode,
  tcs:     SubsystemControlMode,
  observe: SubsystemControlMode
) derives Eq {
  def connectEpics: Boolean =
    altair.connect || gems.connect || gcal.connect || tcs.connect
}

object SubsystemControlModes {
  val default: SubsystemControlModes = SubsystemControlModes(
    altair = SubsystemControlMode.Simulated,
    gems = SubsystemControlMode.Simulated,
    gcal = SubsystemControlMode.Simulated,
    gpi = SubsystemControlMode.Simulated,
    gsaoi = SubsystemControlMode.Simulated,
    tcs = SubsystemControlMode.Simulated,
    observe = SubsystemControlMode.Simulated
  )
}
