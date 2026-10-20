// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Instrument
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.SubsystemControlMode

/**
 * Indicates how each subsystems is treated, e.g. full connection or simulated
 */
case class SubsystemControlModes(
  altair:     SubsystemControlMode,
  gems:       SubsystemControlMode,
  dhs:        SubsystemControlMode,
  flamingos2: SubsystemControlMode,
  gcal:       SubsystemControlMode,
  gmos:       SubsystemControlMode,
  gnirs:      SubsystemControlMode,
  gpi:        SubsystemControlMode,
  ghost:      SubsystemControlMode,
  igrins2:    SubsystemControlMode,
  gsaoi:      SubsystemControlMode,
  gws:        SubsystemControlMode,
  nifs:       SubsystemControlMode,
  niri:       SubsystemControlMode,
  tcs:        SubsystemControlMode
) derives Eq:
  def connectEpics: Boolean =
    altair.connect || gems.connect || flamingos2.connect || gcal.connect || gmos.connect || gnirs.connect || gsaoi.connect || gws.connect || nifs.connect || niri.connect || tcs.connect

  def toMap: Map[Subsystem, SubsystemControlMode] =
    Map(
      ObservatorySubsystem.Altair -> altair,
      ObservatorySubsystem.Gems   -> gems,
      Instrument.Flamingos2       -> flamingos2,
      ObservatorySubsystem.Gcal   -> gcal,
      Instrument.GmosNorth        -> gmos,
      Instrument.GmosSouth        -> gmos,
      Instrument.Gnirs            -> gnirs,
      Instrument.Ghost            -> ghost,
      Instrument.Igrins2          -> igrins2,
      Instrument.Gsaoi            -> gsaoi,
      Instrument.Niri             -> niri,
      ObservatorySubsystem.Tcs    -> tcs,
      ObservatorySubsystem.Dhs    -> dhs,
      ObservatorySubsystem.Gws    -> gws
    )
