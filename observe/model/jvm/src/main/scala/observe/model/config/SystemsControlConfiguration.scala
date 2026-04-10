// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Instrument
import observe.model.Server
import observe.model.SubsystemOrServer
import observe.model.enums.ControlStrategy
import observe.model.enums.Resource

/**
 * Indicates how each subsystems is treated, e.g. full connection or simulated
 */
case class SystemsControlConfiguration(
  altair:     ControlStrategy,
  gems:       ControlStrategy,
  dhs:        ControlStrategy,
  flamingos2: ControlStrategy,
  gcal:       ControlStrategy,
  gmos:       ControlStrategy,
  gnirs:      ControlStrategy,
  gpi:        ControlStrategy,
  gpiGds:     ControlStrategy,
  ghost:      ControlStrategy,
  ghostGds:   ControlStrategy,
  igrins2:    ControlStrategy,
  igrins2Gds: ControlStrategy,
  gsaoi:      ControlStrategy,
  gws:        ControlStrategy,
  nifs:       ControlStrategy,
  niri:       ControlStrategy,
  tcs:        ControlStrategy
) derives Eq:
  def connectEpics: Boolean =
    altair.connect || gems.connect || flamingos2.connect || gcal.connect || gmos.connect || gnirs.connect || gsaoi.connect || gws.connect || nifs.connect || niri.connect || tcs.connect

  def toMap: Map[SubsystemOrServer, ControlStrategy] =
    Map(
      Resource.Altair       -> altair,
      Resource.Gems         -> gems,
      Instrument.Flamingos2 -> flamingos2,
      Resource.Gcal         -> gcal,
      Instrument.GmosNorth  -> gmos,
      Instrument.GmosSouth  -> gmos,
      Instrument.Gnirs      -> gnirs,
      Instrument.Ghost      -> ghost,
      Instrument.Igrins2    -> igrins2,
      Instrument.Gsaoi      -> gsaoi,
      Instrument.Niri       -> niri,
      Resource.TCS          -> tcs,
      Server.Dhs            -> dhs
    )
