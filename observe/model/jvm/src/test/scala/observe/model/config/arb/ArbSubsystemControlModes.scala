// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config.arb

import lucuma.core.util.arb.ArbEnumerated.given
import observe.model.config.*
import observe.model.config.SubsystemControlModes
import observe.model.enums.SubsystemControlMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSubsystemControlModes {

  given Arbitrary[SubsystemControlModes] =
    Arbitrary {
      for {
        altair  <- arbitrary[SubsystemControlMode]
        gems    <- arbitrary[SubsystemControlMode]
        dhs     <- arbitrary[SubsystemControlMode]
        f2      <- arbitrary[SubsystemControlMode]
        gcal    <- arbitrary[SubsystemControlMode]
        gmos    <- arbitrary[SubsystemControlMode]
        gnirs   <- arbitrary[SubsystemControlMode]
        gpi     <- arbitrary[SubsystemControlMode]
        ghost   <- arbitrary[SubsystemControlMode]
        igrins2 <- arbitrary[SubsystemControlMode]
        gsaoi   <- arbitrary[SubsystemControlMode]
        gws     <- arbitrary[SubsystemControlMode]
        nifs    <- arbitrary[SubsystemControlMode]
        niri    <- arbitrary[SubsystemControlMode]
        tcs     <- arbitrary[SubsystemControlMode]
      } yield SubsystemControlModes(altair,
                                    gems,
                                    dhs,
                                    f2,
                                    gcal,
                                    gmos,
                                    gnirs,
                                    gpi,
                                    ghost,
                                    igrins2,
                                    gsaoi,
                                    gws,
                                    nifs,
                                    niri,
                                    tcs
      )
    }

  given Cogen[SubsystemControlModes] =
    Cogen[
      (
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode,
        SubsystemControlMode
      )
    ].contramap(x =>
      (x.altair,
       x.gems,
       x.dhs,
       x.flamingos2,
       x.gcal,
       x.gmos,
       x.gnirs,
       x.gpi,
       x.ghost,
       x.gsaoi,
       x.gws,
       x.nifs,
       x.niri,
       x.tcs
      )
    )

}

object ArbSubsystemControlModes extends ArbSubsystemControlModes
