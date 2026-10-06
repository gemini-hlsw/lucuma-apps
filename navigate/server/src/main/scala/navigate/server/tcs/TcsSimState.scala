// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import monocle.Focus
import monocle.Lens
import navigate.model.AcMechsState
import navigate.model.AllWfsConfiguration
import navigate.model.BafflesState
import navigate.model.GuideState
import navigate.model.PwfsMechsState
import navigate.model.TelescopeState

/**
 * Whole state of the simulated TCS. Every readout the simulated controller offers is a view of this
 * value, and every command is a modification of it.
 */
case class TcsSimState(
  guide:      GuideState,
  telescope:  TelescopeState,
  acMechs:    AcMechsState,
  pwfs1Mechs: PwfsMechsState,
  pwfs2Mechs: PwfsMechsState,
  wfsConfigs: AllWfsConfiguration,
  baffles:    BafflesState
)

object TcsSimState:
  val guide: Lens[TcsSimState, GuideState]               = Focus[TcsSimState](_.guide)
  val telescope: Lens[TcsSimState, TelescopeState]       = Focus[TcsSimState](_.telescope)
  val acMechs: Lens[TcsSimState, AcMechsState]           = Focus[TcsSimState](_.acMechs)
  val pwfs1Mechs: Lens[TcsSimState, PwfsMechsState]      = Focus[TcsSimState](_.pwfs1Mechs)
  val pwfs2Mechs: Lens[TcsSimState, PwfsMechsState]      = Focus[TcsSimState](_.pwfs2Mechs)
  val wfsConfigs: Lens[TcsSimState, AllWfsConfiguration] = Focus[TcsSimState](_.wfsConfigs)
  val baffles: Lens[TcsSimState, BafflesState]           = Focus[TcsSimState](_.baffles)

  val default: TcsSimState = TcsSimState(
    guide = GuideState.default,
    telescope = TelescopeState.default,
    acMechs = AcMechsState.default,
    pwfs1Mechs = PwfsMechsState.default,
    pwfs2Mechs = PwfsMechsState.default,
    wfsConfigs = AllWfsConfiguration.default,
    baffles = BafflesState.default
  )
