// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder
import monocle.Focus
import monocle.Lens

case class SubsystemOverrides(
  isTcsEnabled:        SubsystemEnabled,
  isInstrumentEnabled: SubsystemEnabled,
  isGcalEnabled:       SubsystemEnabled,
  isDhsEnabled:        SubsystemEnabled
) derives Eq,
      Encoder.AsObject,
      Decoder:
  def disableTcs: SubsystemOverrides = copy(isTcsEnabled = SubsystemEnabled.Disabled)

  def enableTcs: SubsystemOverrides = copy(isTcsEnabled = SubsystemEnabled.Enabled)

  def disableInstrument: SubsystemOverrides = copy(isInstrumentEnabled = SubsystemEnabled.Disabled)

  def enableInstrument: SubsystemOverrides = copy(isInstrumentEnabled = SubsystemEnabled.Enabled)

  def disableGcal: SubsystemOverrides = copy(isGcalEnabled = SubsystemEnabled.Disabled)

  def enableGcal: SubsystemOverrides = copy(isGcalEnabled = SubsystemEnabled.Enabled)

  def disableDhs: SubsystemOverrides = copy(isDhsEnabled = SubsystemEnabled.Disabled)

  def enableDhs: SubsystemOverrides = copy(isDhsEnabled = SubsystemEnabled.Enabled)

object SubsystemOverrides:
  val AllEnabled: SubsystemOverrides = SubsystemOverrides(
    isTcsEnabled = SubsystemEnabled.Enabled,
    isInstrumentEnabled = SubsystemEnabled.Enabled,
    isGcalEnabled = SubsystemEnabled.Enabled,
    isDhsEnabled = SubsystemEnabled.Enabled
  )

  val isTcsEnabled: Lens[SubsystemOverrides, SubsystemEnabled]        =
    Focus[SubsystemOverrides](_.isTcsEnabled)
  val isInstrumentEnabled: Lens[SubsystemOverrides, SubsystemEnabled] =
    Focus[SubsystemOverrides](_.isInstrumentEnabled)
  val isGcalEnabled: Lens[SubsystemOverrides, SubsystemEnabled]       =
    Focus[SubsystemOverrides](_.isGcalEnabled)
  val isDhsEnabled: Lens[SubsystemOverrides, SubsystemEnabled]        =
    Focus[SubsystemOverrides](_.isDhsEnabled)
