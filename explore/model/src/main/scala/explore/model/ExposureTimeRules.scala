// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SkyBackground
import lucuma.core.model.sequence.exposure.ExposureRule
import lucuma.core.model.sequence.exposure.ExposureRules

/**
 * The lucuma-core exposure time rules an exposure time field is checked against as it is edited.
 * The ODB checks the same rules on every generated step, so the field rejects what the ODB would
 * flag without a round trip. GHOST names the camera in its messages, so its panel takes
 * `ExposureRules.ghostRed` and `ghostBlue` directly.
 */
object ExposureTimeRules:

  // The sky background only matters to the GMOS imaging saturation rule, which also needs the
  // imaging step and so is not checked here.
  private val AnySky: SkyBackground = SkyBackground.Darkest

  /**
   * The rules for an exposure of `instrument` with no explicit read mode. The ODB then picks the
   * read mode from the exposure time, so the only minimum a short exposure can break is that of the
   * read mode for the shortest exposures.
   */
  def apply(instrument: Option[Instrument]): List[ExposureRule] =
    instrument match
      case Some(Instrument.GmosNorth)  => ExposureRules.gmosNorth(none, AnySky)
      case Some(Instrument.GmosSouth)  => ExposureRules.gmosSouth(none, AnySky)
      case Some(Instrument.Flamingos2) => flamingos2(none)
      case Some(Instrument.Gnirs)      => gnirs(none)
      case Some(Instrument.Igrins2)    => ExposureRules.igrins2(ObserveClass.Science)
      case _                           => Nil

  def flamingos2(explicitReadMode: Option[Flamingos2ReadMode]): List[ExposureRule] =
    ExposureRules.flamingos2(explicitReadMode.getOrElse(Flamingos2ReadMode.Bright))

  def gnirs(explicitReadMode: Option[GnirsReadMode]): List[ExposureRule] =
    ExposureRules.gnirs(explicitReadMode.getOrElse(GnirsReadMode.VeryBright))
