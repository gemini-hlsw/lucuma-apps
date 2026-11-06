// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.data.NonEmptyList
import lucuma.core.enums.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.gnirs
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.igrins2

/** Plain positions, for modes with no slit to nod along. */
type TelescopeConfigPresets = PresetList[String, NonEmptyList[TelescopeConfig]]

/** Along-slit or to-sky patterns, keyed by the instrument's preset enum. */
type SlitTelescopeConfigPresets[P] = PresetList[P, SlitTelescopeConfigs]

/** The telescope-config patterns each observing mode offers. */
object TelescopeConfigPresets:
  // Per-instrument presets. Slit modes key them by the instrument's preset enum; modes with no
  // slit to nod along key them by plain name. The ones derived from the optical configuration
  // are defs.
  val GmosLongSlit: SlitTelescopeConfigPresets[GmosSlitOffsetPreset] =
    PresetList.fromEnumerated(gmos.longslit.defaultSlitTelescopeConfigs)

  val Flamingos2LongSlit: SlitTelescopeConfigPresets[Flamingos2SlitOffsetPreset] =
    PresetList.fromEnumerated(flamingos2.defaultSlitTelescopeConfigs)

  val Flamingos2Telluric: SlitTelescopeConfigPresets[Flamingos2TelluricOffsetPreset] =
    PresetList.fromEnumerated(flamingos2.defaultTelluricTelescopeConfigs)

  val Flamingos2Mos: SlitTelescopeConfigPresets[Flamingos2MosOffsetPreset] =
    PresetList.fromEnumerated(flamingos2.defaultMosTelescopeConfigs)

  val Igrins2LongSlit: SlitTelescopeConfigPresets[Igrins2SlitOffsetPreset] =
    PresetList.fromEnumerated(igrins2.defaultSlitTelescopeConfigs)

  def gnirsLongSlit(
    prism:      GnirsPrism,
    camera:     GnirsCamera,
    wavelength: GnirsGratingWavelength
  ): SlitTelescopeConfigPresets[GnirsSlitOffsetPreset] =
    PresetList.fromEnumerated[GnirsSlitOffsetPreset, SlitTelescopeConfigs](
      gnirs.defaultSlitTelescopeConfigs(_, prism, camera, wavelength)
    )

  def gnirsIfu(fpu: GnirsFpuIfu): TelescopeConfigPresets =
    PresetList(gnirs.gnirsIfuTelescopeConfigPresets(fpu))

  def gmosNorthIfu(fpu: GmosNorthIfuFpu): TelescopeConfigPresets =
    PresetList(gmos.ifu.northTelescopeConfigPresets(fpu))

  def gmosSouthIfu(fpu: GmosSouthIfuFpu): TelescopeConfigPresets =
    PresetList(gmos.ifu.southTelescopeConfigPresets(fpu))

  /** A MOS mask has no single slit to nod along, so the only preset is the default. */
  def gmosMos(defaultConfigs: NonEmptyList[TelescopeConfig]): TelescopeConfigPresets =
    PresetList(NonEmptyList.one("Default" -> defaultConfigs))
