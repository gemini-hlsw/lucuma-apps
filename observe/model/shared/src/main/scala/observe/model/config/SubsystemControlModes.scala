// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import monocle.Focus
import monocle.Lens
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
) derives Eq,
      Encoder.AsObject,
      Decoder:
  def connectEpics: Boolean =
    altair.connect || gems.connect || flamingos2.connect || gcal.connect || gmos.connect || gnirs.connect || gsaoi.connect || gws.connect || nifs.connect || niri.connect || tcs.connect

  /**
   * Site fields whose mode differs between `this` and `other`, as (field, from, to).
   */
  def diff(
    other: SubsystemControlModes,
    site:  Site
  ): List[(SubsystemControlModes.Field, SubsystemControlMode, SubsystemControlMode)] =
    SubsystemControlModes
      .fieldsFor(site)
      .map(f => (f, f.mode.get(this), f.mode.get(other)))
      .filter((_, from, to) => from =!= to)

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

object SubsystemControlModes:
  val Altair: Lens[SubsystemControlModes, SubsystemControlMode]     =
    Focus[SubsystemControlModes](_.altair)
  val Gems: Lens[SubsystemControlModes, SubsystemControlMode]       =
    Focus[SubsystemControlModes](_.gems)
  val Dhs: Lens[SubsystemControlModes, SubsystemControlMode]        =
    Focus[SubsystemControlModes](_.dhs)
  val Flamingos2: Lens[SubsystemControlModes, SubsystemControlMode] =
    Focus[SubsystemControlModes](_.flamingos2)
  val Gcal: Lens[SubsystemControlModes, SubsystemControlMode]       =
    Focus[SubsystemControlModes](_.gcal)
  val Gmos: Lens[SubsystemControlModes, SubsystemControlMode]       =
    Focus[SubsystemControlModes](_.gmos)
  val Gnirs: Lens[SubsystemControlModes, SubsystemControlMode]      =
    Focus[SubsystemControlModes](_.gnirs)
  val Gpi: Lens[SubsystemControlModes, SubsystemControlMode]        =
    Focus[SubsystemControlModes](_.gpi)
  val Ghost: Lens[SubsystemControlModes, SubsystemControlMode]      =
    Focus[SubsystemControlModes](_.ghost)
  val Igrins2: Lens[SubsystemControlModes, SubsystemControlMode]    =
    Focus[SubsystemControlModes](_.igrins2)
  val Gsaoi: Lens[SubsystemControlModes, SubsystemControlMode]      =
    Focus[SubsystemControlModes](_.gsaoi)
  val Gws: Lens[SubsystemControlModes, SubsystemControlMode]        =
    Focus[SubsystemControlModes](_.gws)
  val Nifs: Lens[SubsystemControlModes, SubsystemControlMode]       =
    Focus[SubsystemControlModes](_.nifs)
  val Niri: Lens[SubsystemControlModes, SubsystemControlMode]       =
    Focus[SubsystemControlModes](_.niri)
  val Tcs: Lens[SubsystemControlModes, SubsystemControlMode]        =
    Focus[SubsystemControlModes](_.tcs)

  /**
   * An editable subsystem: its display label, the HOCON key pureconfig uses for it, and its lens.
   */
  case class Field(
    label:     String,
    configKey: String,
    mode:      Lens[SubsystemControlModes, SubsystemControlMode]
  )

  val CommonFields: List[Field] =
    List(
      Field("TCS", "tcs", Tcs),
      Field("GCAL", "gcal", Gcal),
      Field("DHS", "dhs", Dhs),
      Field("GWS", "gws", Gws),
      Field("GMOS", "gmos", Gmos)
    )

  val NorthFields: List[Field] =
    List(
      Field("Altair", "altair", Altair),
      Field("GNIRS", "gnirs", Gnirs),
      Field("NIRI", "niri", Niri),
      Field("NIFS", "nifs", Nifs),
      Field("IGRINS-2", "igrins-2", Igrins2)
    )

  val SouthFields: List[Field] =
    List(
      Field("GeMS", "gems", Gems),
      Field("GSAOI", "gsaoi", Gsaoi),
      Field("Flamingos-2", "flamingos-2", Flamingos2),
      Field("GHOST", "ghost", Ghost)
    )

  /** Fields editable at a site: the common ones first, then the site specific ones. */
  def fieldsFor(site: Site): List[Field] =
    CommonFields ++ (site match
      case Site.GN => NorthFields
      case Site.GS => SouthFields)
