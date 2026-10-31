// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.refined.given
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.json.flamingos2.given
import lucuma.odb.json.gmos.given
import lucuma.odb.json.stepconfig.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * The Instrument Setup of a Daytime Calibration Set: the dynamic configuration every step of the
 * set is taken with.
 */
enum DaytimeCalibrationSetup derives Eq:
  case GmosNorth(config: DynamicConfig.GmosNorth)
  case GmosSouth(config: DynamicConfig.GmosSouth)
  case Flamingos2(config: Flamingos2DynamicConfig)

  def instrument: Instrument = this match
    case GmosNorth(_)  => Instrument.GmosNorth
    case GmosSouth(_)  => Instrument.GmosSouth
    case Flamingos2(_) => Instrument.Flamingos2

object DaytimeCalibrationSetup:
  val gmosNorth: Prism[DaytimeCalibrationSetup, GmosNorth] =
    GenPrism[DaytimeCalibrationSetup, GmosNorth]

  val gmosSouth: Prism[DaytimeCalibrationSetup, GmosSouth] =
    GenPrism[DaytimeCalibrationSetup, GmosSouth]

  val flamingos2: Prism[DaytimeCalibrationSetup, Flamingos2] =
    GenPrism[DaytimeCalibrationSetup, Flamingos2]

  object GmosNorth:
    val config: Lens[GmosNorth, DynamicConfig.GmosNorth] = Focus[GmosNorth](_.config)

  object GmosSouth:
    val config: Lens[GmosSouth, DynamicConfig.GmosSouth] = Focus[GmosSouth](_.config)

  object Flamingos2:
    val config: Lens[Flamingos2, Flamingos2DynamicConfig] = Focus[Flamingos2](_.config)

/**
 * One ordered row of a Daytime Calibration: a kind, an exposure count, the GCAL lamp setup of a
 * Flat or Arc, and the Instrument Setup. `explicitCount` is the user's: the ODB expands a manual
 * row into one atom of that many identical day-cal steps. A smart row carries no count or GCAL
 * setup of its own: the Smart GCAL lookup supplies them. `count` is read only: the steps the row
 * contributes, the lookup's for a smart row, none when it finds no mapping.
 */
case class DaytimeCalibrationSet(
  kind:          CalibrationSetType,
  explicitCount: Option[PosInt],
  gcal:          Option[StepConfig.Gcal],
  setup:         DaytimeCalibrationSetup,
  count:         Option[PosInt] = None
) derives Eq

object DaytimeCalibrationSet:
  val kind: Lens[DaytimeCalibrationSet, CalibrationSetType]       = Focus[DaytimeCalibrationSet](_.kind)
  val explicitCount: Lens[DaytimeCalibrationSet, Option[PosInt]]  =
    Focus[DaytimeCalibrationSet](_.explicitCount)
  val gcal: Lens[DaytimeCalibrationSet, Option[StepConfig.Gcal]]  =
    Focus[DaytimeCalibrationSet](_.gcal)
  val setup: Lens[DaytimeCalibrationSet, DaytimeCalibrationSetup] =
    Focus[DaytimeCalibrationSet](_.setup)

  given Decoder[DaytimeCalibrationSet] = Decoder.instance: c =>
    for
      kind  <- c.downField("kind").as[CalibrationSetType]
      exp   <- c.downField("explicitCount").as[Option[PosInt]]
      count <- c.downField("count").as[Option[PosInt]]
      gcal  <- c.downField("gcal").as[Option[StepConfig.Gcal]]
      north <- c.downField("gmosNorth").as[Option[DynamicConfig.GmosNorth]]
      south <- c.downField("gmosSouth").as[Option[DynamicConfig.GmosSouth]]
      f2    <- c.downField("flamingos2").as[Option[Flamingos2DynamicConfig]]
      setup <- north
                 .map(DaytimeCalibrationSetup.GmosNorth(_))
                 .orElse(south.map(DaytimeCalibrationSetup.GmosSouth(_)))
                 .orElse(f2.map(DaytimeCalibrationSetup.Flamingos2(_)))
                 .toRight(DecodingFailure("Calibration set has no instrument setup", c.history))
    yield DaytimeCalibrationSet(kind, exp, gcal, setup, count)
