// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.coordinates.query.given

/**
 * One aperture of a MOS mask design, as served by the ODB's `MaskSlit`.
 */
case class MaskDesignSlit(
  id:               Int,
  coordinates:      Coordinates,
  x:                BigDecimal,
  y:                BigDecimal,
  width:            Angle,
  length:           Angle,
  offsetAlongSlit:  Angle,
  offsetAcrossSlit: Angle,
  tilt:             Angle,
  priority:         MosSlitPriority
) derives Eq:
  def isAcquisition: Boolean = priority === MosSlitPriority.Acquisition

object MaskDesignSlit:
  // MosSlitPriority tags are the mask file format's single characters, so the
  // Enumerated decoder does not match the GraphQL enum names.
  private given Decoder[MosSlitPriority] =
    Decoder[String].emap:
      case "ACQUISITION" => MosSlitPriority.Acquisition.asRight
      case "HIGH"        => MosSlitPriority.High.asRight
      case "MEDIUM"      => MosSlitPriority.Medium.asRight
      case "LOW"         => MosSlitPriority.Low.asRight
      case "IGNORE"      => MosSlitPriority.Ignore.asRight
      case other         => s"Could not parse MOS slit priority '$other'".asLeft

  given Decoder[MaskDesignSlit] = Decoder.instance: c =>
    for
      id       <- c.downField("id").as[Int]
      coords   <- c.downField("coordinates").as[Coordinates]
      x        <- c.downField("x").as[BigDecimal]
      y        <- c.downField("y").as[BigDecimal]
      width    <- c.downField("width").as[Angle]
      length   <- c.downField("length").as[Angle]
      along    <- c.downField("offsetAlongSlit").as[Angle]
      across   <- c.downField("offsetAcrossSlit").as[Angle]
      tilt     <- c.downField("tilt").as[Angle]
      priority <- c.downField("priority").as[MosSlitPriority]
    yield MaskDesignSlit(id, coords, x, y, width, length, along, across, tilt, priority)

/**
 * The design read from a MOS mask attachment's file, as served by the ODB's `MaskDefinition`.
 */
case class MaskDesign(
  name:          NonEmptyString,
  instrument:    Instrument,
  pointing:      Coordinates,
  positionAngle: Angle,
  slits:         List[MaskDesignSlit]
) derives Eq:
  // Fixed per instrument: mask design software only accepts these orientations.
  def dispersionDirection: MosDispersionDirection =
    instrument match
      case Instrument.Flamingos2 => MosDispersionDirection.Vertical
      case _                     => MosDispersionDirection.Horizontal

object MaskDesign:
  given Decoder[MaskDesign] = Decoder.instance: c =>
    for
      name          <- c.downField("name").as[NonEmptyString]
      instrument    <- c.downField("instrument").as[Instrument]
      pointing      <- c.downField("pointing").as[Coordinates]
      positionAngle <- c.downField("positionAngle").as[Angle]
      slits         <- c.downField("slits").as[List[MaskDesignSlit]]
    yield MaskDesign(name, instrument, pointing, positionAngle, slits)
