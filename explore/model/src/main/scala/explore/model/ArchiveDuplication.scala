// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.model.enums.ArchiveDuplicationState
import org.typelevel.cats.time.given

import java.time.LocalDate

/**
 * Archive Duplication Search result for one observation.
 */
case class ArchiveDuplication(
  state:         ArchiveDuplicationState,
  matchCount:    NonNegInt,
  saturated:     Boolean,
  lastCheckedAt: Option[Timestamp],
  error:         Option[NonEmptyString],
  attemptedAt:   Option[Timestamp],
  stale:         Boolean
) derives Eq:
  def isNotApplicable: Boolean =
    state === ArchiveDuplicationState.NotApplicable

  def hasMatches: Boolean =
    matchCount.value > 0

  def needsSearch: Boolean =
    state === ArchiveDuplicationState.NotChecked || state === ArchiveDuplicationState.Error ||
      stale

object ArchiveDuplication:
  given Decoder[ArchiveDuplication] = Decoder.instance: c =>
    for
      state         <- c.get[ArchiveDuplicationState]("state")
      matchCount    <- c.get[NonNegInt]("matchCount")
      saturated     <- c.get[Boolean]("saturated")
      lastCheckedAt <- c.get[Option[Timestamp]]("lastCheckedAt")
      error         <- c.get[Option[NonEmptyString]]("error")
      attemptedAt   <- c.get[Option[Timestamp]]("attemptedAt")
      stale         <- c.get[Boolean]("stale")
    yield ArchiveDuplication(
      state,
      matchCount,
      saturated,
      lastCheckedAt,
      error,
      attemptedAt,
      stale
    )

/**
 * One archived file an Archive Duplication Search matched.
 */
case class ArchiveMatch(
  name:                 String,
  dataLabel:            Option[String],
  coordinates:          Option[Coordinates],
  instrumentString:     String,
  instrument:           Option[Instrument],
  qaStateString:        Option[String],
  utDateTime:           Option[Timestamp],
  releaseDate:          Option[LocalDate],
  programReference:     Option[String],
  observationReference: Option[String],
  objectName:           Option[String],
  exposure:             Option[TimeSpan],
  disperser:            Option[String],
  filter:               Option[String],
  wavelength:           Option[Wavelength],
  distance:             Option[Angle]
) derives Eq

object ArchiveMatch:
  given Decoder[ArchiveMatch] = deriveDecoder
