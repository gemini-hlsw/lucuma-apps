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
import lucuma.catalog.goa.GoaEndpoint
import lucuma.catalog.goa.GoaParams
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
import org.http4s.Uri
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
  stale:         Boolean,
  queryUrls:     List[String]
) derives Eq:
  def isNotApplicable: Boolean =
    state === ArchiveDuplicationState.NotApplicable

  def hasMatches: Boolean =
    matchCount.value > 0

  def needsSearch: Boolean =
    state === ArchiveDuplicationState.NotChecked || state === ArchiveDuplicationState.Error ||
      stale

  /** The archive search pages for the queries the Search ran, one per fan-out query. */
  lazy val searchLinks: List[ArchiveSearchLink] =
    queryUrls.zipWithIndex.map((url, i) => ArchiveSearchLink.fromQueryUrl(url, i))

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
      queryUrls     <- c.get[List[String]]("queryUrls")
    yield ArchiveDuplication(
      state,
      matchCount,
      saturated,
      lastCheckedAt,
      error,
      attemptedAt,
      stale,
      queryUrls
    )

/**
 * A link to the archive's own search page for one GOA query.
 */
case class ArchiveSearchLink(label: String, url: String) derives Eq

object ArchiveSearchLink:
  def fromQueryUrl(queryUrl: String, index: Int): ArchiveSearchLink =
    val uri: Option[Uri] = Uri.fromString(queryUrl).toOption
    ArchiveSearchLink(
      uri.flatMap(GoaParams.instrumentOf).fold(s"Search ${index + 1}")(_.shortName),
      uri.fold(queryUrl)(GoaEndpoint.fromUri.replace(GoaEndpoint.SearchForm)(_).renderString)
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
