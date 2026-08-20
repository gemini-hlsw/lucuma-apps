// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 * The centre an Archive Duplication Search was run around. A sidereal pointing is searched by
 * coordinates, a non-sidereal one by target name, which is why this is a sum and not a pair of
 * options.
 */
enum SearchCenter derives Eq:
  case Coords(coordinates: Coordinates)
  case TargetName(name: NonEmptyString)

/** The Search Area an Archive Duplication Search covered: a centre plus a radius. */
case class SearchArea(center: SearchCenter, radius: Option[Angle]) derives Eq:
  // The unit suffix is explicit so the archive cannot mis-default it. A missing radius means no
  // `sr` segment at all: a zero-radius search could only ever come back empty.
  private def radiusSegment: Option[String] =
    radius.map: r =>
      f"sr=${Angle.signedDecimalArcseconds.get(r).toDouble / 60.0}%.4fm"

  /** The Gemini Observatory Archive's own search for this Search Area. */
  def goaSearchUrl: String =
    val center_ : List[String] = center match
      case SearchCenter.Coords(c)     =>
        List(
          f"ra=${c.ra.toAngle.toDoubleDegrees}%.6f",
          f"dec=${c.dec.toAngle.toSignedDoubleDegrees}%.6f"
        )
      case SearchCenter.TargetName(n) =>
        List(s"object=${n.value.replace(" ", "+")}")
    (SearchArea.GoaSearchFormUrl :: center_ ++ radiusSegment.toList).mkString("/")

object SearchArea:
  private val GoaSearchFormUrl = "https://archive.gemini.edu/searchform"

/**
 * Archive Duplication Search result for one observation, without its Archive Matches: everything
 * the collapsed table renders. Matches are fetched per observation on expansion — see
 * `docs/adr/0007-archive-duplication-is-pulled-not-pushed.md`.
 */
case class ArchiveDuplication(
  state:         ArchiveDuplicationState,
  matchCount:    NonNegInt,
  saturated:     Boolean,
  lastCheckedAt: Option[Timestamp],
  error:         Option[NonEmptyString],
  searchArea:    Option[SearchArea]
) derives Eq:
  def isNotApplicable: Boolean =
    state === ArchiveDuplicationState.NotApplicable

  def hasMatches: Boolean =
    matchCount.value > 0

  // An observation the sweep should ask about: never checked, or checked and failed.
  def needsSearch: Boolean =
    state === ArchiveDuplicationState.NotChecked || state === ArchiveDuplicationState.Error

object ArchiveDuplication:
  given Decoder[ArchiveDuplication] = Decoder.instance: c =>
    for
      state         <- c.get[ArchiveDuplicationState]("state")
      matchCount    <- c.get[NonNegInt]("matchCount")
      saturated     <- c.get[Boolean]("saturated")
      lastCheckedAt <- c.get[Option[Timestamp]]("lastCheckedAt")
      error         <- c.get[Option[NonEmptyString]]("error")
      coordinates   <- c.get[Option[Coordinates]]("searchCoordinates")
      targetName    <- c.get[Option[NonEmptyString]]("searchTargetName")
      radius        <- c.get[Option[Angle]]("searchRadius")
    yield
      val center: Option[SearchCenter] =
        coordinates
          .map(SearchCenter.Coords(_))
          .orElse(targetName.map(SearchCenter.TargetName(_)))
      ArchiveDuplication(
        state,
        matchCount,
        saturated,
        lastCheckedAt,
        error,
        center.map(SearchArea(_, radius))
      )

/**
 * One archived file an Archive Duplication Search matched. Most fields are the archive's own text
 * rather than a typed projection, because which of them the archive populates varies by instrument
 * and by era.
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
