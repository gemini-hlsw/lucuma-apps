// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import explore.events.HorizonsMessage
import explore.model.*
import explore.model.RegionOrTracking.*
import explore.utils.tracking.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Region
import lucuma.core.model.Target
import lucuma.schemas.model.CoordinatesAt
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.syntax.*
import org.typelevel.cats.time.given
import workers.WorkerClient

import java.time.Instant

/**
 * Holds the information about ObservationTargets at a given time. What this means differs based on
 * the information available at the time `build` is called. See the documentation for `build` below.
 *
 * This class encapsulates the complexity of all of the possbilities in `build`.
 * @param at - the time for which the coordinates were calculated.
 * @param asterism - this is the location of the center of all the science targets.
 * @param science - the location of all of the science targets.
 * @param blindOffset - the location of the blind offset, if any.
 */
case class ObservationRegionsOrCoordinatesAt(
  at:          Option[Instant],
  asterism:    Option[ErrorOrRegionOrCoords],
  science:     List[(TargetWithId, Option[ErrorOrRegionOrCoords])],
  blindOffset: Option[(TargetWithId, Option[ErrorOrRegionOrCoords])]
) derives Eq:
  val allTargets: List[(TargetWithId, Option[ErrorOrRegionOrCoords])] =
    science ++ blindOffset.toList

object ObservationRegionsOrCoordinatesAt:
  val Empty: ObservationRegionsOrCoordinatesAt =
    ObservationRegionsOrCoordinatesAt(none, none, List.empty, none)

  /**
   * There are a lot of scenarios to contemplate here:
   *
   * 1) If we have both an obsTime AND a site, we can get ephemeris data and process all targets
   * including non-sidereals, "moving" all targets to the obsTime. If there are non-sidereals this
   * requires a call to Horizons (or maybe from the cache). That is why this method is effectful.
   *
   * 2) If we have an obsTime, but not a site, we can't deal with non-sidereals, so the
   * ErrorOrRegionCoords for non-sidereals will be None, and the the asterism will be None UNLESS it
   * has a ToO as well.
   *
   * 3) If we have neither obsTime or site, we use the epoch of the first sidereal target (if any)
   * and move all sidereal targets to that time. If there are no sidereal targets, `at` in the
   * result will be None. Otherwise, this is equivalent to scenario 2 providing an obsTime related
   * to epoch of the first sidereal target.
   */
  def build(
    obsTargets: ObservationTargets,
    obsTime:    Option[Instant],
    site:       Option[Site]
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ObservationRegionsOrCoordinatesAt] =
    obsTime.fold(
      atTargetEpoch(obsTargets).pure[IO]
    )(at =>
      site.fold(atInstant(obsTargets, at.some).pure[IO])(s => forSiteAndTime(obsTargets, at, s))
    )

  private def getAsterism(
    science: List[Option[ErrorOrRegionOrCoords]]
  ): Option[ErrorOrRegionOrCoords] =
    science
      .collect:
        case Some(Right(Left(region))) => ErrorOrRegionOrCoords.fromRegion(region)
      .headOption
      .orElse:
        science
          .traverse: eorc =>
            eorc match
              case Some(Right(Right(coords))) => coords.some
              case _                          => none
          .flatMap: coordsList =>
            NonEmptyList
              .fromList(coordsList)
              .map: nel =>
                val coordsAt =
                  CoordinatesAt(nel.head.at, Coordinates.centerOf(nel.map(_.coordinates)))
                ErrorOrRegionOrCoords.fromCoordinatesAt(coordsAt)

  private def forSiteAndTime(obsTargets: ObservationTargets, obsTime: Instant, site: Site)(using
    WorkerClient[IO, HorizonsMessage.Request]
  ): IO[ObservationRegionsOrCoordinatesAt] =
    def forTarget(twid: TargetWithId): IO[(TargetWithId, Option[ErrorOrRegionOrCoords])] =
      getRegionOrTrackingForObservingNight(twid.target, site, obsTime).map: erot =>
        (twid, erot.flatMap(_.regionOrCoordinatesAt(obsTime)).some)

    def science = obsTargets.science.traverse(forTarget)
    def blind   = obsTargets.blindOffset.traverse(forTarget)

    (science, blind).mapN: (s, b) =>
      ObservationRegionsOrCoordinatesAt(obsTime.some, getAsterism(s.map(_._2)), s, b)

  // we don't have an obstime or site, so we can't handle non-sidereals, but we can pick a time
  // from one of the sidereals (if any) and correct all other sidereals to that.
  private def atTargetEpoch(obsTargets: ObservationTargets): ObservationRegionsOrCoordinatesAt =
    // try to get the epoch from the first non-sidereal
    val at: Option[Instant] = obsTargets.allTargets
      .map(_.target)
      .collectFirst:
        case Target.Sidereal(tracking = tracking) => tracking.epoch.toInstant
    atInstant(obsTargets, at)

  private def atInstant(
    obsTargets: ObservationTargets,
    at:         Option[Instant]
  ): ObservationRegionsOrCoordinatesAt =
    def forTarget(twid: TargetWithId): (TargetWithId, Option[ErrorOrRegionOrCoords]) =
      twid.target match
        case Target.Sidereal(tracking = tracking) =>
          // If there is no 'at', there are no sidereals
          val coords = at
            .flatMap(a =>
              if tracking.epoch.toInstant === a then CoordinatesAt(a, tracking.baseCoordinates).some
              else tracking.coordinatesAt(a).toOption
            )
            .getOrElse(CoordinatesAt(Epoch.MinValue.toInstant, Coordinates.Zero))
          (twid, ErrorOrRegionOrCoords.fromCoordinatesAt(coords).some)
        case Target.Nonsidereal(_, _, _)          => (twid, none)
        case Target.Opportunity(region = region)  =>
          (twid, ErrorOrRegionOrCoords.fromRegion(region).some)
    val science                                                                      = obsTargets.mapScience(forTarget)
    val blind                                                                        = obsTargets.blindOffset.map(forTarget)
    val asterism                                                                     = getAsterism(science.map(_._2))

    ObservationRegionsOrCoordinatesAt(at, asterism, science, blind)
