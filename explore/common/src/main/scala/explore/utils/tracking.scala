// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.syntax.all.*
import explore.events.HorizonsMessage
import explore.model.WorkerClients.HorizonsClient
import explore.model.boopickle.HorizonsPicklers.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.TargetWithId
import workers.WorkerClient

import java.time.Instant

object tracking:

  def getTrackingForObservingNight(
    target:      Target,
    site:        Site,
    when:        Instant,
    totalPoints: Int = 300
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, Tracking]] =
    target match
      case Target.Nonsidereal(_, key, _)      =>
        key.horizonsKey.fold(
          "User defined Ephemeris Keys are not supported.".asLeft[Tracking].pure[IO]
        )(hk =>
          val interval = ObservingNight.fromSiteAndInstant(site, when).interval
          val start    = interval.lowerBound.a
          val end      = interval.upperBound.a
          println(s"observing night start: $start, end: $end")
          HorizonsClient[IO]
            .requestSingle(
              HorizonsMessage.EphemerisRequest(hk, site, start, end, totalPoints)
            )
            .map(_.fold("Error calling HorizonsClient.".asLeft)(_.map(_.ephemerisTracking)))
        )
      case Target.Opportunity(_, _, _)        => "Targets of Opportunity have no coordinates".asLeft.pure
      case Target.Sidereal(_, tracking, _, _) => tracking.asRight.pure

  def getTrackingForObservingNightMap(
    targetWithIds: List[TargetWithId],
    site:          Site,
    when:          Instant,
    totalPoints:   Int = 300
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, Map[Target.Id, Tracking]]] =
    targetWithIds
      .traverse(twid =>
        getTrackingForObservingNight(twid.target, site, when, totalPoints)
          .map(r => r.map(t => (twid.id, t)))
      )
      .map(_.sequence.map(_.toMap))

  // Not yet used, but will be used in an upcoming PR

  // def getTrackingAt(
  //   target: Target,
  //   site:   Site,
  //   at:     Instant
  // )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, Tracking]] =
  //   target match
  //     case Target.Nonsidereal(_, key, _)      =>
  //       key.horizonsKey.fold(
  //         "User defined Ephemeris Keys are not supported.".asLeft[Tracking].pure[IO]
  //       )(hk =>
  //         // We're interested in a single point, but we'll make sure our ephemeris
  //         // includes that point by bracketing it.
  //         val start = at.minusSeconds(60)
  //         val end   = at.plusSeconds(61)
  //         HorizonsClient[IO]
  //           .requestSingle(
  //             HorizonsMessage.EphemerisRequest(hk, site, start, end, 3)
  //           )
  //           .map(_.fold("Error calling HorizonsClient.".asLeft)(_.map(_.ephemerisTracking)))
  //       )
  //     case Target.Opportunity(_, _, _)        => "Targets of Opportunity have no coordinates".asLeft.pure
  //     case Target.Sidereal(_, tracking, _, _) => tracking.asRight.pure

  // def getTrackingsAt(
  //   targets: List[Target],
  //   site:    Site,
  //   at:      Instant
  // )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, List[Tracking]]] =
  //   targets
  //     .traverse(t => getTrackingAt(t, site, at))
  //     .map(_.sequence)

  // def getTrackingAtMap(
  //   targetWithIds: List[TargetWithId],
  //   site:          Site,
  //   at:            Instant
  // )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, Map[Target.Id, Tracking]]] =
  //   targetWithIds
  //     .traverse(twid => getTrackingAt(twid.target, site, at).map(r => r.map(t => (twid.id, t))))
  //     .map(_.sequence.map(_.toMap))

  // def getTrackingOrRegionAt(
  //   target: Target,
  //   site:   Site,
  //   at:     Instant
  // )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, Either[Tracking, Region]]] =
  //   target match
  //     case Target.Opportunity(_, region, _) => region.asRight.asRight.pure
  //     case _                                => getTrackingAt(target, site, at).map(_.map(_.asLeft))

  // def getTrackingOrRegionAtMap(
  //   targetWithIds: List[TargetWithId],
  //   site:          Site,
  //   at:            Instant
  // )(using
  //   WorkerClient[IO, HorizonsMessage.Request]
  // ): IO[Map[Target.Id, Either[String, Either[Tracking, Region]]]] =
  //   targetWithIds
  //     .traverse(twid => getTrackingOrRegionAt(twid.target, site, at).map(r => (twid.id, r)))
  //     .map(_.toMap)

  // extension (list: List[Either[String, Either[Tracking, Region]]])
  //   // If there is a ToO, the asterism as treated as a ToO and the FIRST ToO region is returned.
  //   def asterismCoordsOrRegionAt(
  //     at: Instant
  //   ): Option[Either[String, Either[CoordinatesAt, Region]]] =
  //     NonEmptyList
  //       .fromList(list)
  //       .map:
  //         _.sequence
  //           .flatMap: (nel: NonEmptyList[Either[Tracking, Region]]) =>
  //             nel
  //               .collect:
  //                 case Right(region) => region.asRight.asRight
  //               .headOption
  //               .getOrElse {
  //                 val trackings =
  //                   nel.collect:
  //                     // everything should be a Left now, so we should have a non-empty list
  //                     case Left(tracking) => tracking
  //                 NonEmptyList
  //                   .fromList(trackings)
  //                   .fold("Missing tracking information".asLeft): trackings =>
  //                     val tracking =
  //                       if (trackings.size === 1) trackings.head else CompositeTracking(trackings)
  //                     tracking.coordinatesAt(at).map(_.asLeft)
  //               }
