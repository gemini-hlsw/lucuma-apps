// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.syntax.all.*
import explore.events.HorizonsMessage
import explore.model.*
import explore.model.WorkerClients.HorizonsClient
import explore.model.boopickle.HorizonsPicklers.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId
import workers.WorkerClient

import java.time.Instant

object tracking:

  def getRegionOrTrackingForObservingNight(
    target:      Target,
    site:        Site,
    when:        Instant,
    totalPoints: Int = 300
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[Either[String, RegionOrTracking]] =
    target match
      case Target.Nonsidereal(_, key, _) =>
        key.horizonsKey.fold(
          "User defined Ephemeris Keys are not yet supported".asLeft.pure[IO]
        )(hk =>
          val interval = ObservingNight.fromSiteAndInstant(site, when).interval
          val start    = interval.lowerBound.a
          val end      = interval.upperBound.a
          HorizonsClient[IO]
            .requestSingle(
              HorizonsMessage.EphemerisRequest(hk, site, start, end, totalPoints)
            )
            .map(
              _.fold("Error calling HorizonsClient".asLeft)(et =>
                et.map(t => RegionOrTracking.fromTracking(t.ephemerisTracking))
              )
            )
        )
      case Target.Sidereal(_, t, _, _)   => RegionOrTracking.fromTracking(t).asRight.pure
      case Target.Opportunity(_, r, _)   => RegionOrTracking.fromRegion(r).asRight.pure

  def getRegionOrTrackingMapForObservingNight(
    targetWithIds: List[TargetWithId],
    site:          Site,
    when:          Instant,
    totalPoints:   Int = 300
  )(using
    WorkerClient[IO, HorizonsMessage.Request]
  ): IO[Either[String, RegionOrTrackingMap]] =
    targetWithIds
      .traverse(twid =>
        getRegionOrTrackingForObservingNight(twid.target, site, when, totalPoints)
          .map(_.map(t => (twid.id, t)))
      )
      .map(_.sequence.map(RegionOrTrackingMap.from(_)))
