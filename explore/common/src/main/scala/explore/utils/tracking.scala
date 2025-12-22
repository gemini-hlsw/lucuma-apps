// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.syntax.all.*
import explore.events.HorizonsMessage
import explore.model.*
import explore.model.WorkerClients.HorizonsWorkerClient
import explore.model.boopickle.HorizonsPicklers.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.model.EphemerisKey
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.ObservingNight
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.horizons.HorizonsClient.ElementsPerDay
import lucuma.schemas.model.TargetWithId
import org.typelevel.cats.time.given
import workers.WorkerClient

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.temporal.ChronoUnit

object tracking:
  private def getEphemerisTrackingForObservingNight(
    key:   EphemerisKey,
    site:  Site,
    night: ObservingNight
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[EphemerisTracking]] =
    val TotalPoints = 600
    key.horizonsKey.fold(
      "User defined Ephemeris Keys are not yet supported".asLeft.pure[IO]
    )(hk =>
      val interval = night.interval
      val start    = interval.lower.minus(Duration.ofHours(12))
      val end      = interval.upper.plus(Duration.ofHours(12))
      HorizonsWorkerClient[IO]
        .requestSingle(
          HorizonsMessage.EphemerisRequest(hk, site, start, end, TotalPoints)
        )
        .map(
          _.fold("Error calling HorizonsClient".asLeft)(et => et.map(_.ephemerisTracking))
        )
    )

  private def getEphemerisTrackingForSemester(
    key:      EphemerisKey,
    site:     Site,
    semester: Semester,
    cadence:  ElementsPerDay
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[EphemerisTracking]] =
    key.horizonsKey.fold(
      "User defined Ephemeris Keys are not yet supported".asLeft.pure[IO]
    )(hk =>
      val start = semester.start.atSite(site).toInstant
      val end   = semester.end.atSite(site).toInstant
      val days  = ChronoUnit.DAYS.between(start, end) + 1
      HorizonsWorkerClient[IO]
        .requestSingle(
          HorizonsMessage.AlignedEphemerisRequest(hk, site, start, days.toInt, cadence)
        )
        .map(
          _.fold("Error calling HorizonsClient".asLeft)(et => et.map(_.ephemerisTracking))
        )
    )

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Site,
    night:  ObservingNight
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    target match
      case Target.Nonsidereal(_, key, _) =>
        getEphemerisTrackingForObservingNight(key, site, night).map(
          _.map(RegionOrTracking.fromTracking)
        )
      case Target.Sidereal(_, t, _, _)   => RegionOrTracking.fromTracking(t).asRight.pure
      case Target.Opportunity(_, r, _)   => RegionOrTracking.fromRegion(r).asRight.pure

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Site,
    when:   Instant
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    val night = ObservingNight.fromSiteAndInstant(site, when)
    getRegionOrTrackingForObservingNight(target, site, night)

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Site,
    when:   LocalDate
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    val night = ObservingNight.fromSiteAndLocalDate(site, when)
    getRegionOrTrackingForObservingNight(target, site, night)

  def getRegionOrTrackingMapForObservingNight(
    targetWithIds: List[TargetWithId],
    site:          Site,
    when:          Instant
  )(using
    WorkerClient[IO, HorizonsMessage.Request]
  ): IO[ErrorMsgOr[RegionOrTrackingMap]] =
    targetWithIds
      .traverse(twid =>
        getRegionOrTrackingForObservingNight(twid.target, site, when)
          .map(_.map(t => (twid.id, t)))
      )
      .map(_.sequence.map(RegionOrTrackingMap.from(_)))

  // Get low resolution tracking for the semester.
  def getRegionOrTrackingForSemester(
    target:   Target,
    site:     Site,
    semester: Semester,
    cadence:  ElementsPerDay = 2
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    target match
      case Target.Nonsidereal(_, key, _) =>
        getEphemerisTrackingForSemester(key, site, semester, cadence).map(
          _.map(RegionOrTracking.fromTracking)
        )
      case Target.Sidereal(_, t, _, _)   => RegionOrTracking.fromTracking(t).asRight.pure
      case Target.Opportunity(_, r, _)   => RegionOrTracking.fromRegion(r).asRight.pure

  // Combines the high-res ephemeris from getRegionOrTrackingForObservingNight with
  // the low-res ephemeris from getRegionOrTrackingForSemester.
  def getMixedResolutionRegionOrTracking(
    target:        Target,
    site:          Site,
    when:          Instant,
    lowResCadence: ElementsPerDay = 2
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    target match
      case Target.Nonsidereal(_, key, _) =>
        val semester = Semester
          .fromSiteAndInstant(site, when)
          .getOrElse:
            if (when < Semester.MinValue.start.atSite(site).toInstant) Semester.MinValue
            else Semester.MaxValue
        val night    = ObservingNight.fromSiteAndInstant(site, when)
        val lowRes   = getEphemerisTrackingForSemester(key, site, semester, lowResCadence)
        val highRes  = getEphemerisTrackingForObservingNight(key, site, night)
        (lowRes, highRes).mapN: (low, high) =>
          (low, high).mapN: (lowTrack, highTrack) =>
            RegionOrTracking.fromTracking(lowTrack ++ highTrack)

      case Target.Sidereal(_, t, _, _) => RegionOrTracking.fromTracking(t).asRight.pure
      case Target.Opportunity(_, r, _) => RegionOrTracking.fromRegion(r).asRight.pure

  def getMixedResolutionRegionOrTrackingMap(
    targetWithIds: List[TargetWithId],
    site:          Site,
    when:          Instant,
    lowResCadence: ElementsPerDay = 2
  )(using
    WorkerClient[IO, HorizonsMessage.Request]
  ): IO[ErrorMsgOr[RegionOrTrackingMap]] =
    targetWithIds
      .traverse(twid =>
        getMixedResolutionRegionOrTracking(twid.target, site, when, lowResCadence)
          .map(_.map(t => (twid.id, t)))
      )
      .map(_.sequence.map(RegionOrTrackingMap.from(_)))
