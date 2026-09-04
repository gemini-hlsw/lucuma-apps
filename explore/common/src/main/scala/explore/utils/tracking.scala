// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.math.Region
import lucuma.core.model.Ephemeris
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.ObservingNight
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.TargetResolution
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
  // The region a target falls back to when nothing better is known. Only an unresolved Target of
  // Opportunity gets here, and it always has a region, so the default is unreachable in practice.
  private def regionOf(target: Target): RegionOrTracking =
    RegionOrTracking.fromRegion(Target.region.getOption(target).getOrElse(Region.Full))

  private def getEphemerisTrackingForObservingNight(
    key:   Ephemeris.Key,
    site:  Site,
    night: ObservingNight
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[EphemerisTracking]] =
    val TotalPoints = 600
    key.horizonsKey.fold(
      "User defined Ephemeris Keys are not yet supported".asLeft.pure[IO]
    ): hk =>
      val interval = night.interval
      val start    = interval.lower.minus(Duration.ofHours(12))
      val end      = interval.upper.plus(Duration.ofHours(12))
      HorizonsWorkerClient[IO]
        .requestSingle:
          HorizonsMessage.EphemerisRequest(hk, site, start, end, TotalPoints)
        .map(_.getOrElse("Error calling HorizonsClient".asLeft))

  private def getEphemerisTrackingForSemester(
    key:      Ephemeris.Key,
    site:     Site,
    semester: Semester,
    cadence:  ElementsPerDay
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[EphemerisTracking]] =
    key.horizonsKey.fold(
      "User defined Ephemeris Keys are not yet supported".asLeft.pure[IO]
    ): hk =>
      val start = semester.start.atSite(site).toInstant
      val end   = semester.end.atSite(site).toInstant
      val days  = ChronoUnit.DAYS.between(start, end) + 1
      HorizonsWorkerClient[IO]
        .requestSingle:
          HorizonsMessage.AlignedEphemerisRequest(hk, site, start, days.toInt, cadence)
        .map(_.getOrElse("Error calling HorizonsClient".asLeft))

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  // Site and night are both only needed for non-sidereal targets
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Option[Site],
    night:  Option[ObservingNight]
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    // Keyed on how the target tracks rather than on which subtype it is, so that a resolved
    // Target of Opportunity behaves as whatever it resolved to. Only an unresolved one has no
    // tracking, and that is the None case.
    target.resolution match
      case Some(TargetResolution.Nonsidereal(key)) =>
        (site, night) match
          case (Some(site), Some(night)) =>
            getEphemerisTrackingForObservingNight(key, site, night).map:
              _.map(RegionOrTracking.fromTracking)
          case (None, _)                 =>
            "No site is known. This is likely a missing observing mode.".asLeft.pure[IO]
          case (_, None)                 =>
            "No observing night is known. This is likely a missing observing mode.".asLeft.pure[IO]
      case Some(TargetResolution.Sidereal(t, _))   => RegionOrTracking.fromTracking(t).asRight.pure
      case None                                    => regionOf(target).asRight.pure

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Option[Site],
    when:   Instant
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    val night = site.map(ObservingNight.fromSiteAndInstant(_, when))
    getRegionOrTrackingForObservingNight(target, site, night)

  // Gets high resolution tracking for the observing night. In order to maximize cache hits and
  // be useable for both Night and 2H elevation plots, it pads by 12 hours on each end.
  def getRegionOrTrackingForObservingNight(
    target: Target,
    site:   Site,
    when:   LocalDate
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    val night = ObservingNight.fromSiteAndLocalDate(site, when)
    getRegionOrTrackingForObservingNight(target, site.some, night.some)

  def getRegionOrTrackingMapForObservingNight(
    targetWithIds: List[TargetWithId],
    site:          Option[Site],
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
    target.resolution match
      case Some(TargetResolution.Nonsidereal(key)) =>
        getEphemerisTrackingForSemester(key, site, semester, cadence).map(
          _.map(RegionOrTracking.fromTracking)
        )
      case Some(TargetResolution.Sidereal(t, _))   => RegionOrTracking.fromTracking(t).asRight.pure
      case None                                    => regionOf(target).asRight.pure

  // Combines the high-res ephemeris from getRegionOrTrackingForObservingNight with
  // the low-res ephemeris from getRegionOrTrackingForSemester.
  def getMixedResolutionRegionOrTracking(
    target:        Target,
    site:          Site,
    when:          Instant,
    lowResCadence: ElementsPerDay = 2
  )(using WorkerClient[IO, HorizonsMessage.Request]): IO[ErrorMsgOr[RegionOrTracking]] =
    target.resolution match
      case Some(TargetResolution.Nonsidereal(key)) =>
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

      case Some(TargetResolution.Sidereal(t, _)) => RegionOrTracking.fromTracking(t).asRight.pure
      case None                                  => regionOf(target).asRight.pure

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
