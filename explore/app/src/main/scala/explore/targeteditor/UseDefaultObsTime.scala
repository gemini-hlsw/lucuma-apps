// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.hooks.*
import explore.events.HorizonsMessage
import explore.model.AppContext
import explore.model.ObservationTargets
import explore.model.RegionOrTrackingMap
import explore.model.reusability.given
import explore.utils.obsTimeOrDefault
import explore.utils.siderealBaseTracking
import explore.utils.tracking.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import lucuma.ui.reusability.given
import workers.WorkerClient

import java.time.Instant

object UseDefaultObsTime:

  /**
   * The time to display an observation at: its explicit time, or else the next transit of its
   * base at the site. Sidereal asterisms (or an explicit base) resolve synchronously. Nonsidereal
   * ones are pending while the semester ephemeris loads, so that the observing night ephemeris is
   * fetched only once, around the transit. Falls back to the start of the current UTC day.
   */
  def useDefaultObsTime(
    targets:      Option[ObservationTargets],
    site:         Option[Site],
    explicitTime: Option[Instant],
    explicitBase: Option[Coordinates]
  )(ctx: AppContext[IO]): HookResult[Pot[Instant]] =
    import ctx.given

    for
      syncTime    <-
        useMemo((explicitTime, site, siderealBaseTracking(targets, explicitBase))):
          (explicitTime, site, baseTracking) =>
            Option.when(explicitTime.isDefined || baseTracking.isDefined):
              obsTimeOrDefault(explicitTime, site, baseTracking)
      transitTime <-
        useEffectKeepResultWithDeps((syncTime.value.isEmpty, targets, site)):
          (needsTransit, targets, site) =>
            // An unresolved ToO has nothing to transit.
            targets
              .filter(_ => needsTransit)
              .filterNot(_.hasUnresolvedTargetOfOpportunity)
              .product(site)
              .flatTraverse(nextTransit)
              .handleError(_ => none)
    yield syncTime.value match
      case Some(time) => time.ready
      case None       => transitTime.value.value.map(_.getOrElse(obsTimeOrDefault(none)))

  // The semester ephemeris depends only on the site and semester, not on the observation time.
  private def nextTransit(targets: ObservationTargets, site: Site)(using
    WorkerClient[IO, HorizonsMessage.Request]
  ): IO[Option[Instant]] =
    IO(Instant.now()).flatMap: now =>
      val semester: Semester = semesterAt(site, now)
      targets.science
        .traverse: targetWithId =>
          getRegionOrTrackingForSemester(targetWithId.target, site, semester)
            .map(_.map(targetWithId.id -> _))
        .map: trackings =>
          trackings.sequence
            .map(RegionOrTrackingMap.from)
            .toOption
            .flatMap(targets.optAsterismTracking)
            .flatMap(_.timeOrNextTransit(site, none, now))
