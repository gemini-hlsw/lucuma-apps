// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.hooks.*
import crystal.react.syntax.pot.given
import explore.model.AppContext
import explore.model.ErrorMsgOr
import explore.model.ObservationTargets
import explore.model.ObservationTargetsCoordinatesAt
import explore.model.RegionOrTrackingMap
import explore.model.reusability.given
import explore.utils.tracking.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Tracking
import lucuma.schemas.model.TargetVisualization
import lucuma.ui.reusability.given

import java.time.Instant

object UseTrackingMap:
  /**
   * Tracking for every target of an observation: high resolution around the observing night, low
   * resolution over the semester so Aladin can pan. The component at the top of a screen
   * (ObsTabTiles or a target tab tile) calls this once and passes the map down, so the average PA
   * and Aladin are computed from the same positions. The previous map is kept while recomputing so
   * Aladin does not unmount on every edit. Without a site, non-sidereal targets yield an error
   * rather than an ephemeris for a guessed site.
   */
  def useTrackingMap(
    targets: Option[ObservationTargets],
    site:    Option[Site],
    obsTime: Option[Instant]
  )(ctx: AppContext[IO]): HookResult[Pot[ErrorMsgOr[RegionOrTrackingMap]]] =
    import ctx.given

    useEffectKeepResultWithDeps((targets, site, obsTime)): (targets, site, obsTime) =>
      obsTime.traverse: at =>
        // An unresolved ToO has nothing to track.
        targets
          .filterNot(_.hasUnresolvedTargetOfOpportunity)
          .fold(RegionOrTrackingMap.Empty.asRight.pure[IO]): ts =>
            getMixedResolutionRegionOrTrackingMap(ts.allTargets.toList, site, at)
    .map(_.value.value.flatMap(_.fold(Pot.pending)(_.ready)))

  /**
   * Tracking of the asterism as a whole, i.e. the base position over time. `None` while the
   * tracking map is unavailable or when the asterism has an unresolved ToO.
   */
  def useAsterismTracking(
    targets:     Option[ObservationTargets],
    trackingMap: Pot[ErrorMsgOr[RegionOrTrackingMap]]
  ): HookResult[Reusable[Option[Tracking]]] =
    useMemo((targets, trackingMap.toOption.flatMap(_.toOption))): (targets, trackings) =>
      // We should have trackings for all the targets, so we'll ignore errors here.
      (targets, trackings).flatMapN(_.optAsterismTracking(_))

  /**
   * Positions of everything an observation points at, at `obsTime`: the targets, the base, the
   * blind offset and the instrument slots.
   */
  def useObsTargetsCoords(
    targets:      Option[ObservationTargets],
    obsTime:      Option[Instant],
    trackingMap:  Pot[ErrorMsgOr[RegionOrTrackingMap]],
    targetViz:    Option[TargetVisualization],
    explicitBase: Option[Coordinates]
  ): HookResult[Reusable[Pot[ErrorMsgOr[ObservationTargetsCoordinatesAt]]]] =
    useMemo((targets, obsTime, trackingMap, targetViz, explicitBase)):
      (targets, obsTime, trPot, targetViz, explicitBase) =>
        (targets, obsTime).tupled.fold(Pot.pending): (ts, at) =>
          trPot.map: tr =>
            if (ts.hasUnresolvedTargetOfOpportunity)
              ObservationTargetsCoordinatesAt.emptyAt(at)
            else
              // Generic instrument slot layout, resolved to obs-time coords inside
              // ObservationTargetsCoordinatesAt alongside base/blind-offset coords.
              tr.flatMap: map =>
                ObservationTargetsCoordinatesAt(at,
                                                ts,
                                                map,
                                                targetViz.foldMap(_.slots),
                                                explicitBase
                )
