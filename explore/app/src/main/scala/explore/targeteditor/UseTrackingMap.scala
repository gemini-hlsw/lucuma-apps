// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.ErrorMsgOr
import explore.model.ObservationTargets
import explore.model.RegionOrTrackingMap
import explore.model.reusability.given
import explore.utils.tracking.*
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
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
        targets match
          case None                                            =>
            RegionOrTrackingMap.Empty.asRight.pure[IO]
          case Some(ts) if ts.hasUnresolvedTargetOfOpportunity =>
            RegionOrTrackingMap.Empty.asRight.pure[IO]
          case Some(ts)                                        =>
            getMixedResolutionRegionOrTrackingMap(ts.allTargets.toList, site, at)
    .map(_.value.value.flatMap(_.fold(Pot.pending)(_.ready)))
