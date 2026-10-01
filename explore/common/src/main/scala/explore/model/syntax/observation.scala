// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.model.AveragePABasis
import explore.model.Execution
import explore.model.Observation
import explore.model.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostIfuMappingSyntax.*
import lucuma.core.model.sequence.ghost.IfuMappingContext
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.TargetVisualization
import lucuma.schemas.model.TargetWithId
import lucuma.ui.visualization.GhostGeometry

import java.time.Instant
import java.time.temporal.ChronoUnit

/**
 * Derivations from an observation that need a time or the asterism tracking, so they cannot be
 * plain members of `Observation`.
 */
object observation:
  // The ODB assumes the same visit length for an Altair observation that has no sequence yet.
  private val NominalAltairVisitDuration: TimeSpan =
    TimeSpan.unsafeFromDuration(1, ChronoUnit.HOURS)

  extension (o: Observation)
    // The explicit duration if set, else the remaining time from the digest.
    def obsDuration: Option[TimeSpan] =
      o.observationDuration.orElse(o.execution.digest.remainingObsTime.value)

    // The duration AGS and the average parallactic angle work with. An Altair sequence cannot be
    // generated before a guide star is picked, so until then Altair observations get a nominal
    // duration, or AGS would never run. Display keeps `obsDuration`, which has no such fiction.
    def agsObsDuration: Option[TimeSpan] =
      o.obsDuration.orElse(Option.when(o.altair.isDefined)(NominalAltairVisitDuration))

    def acqConfigs: Option[NonEmptySet[TelescopeConfig]] =
      NonEmptySet.fromSet(Execution.acqConfigs.getOption(o.execution).orEmpty)

    def sciConfigs: Option[NonEmptySet[TelescopeConfig]] =
      NonEmptySet.fromSet(Execution.sciConfigs.getOption(o.execution).orEmpty)

    // The IFU mapping for ghost. TODO: Add support for explicit base
    def ghostIfuMapping(
      scienceTargets: List[TargetWithId],
      obsTime:        Instant
    ): Option[GhostIfuMapping] =
      o.observingMode.toOption.flatten match
        case Some(ghost: ObservingMode.GhostIfu) =>
          val ctx = IfuMappingContext(
            ghost.resolutionMode,
            ghost.skyPosition,
            o.posAngleConstraint,
            none,
            Timestamp.fromInstantTruncatedAndBounded(obsTime)
          )

          // Whether `sky` is within the minimum IFU-arm separation of any science target.
          def tooCloseToScience(sky: Coordinates): Boolean =
            scienceTargets.exists: t =>
              Target.siderealTracking
                .getOption(t.target)
                .flatMap(_.at(obsTime))
                .exists(GhostGeometry.tooClose(_, sky))

          GhostIfuMapping.derive(ctx, scienceTargets.map(t => (t.id, t.target))) match
            case Right(mapping)                                         =>
              mapping.some
            // Derivation fails when the sky is too close to the science target.
            // Fall back to TargetPlusSky so the sky marker stays visible and
            // the keep-out zone can flag it
            case Left(_) if ghost.skyPosition.exists(tooCloseToScience) =>
              (scienceTargets.headOption.map(_.id), ghost.skyPosition)
                .mapN(GhostIfuMapping.TargetPlusSky.apply)
            case Left(_)                                                =>
              none
        case _                                   => none

    def targetVisualization(
      scienceTargets: List[TargetWithId],
      obsTime:        Instant
    ): TargetVisualization =
      o.basicConfiguration
        .map(_.targetVisualization(scienceTargets, o.ghostIfuMapping(scienceTargets, obsTime)))
        .getOrElse(TargetVisualization.Empty)

    // The science part of the observation, i.e. after setup: site, base tracking, start and
    // duration. None without a sequence, since the setup time comes from the digest.
    def scienceWindow(
      baseTracking: Option[Tracking],
      obsTime:      Instant
    ): Option[(Site, Tracking, Instant, TimeSpan)] =
      (o.site, baseTracking, o.agsObsDuration, o.execution.digest.fullSetupTime.value)
        .flatMapN: (site, tracking, fullDuration, setupDuration) =>
          fullDuration
            .subtract(setupDuration)
            .filter(_ > TimeSpan.Zero)
            .map: scienceDuration =>
              (site,
               tracking,
               obsTime.plusNanos(setupDuration.toMicroseconds * 1000),
               scienceDuration
              )

    // Average PA over the science part of the observation, i.e. after setup.
    def averagePA(baseTracking: Option[Tracking], obsTime: Instant): Option[AveragePABasis] =
      if o.posAngleConstraint =!= PosAngleConstraint.AverageParallactic then none
      else
        o.scienceWindow(baseTracking, obsTime)
          .flatMap: (site, tracking, scienceStart, scienceDuration) =>
            averageParallacticAngle(site.place, tracking, scienceStart, scienceDuration)
              .map(AveragePABasis(scienceStart, scienceDuration, _))

    // The average parallactic angle is required and its inputs are known, yet it cannot be
    // computed: the target is below the horizon over the science window.
    def targetNotObservable(baseTracking: Option[Tracking], obsTime: Instant): Boolean =
      o.posAngleConstraint === PosAngleConstraint.AverageParallactic &&
        o.scienceWindow(baseTracking, obsTime).isDefined &&
        o.averagePA(baseTracking, obsTime).isEmpty
