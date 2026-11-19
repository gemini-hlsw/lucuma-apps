// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
import lucuma.core.enums.GuideProbe
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId

/**
 * The observation's own targets as guide stars, for the probes that guide on axis (Altair AOWFS).
 */
object ObservationGuideStars:

  // As in the ODB, every target of the asterism: science and calibration, not the blind offset.
  private def scienceTargets(targets: ObservationTargets): List[TargetWithId] =
    targets.science

  /** How many targets the observation has, to tell whether an id-less selection is ambiguous. */
  def count(targets: Option[ObservationTargets]): Int =
    targets.foldMap(scienceTargets(_).length)

  /** The science targets' names, to label them as guide stars. */
  def names(targets: ObservationTargets): Map[Target.Id, NonEmptyString] =
    scienceTargets(targets).map(twid => twid.id -> twid.target.name).toMap

  /**
   * The science targets as guide star candidates. A sidereal target keeps its tracking; a
   * non-sidereal one is a fixed candidate at its position at the observation time, given by
   * `coordinatesAt`, and is skipped without one. Opportunity targets have no position.
   */
  def candidates(
    targets:       ObservationTargets,
    coordinatesAt: Target.Id => Option[Coordinates]
  ): List[GuideStarCandidate] =
    scienceTargets(targets).flatMap: twid =>
      val tracking: Option[SiderealTracking] =
        twid.target match
          case Target.Sidereal(tracking = tracking) => tracking.some
          case Target.Nonsidereal(name = _)         =>
            coordinatesAt(twid.id).map(SiderealTracking.const)
          case Target.Opportunity(name = _)         => none
      tracking.map(GuideStarCandidate.observationTarget(twid.id, _, twid.target.sourceProfile))

  /** The candidates for a probe: none unless the probe guides on axis. */
  def candidatesFor(
    probe:         Option[GuideProbe],
    targets:       Option[ObservationTargets],
    coordinatesAt: Target.Id => Option[Coordinates]
  ): List[GuideStarCandidate] =
    (probe.filter(_.guidesOnAxis), targets).tupled.foldMap((_, ts) => candidates(ts, coordinatesAt))

  /**
   * What to store when a candidate is picked by hand. With a single science target the id is
   * dropped, so the selection survives the target being replaced (e.g. cloned on edit).
   */
  def selectionToStore(picked: GuideStarId, targets: Option[ObservationTargets]): GuideStarId =
    picked match
      case GuideStarId.ObservationTarget(Some(_)) if count(targets) === 1 =>
        GuideStarId.ObservationTarget(none)
      case other                                                          => other
