// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.Target
import lucuma.core.util.Gid
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedMap

trait TargetWithMetadata:
  val target: Target
  val disposition: TargetDisposition

  // Both of these MUST stay lazy. Implementors are free to satisfy `target` with a `val` in the
  // class body rather than a constructor parameter -- `MotionCorrectedTarget` and
  // `TargetSearchResult` both do -- and a trait's initializer runs before those are assigned, so
  // an eager val here reads a null `target`.

  /**
   * Is this a Target of Opportunity? True whether or not the alert has arrived: a ToO keeps its
   * identity, and its approved region, once resolved. Ask this for questions about policy - may it
   * be searched for, may it share an asterism, may the observation interrupt others.
   */
  lazy val isTargetOfOpportunity: Boolean = Target.opportunity.getOption(target).isDefined

  /**
   * Is this a Target of Opportunity that is still waiting for its alert? Only an unresolved one has
   * no tracking at all, so this is the question to ask before anything that needs a position -
   * coordinates, plots, guide stars, sky slots.
   */
  lazy val isUnresolvedTargetOfOpportunity: Boolean = target.resolution.isEmpty

case class TargetWithId(
  id:              Target.Id,
  target:          Target,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) extends TargetWithMetadata derives Eq:
  def toOptId: TargetWithOptId = TargetWithOptId(id.some, target, disposition, calibrationRole)

  def isReadonlyForProgramType(programType: ProgramType): Boolean =
    disposition match
      case TargetDisposition.Calibration => programType =!= ProgramType.System
      case _                             => false

object TargetWithId:
  val id: Lens[TargetWithId, Target.Id]        = Focus[TargetWithId](_.id)
  val target: Lens[TargetWithId, Target]       = Focus[TargetWithId](_.target)
  val name: Lens[TargetWithId, NonEmptyString] = target.andThen(Target.name)

  val integratedBrightnesses
    : Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Integrated]]] =
    target.andThen(Target.integratedBrightnesses)

  val surfaceBrightnesses: Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Surface]]] =
    target.andThen(Target.surfaceBrightnesses)

case class TargetWithOptId(
  optId:           Option[Target.Id],
  target:          Target,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) extends TargetWithMetadata derives Eq:
  def withId(targetId: Target.Id): TargetWithId =
    TargetWithId(targetId, target, disposition, calibrationRole)

object TargetWithOptId:
  def newScience(target: Target): TargetWithOptId =
    TargetWithOptId(none, target, TargetDisposition.Science, none)
