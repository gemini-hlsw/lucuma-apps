// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
