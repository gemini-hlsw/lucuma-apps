// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Epoch
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Gid
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import scala.collection.immutable.SortedMap

case class TargetWithId(
  id:              Target.Id,
  target:          Target,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) derives Eq {
  def toOptId: TargetWithOptId = TargetWithOptId(id.some, target, disposition, calibrationRole)

  def toSidereal: Option[SiderealTargetWithId] = TargetWithId.sidereal.getOption(this)

  def toNonSidereal: Option[NonsiderealTargetWithId] = TargetWithId.nonsidereal.getOption(this)
}

object TargetWithId {
  val id: Lens[TargetWithId, Target.Id]        = Focus[TargetWithId](_.id)
  val target: Lens[TargetWithId, Target]       = Focus[TargetWithId](_.target)
  val name: Lens[TargetWithId, NonEmptyString] = target.andThen(Target.name)

  val integratedBrightnesses
    : Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Integrated]]] =
    target.andThen(Target.integratedBrightnesses)

  val surfaceBrightnesses: Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Surface]]] =
    target.andThen(Target.surfaceBrightnesses)

  val sidereal: Prism[TargetWithId, SiderealTargetWithId] =
    Prism.partial[TargetWithId, SiderealTargetWithId] {
      case TargetWithId(id, t @ Target.Sidereal(_, _, _, _), disp, role) =>
        SiderealTargetWithId(id, t, disp, role)
    }(_.toTargetWithId)

  val nonsidereal: Prism[TargetWithId, NonsiderealTargetWithId] =
    Prism.partial[TargetWithId, NonsiderealTargetWithId] {
      case TargetWithId(id, t @ Target.Nonsidereal(_, _, _), disp, role) =>
        NonsiderealTargetWithId(id, t, disp, role)
    }(_.toTargetWithId)
}

case class TargetWithOptId(
  optId:           Option[Target.Id],
  target:          Target,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) derives Eq:
  def withId(targetId: Target.Id): TargetWithId =
    TargetWithId(targetId, target, disposition, calibrationRole)

object TargetWithOptId:
  def newScience(target: Target): TargetWithOptId =
    TargetWithOptId(none, target, TargetDisposition.Science, none)

case class SiderealTargetWithId(
  id:              Target.Id,
  target:          Target.Sidereal,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) derives Eq {
  def toTargetWithId = TargetWithId(id, target, disposition, calibrationRole)

  def at(i: Instant): SiderealTargetWithId = {
    val ldt            = LocalDateTime.ofInstant(i, ZoneOffset.UTC)
    val epoch          = Epoch.Julian.fromLocalDateTime(ldt).getOrElse(target.tracking.epoch)
    val trackingUpdate = (tracking: SiderealTracking) =>
      tracking.at(i).fold(tracking) { c =>
        val update = SiderealTracking.baseCoordinates.replace(c) >>> SiderealTracking.epoch
          .replace(epoch)
        update(tracking)
      }

    copy(target = Target.Sidereal.tracking.modify(trackingUpdate)(target))
  }
}

object SiderealTargetWithId:
  val id: Lens[SiderealTargetWithId, Target.Id]           = Focus[SiderealTargetWithId](_.id)
  val target: Lens[SiderealTargetWithId, Target.Sidereal] = Focus[SiderealTargetWithId](_.target)

case class NonsiderealTargetWithId(
  id:              Target.Id,
  target:          Target.Nonsidereal,
  disposition:     TargetDisposition,
  calibrationRole: Option[CalibrationRole]
) derives Eq {
  def toTargetWithId = TargetWithId(id, target, disposition, calibrationRole)
}
