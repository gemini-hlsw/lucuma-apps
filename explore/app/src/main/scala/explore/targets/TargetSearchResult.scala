// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Eq
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithMetadata
import lucuma.schemas.model.TargetWithOptId
import monocle.Focus
import monocle.Lens
import monocle.Optional

case class TargetSearchResult(
  targetWithOptId: TargetWithOptId,
  angularSize:     Option[AngularSize]
) extends TargetWithMetadata:
  val optId: Option[Target.Id]                 = targetWithOptId.optId
  val target: Target                           = targetWithOptId.target
  val disposition: TargetDisposition           = targetWithOptId.disposition
  val calibrationRole: Option[CalibrationRole] = targetWithOptId.calibrationRole

object TargetSearchResult:
  def fromCatalogTargetResult(r: CatalogTargetResult): TargetSearchResult =
    TargetSearchResult(
      TargetWithOptId.newScience(r.target),
      r.angularSize
    )

  given Eq[TargetSearchResult] =
    Eq.by(t =>
      Target.catalogInfo
        .getOption(t.target)
        .flatten
        .map(CatalogInfo.id.get)
        .map(_.value)
        .orElse(Target.ephemerisKey.getOption(t.target).map(_.toString))
        .getOrElse(Target.name.get(t.target).value)
    )

  given Reusability[TargetSearchResult] =
    Reusability.byEq

  val targetWithOpId: Lens[TargetSearchResult, TargetWithOptId] =
    Focus[TargetSearchResult](_.targetWithOptId)

  val target: Lens[TargetSearchResult, Target] =
    targetWithOpId.andThen(Focus[TargetWithOptId](_.target))

  val siderealTracking: Optional[TargetSearchResult, SiderealTracking] =
    target.andThen(Target.siderealTracking)
