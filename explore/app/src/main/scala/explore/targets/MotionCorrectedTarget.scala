// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import explore.model.ErrorMsgOr
import explore.model.RegionOrCoordinatesAt
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithMetadata

case class MotionCorrectedTarget(
  targetWithId:   TargetWithId,
  regionOrCoords: Option[ErrorMsgOr[RegionOrCoordinatesAt]]
) extends TargetWithMetadata:
  val id: Target.Id                  = targetWithId.id
  val target: Target                 = targetWithId.target
  val disposition: TargetDisposition = targetWithId.disposition
