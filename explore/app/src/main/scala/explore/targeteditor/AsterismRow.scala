// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.derived.*
import explore.model.ErrorMsgOr
import explore.model.RegionOrCoordinatesAt
import explore.targets.MotionCorrectedTarget
import lucuma.schemas.model.SlotId
import lucuma.core.model.Target

// Rows for the targets table that supports showing both targets and sky positions.
enum AsterismRow derives Eq:
  case TargetRow(value: MotionCorrectedTarget)
  case SkyRow(slot: SlotId, coords: Option[ErrorMsgOr[RegionOrCoordinatesAt]])

  // Stable id used by the table's getRowId usable for targets and sky pos
  def toSelection: AsterismSelection = this match
    case TargetRow(mct)  => AsterismSelection.Target(mct.id)
    case SkyRow(slot, _) => AsterismSelection.Sky(slot)

  def asAsterismSelection: AsterismSelection = toSelection

  def asAsterismRow: AsterismRow = this

enum AsterismSelection derives Eq:
  case Target(id: Target.Id)
  case Sky(slot: SlotId)
