// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.derived.*
import explore.model.ErrorMsgOr
import explore.model.RegionOrCoordinatesAt
import explore.targets.MotionCorrectedTarget
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Target
import lucuma.schemas.model.SlotId

// Rows for the targets table that supports showing both targets and sky positions.
enum AsterismRow derives Eq:
  case TargetRow(value: MotionCorrectedTarget)
  case SkyRow(slot: SlotId, coords: Option[ErrorMsgOr[RegionOrCoordinatesAt]])

  // Stable id used by the table's getRowId usable for targets and sky pos
  def rowKey: String = this match
    case TargetRow(mct)  => mct.id.toString
    case SkyRow(slot, _) => s"sky-${slot.tag}"

  def location: Option[ErrorMsgOr[RegionOrCoordinatesAt]] = this match
    case TargetRow(mct) => mct.regionOrCoords
    case SkyRow(_, c)   => c

  def toSelection: AsterismSelection = this match
    case TargetRow(mct)  => AsterismSelection.Target(mct.id)
    case SkyRow(slot, _) => AsterismSelection.Sky(slot)

object AsterismRow:
  given Reusability[AsterismRow] = Reusability.byEq

enum AsterismSelection derives Eq:
  case Target(id: lucuma.core.model.Target.Id)
  case Sky(slot: SlotId)

object AsterismSelection:
  given Reusability[AsterismSelection] = Reusability.byEq
