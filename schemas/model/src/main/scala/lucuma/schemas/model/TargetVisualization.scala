// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.util.Enumerated

enum SlotId(val tag: String, val shortName: String) derives Enumerated:
  case GhostIfu1 extends SlotId("ghostIfu1", "IFU1")
  case GhostIfu2 extends SlotId("ghostIfu2", "IFU2")
  case Base      extends SlotId("base", "Base")

  def isBase: Boolean = this match
    case Base => true
    case _    => false

  // How the position is named in the targets table and its coordinate editor.
  def positionLabel: String = this match
    case Base => "Base Position"
    case _    => s"$shortName Sky"

// A slot can be a science target or sky, identified by its slot id.
enum InstrumentSlot derives Eq:
  case Science(targetId: Target.Id, id: SlotId)
  case Sky(coordinates: Coordinates, id: SlotId)

  def slotId: SlotId = this match
    case Science(_, id) => id
    case Sky(_, id)     => id

case class TargetVisualization(
  slots:       List[InstrumentSlot],
  labelPrefix: Option[String]
) derives Eq:
  private def labelFor(id: SlotId): String =
    labelPrefix.fold(id.shortName)(p => s"$p-${id.shortName}")

  def labels: Map[Target.Id, String] =
    slots
      .collect:
        case InstrumentSlot.Science(tid, id) => tid -> labelFor(id)
      .toMap

  // Sky positions with labels
  def skyLabels: List[(Coordinates, String)] =
    slots.collect:
      case InstrumentSlot.Sky(c, id) => (c, labelFor(id))

object TargetVisualization:
  val Empty: TargetVisualization = TargetVisualization(Nil, None)
