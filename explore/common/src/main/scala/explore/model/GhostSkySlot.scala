// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.model.Target
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.InstrumentSlot
import lucuma.schemas.model.SlotId

// GHOST-specific logic for the manually-assignable sky position on IFU2. This is the single
// source of truth shared by the Aladin clickable-region gating (`InteractiveRegion`) and the
// "Add Sky Position" menu entry (`SkyAssignmentState`), so the two can never disagree about
// when a sky position is assignable. Kept out of the instrument-agnostic
// `ConfigurationForVisualization` since it encodes GHOST's IFU layout and resolution semantics.
object GhostSkySlot:

  // IFU2 is free to hold a sky position, i.e. no science target is mapped to it.
  // Standard resolution can be one or two targets, so it qualifies only when IFU2 is not used
  // by a science target. High resolution is always a single science target on IFU1 with IFU2
  // reserved for the sky.
  def isIfu2AvailableForSky(viz: ConfigurationForVisualization): Boolean =
    viz.configuration match
      case _: BasicConfiguration.GhostIfu =>
        !viz.targetVisualization.slots.exists:
          case InstrumentSlot.Science(_, SlotId.GhostIfu2) => true
          case _                                           => false
      case _                              => false

  // A sky position may be manually assigned right now when:
  //  - IFU2 is free to hold a sky position (no science target mapped to it),
  //  - IFU1 holds a science target that is *not* a ToO. A ToO has no predictable coordinates
  //    to anchor the patrol field, so sky assignment is not offered when IFU1 is a ToO,
  //  - IFU2 doesn't already hold a sky position (or a second science target, in Standard res).
  // `isTargetOfOpportunity` resolves the ToO status of an IFU1 science target by id, so both
  // callers (the menu and the region) base the decision on the same configuration slots.
  def skySlotAvailable(
    viz:                  ConfigurationForVisualization,
    isTargetOfOpportunity: Target.Id => Boolean
  ): Option[SlotId] =
    val ifu1Science = viz.targetVisualization.slots.collectFirst:
      case InstrumentSlot.Science(tid, SlotId.GhostIfu1) => tid
    val ifu1Ready   = ifu1Science.exists(!isTargetOfOpportunity(_))
    val ifu2Taken   = viz.targetVisualization.slots.exists(_.slotId === SlotId.GhostIfu2)
    Option.when(isIfu2AvailableForSky(viz) && ifu1Ready && !ifu2Taken)(SlotId.GhostIfu2)
