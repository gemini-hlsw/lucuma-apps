// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.ObservingModeType
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.react.common.Css
import lucuma.schemas.model.SlotId
import lucuma.ui.visualization.GhostGeometry
import lucuma.ui.visualization.VisualizationStyles

// A clickable area on the Aladin area
case class InteractiveRegion(
  slot:             SlotId,
  posAngle:         Angle,
  shape:            ShapeExpression,
  shapeCss:         Css,
  hoverCss:         Css,
  exclusionOffsets: List[Offset],
  onClick:          Coordinates => IO[Unit]
)

object InteractiveRegion:
  // The sky keep-out zone(s) (e.g. GHOST's 102" minimum IFU arm separation around each science
  // target), one shape per science target so overlapping disks stay visually distinct.
  def skyKeepOutZone(
    vizConf:  Option[ConfigurationForVisualization],
    coordsAt: ObservationTargetsCoordinatesAt
  ): Option[NonEmptyList[(Css, ShapeExpression)]] =
    for
      conf  <- vizConf if conf.configuration.obsModeType === ObservingModeType.GhostIfu
      shapes = GhostGeometry.skyExclusionShapes(coordsAt.scienceOffsetsFromBase)
      nel   <- NonEmptyList.fromList(shapes)
    yield nel.map(VisualizationStyles.GhostSkyExclusionZone -> _)

  // Whether two science targets (e.g. GHOST dual-target mode) are too close to each other.
  def scienceTargetsTooClose(coordsAt: ObservationTargetsCoordinatesAt): Boolean =
    GhostGeometry.anyTooClose(coordsAt.scienceOffsetsFromBase)

  // ghost only for now but we could add more regions for other instruments
  def forViz(
    vizConf:               Option[ConfigurationForVisualization],
    coordsAt:              ObservationTargetsCoordinatesAt,
    isTargetOfOpportunity: Target.Id => Boolean,
    selectedGS:            Option[AgsAnalysis.Usable],
    assignSky:             Option[(SlotId, Coordinates) => IO[Unit]]
  ): List[InteractiveRegion] =
    (vizConf, assignSky).tupled.toList.flatMap: (viz, assign) =>
      GhostSkySlot
        .skySlotAvailable(viz, isTargetOfOpportunity)
        .map: slot =>
          val pa      = selectedGS.map(_.posAngle).getOrElse(viz.posAngle)
          val offsets = coordsAt.scienceOffsetsFromBase

          InteractiveRegion(
            slot,
            pa,
            GhostGeometry.ifu2SkySlotShape(pa, offsets),
            VisualizationStyles.GhostIfu2PatrolField,
            VisualizationStyles.GhostIfu2PatrolFieldHovered,
            offsets,
            assign(slot, _)
          )
        .toList
