// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.IO
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.react.common.Css
import lucuma.schemas.model.SlotId
import lucuma.ui.visualization.GhostGeometry
import lucuma.ui.visualization.VisualizationStyles

// A clickable area on the Aladin area
case class InteractiveRegion(
  slot:     SlotId,
  posAngle: Angle,
  shape:    ShapeExpression,
  shapeCss: Css,
  hoverCss: Css,
  onClick:  Coordinates => IO[Unit]
)

object InteractiveRegion:
  // ghost only for now but we could add more regions for other instruments
  def forViz(
    vizConf:               Option[ConfigurationForVisualization],
    isTargetOfOpportunity: Target.Id => Boolean,
    selectedGS:            Option[AgsAnalysis.Usable],
    assignSky:             Option[(SlotId, Coordinates) => IO[Unit]]
  ): List[InteractiveRegion] =
    (vizConf, assignSky).tupled.toList.flatMap: (viz, assign) =>
      GhostSkySlot
        .skySlotAvailable(viz, isTargetOfOpportunity)
        .map: slot =>
          val pa = selectedGS.map(_.posAngle).getOrElse(viz.posAngle)
          InteractiveRegion(
            slot,
            pa,
            GhostGeometry.ifu2PatrolFieldShape(pa),
            VisualizationStyles.GhostIfu2PatrolField,
            VisualizationStyles.GhostIfu2PatrolFieldHovered,
            assign(slot, _)
          )
        .toList
