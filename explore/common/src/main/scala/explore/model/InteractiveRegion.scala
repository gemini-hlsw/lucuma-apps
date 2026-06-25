// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.IO
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.react.common.Css
import lucuma.schemas.model.SlotId
import lucuma.ui.visualization.GhostGeometry
import lucuma.ui.visualization.VisualizationStyles

// A clickable area on the Aladin chart. The Aladin components consume these generically
// and know nothing about the instrument that produced them.
case class InteractiveRegion(
  slot:     SlotId,
  posAngle: Angle,
  shape:    ShapeExpression,
  shapeCss: Css,
  hoverCss: Css,
  onClick:  Coordinates => IO[Unit]
)

object InteractiveRegion:
  // All GHOST IFU2 knowledge lives here. Add a new `Option.when(...)` block (and a new
  // *Geometry shape) to support another instrument's sky slot — no Aladin component changes.
  def forViz(
    vizConf:    Option[ConfigurationForVisualization],
    obsCoords:  ObservationTargetsCoordinatesAt,
    selectedGS: Option[AgsAnalysis.Usable],
    assignSky:  Option[(SlotId, Coordinates) => IO[Unit]]
  ): List[InteractiveRegion] =
    (vizConf, assignSky).tupled.toList.flatMap: (viz, assign) =>
      val ifu1Assigned = obsCoords.slotCoords.contains(SlotId.GhostIfu1)
      val ifu2Assigned = obsCoords.slotCoords.contains(SlotId.GhostIfu2)
      // IFU2 is assignable only in GHOST Standard mode, once IFU1 is set (mode accepted)
      // and IFU2 has no sky position yet.
      Option
        .when(viz.isGhostStandard && ifu1Assigned && !ifu2Assigned):
          val pa = selectedGS.map(_.posAngle).getOrElse(viz.posAngle)
          InteractiveRegion(
            SlotId.GhostIfu2,
            pa,
            GhostGeometry.ifu2PatrolFieldShape(pa),
            VisualizationStyles.GhostIfu2PatrolField,
            VisualizationStyles.GhostIfu2PatrolFieldHovered,
            c => assign(SlotId.GhostIfu2, c)
          )
        .toList
