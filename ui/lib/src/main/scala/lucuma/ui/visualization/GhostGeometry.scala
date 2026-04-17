// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.implicits.catsKernelOrderingForOrder
import lucuma.ags.AgsParams
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ghost
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

object GhostGeometry extends PwfsGeometry:

  def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (GhostScienceArea, ghost.scienceArea.fovAt(posAngle, offset)),
      (GhostIfu1PatrolField, ghost.GhostIfuPatrolField.ifu1PatrolFieldAt(posAngle, offset)),
      (GhostIfu2PatrolField, ghost.GhostIfuPatrolField.ifu2PatrolFieldAt(posAngle, offset))
    )

  override protected def candidatesAreaCss: Css = GhostCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.GhostIfu().withPWFS1
      case GuideProbe.PWFS2 => AgsParams.GhostIfu().withPWFS2
      case _                => AgsParams.GhostIfu()
