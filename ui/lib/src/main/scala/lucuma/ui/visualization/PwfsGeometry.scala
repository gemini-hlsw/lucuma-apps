// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.pwfs
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

trait PwfsGeometry:

  protected def pwfsCandidatesArea(
    baseCss:  Css,
    posAngle: Angle,
    extraCss: Css
  ): SortedMap[Css, ShapeExpression] =
    SortedMap((baseCss |+| extraCss, pwfs.patrolField.patrolFieldAt(posAngle, Offset.Zero)))

  protected def pwfsProbeShapes(
    probe:           GuideProbe,
    guideStarOffset: Offset,
    offsetPos:       Offset
  ): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (PwfsArm, pwfs.probeArm.armAt(probe, guideStarOffset, offsetPos)),
      (PwfsArmVignetted, pwfs.probeArm.armVignettedAreaAt(probe, guideStarOffset, offsetPos)),
      (PwfsMirror, pwfs.probeArm.mirrorAt(probe, guideStarOffset, offsetPos)),
      (PwfsMirrorVignetted, pwfs.probeArm.mirrorVignettedAreaAt(probe, guideStarOffset, offsetPos))
    )
