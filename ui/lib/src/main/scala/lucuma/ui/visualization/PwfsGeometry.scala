// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.implicits.catsKernelOrderingForOrder
import lucuma.ags.AgsAnalysis
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

/**
 * Geometry methods for PWFS-only instruments
 */
trait PwfsGeometry extends WithPwfsGeometry:

  def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression]

  protected def candidatesAreaCss: Css

  protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams

  protected def posAngle(
    gs:               Option[AgsAnalysis.Usable],
    fallbackPosAngle: Option[Angle]
  ): Option[Angle] =
    gs.map(_.posAngle).orElse(fallbackPosAngle)

  def instrumentGeometry(
    referenceCoordinates:    Coordinates,
    fallbackPosAngle:        Option[Angle],
    guideProbe:              Option[GuideProbe],
    gs:                      Option[AgsAnalysis.Usable],
    patrolFieldIntersection: Option[ShapeExpression],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    posAngle(gs, fallbackPosAngle)
      .map: posAngle =>
        val candidatesArea: SortedMap[Css, ShapeExpression] =
          guideProbe match
            case Some(GuideProbe.PWFS1 | GuideProbe.PWFS2) =>
              pwfsCandidatesArea(candidatesAreaCss, posAngle, candidatesVisibilityCss)
            case Some(GuideProbe.AltairAOWFS)              =>
              altairCandidatesArea(candidatesAreaCss, posAngle, candidatesVisibilityCss)
            case _                                         =>
              SortedMap.empty

        val baseShapes = shapesForMode(posAngle, Offset.Zero) ++ candidatesArea

        val probe = gs.map: gs =>
          val gsOffset   = referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset
          val probeShape = guideProbe match
            case Some(p @ (GuideProbe.PWFS1 | GuideProbe.PWFS2)) =>
              pwfsProbeShapes(p, gsOffset, Offset.Zero)
            case _                                               =>
              SortedMap.empty[Css, ShapeExpression]

          // The intersection over all offsets is evaluated by the AGS worker, not here.
          patrolFieldIntersection.fold(probeShape)(pf =>
            probeShape + (PatrolFieldIntersection -> pf)
          )

        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
