// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.GuidedOffset
import lucuma.ags.ScienceOffsets
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
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
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    posAngle(gs, fallbackPosAngle)
      .map: posAngle =>
        val candidatesArea: SortedMap[Css, ShapeExpression] =
          conf.map(_.guideProbe(trackType)) match
            case Some(GuideProbe.PWFS1 | GuideProbe.PWFS2) =>
              pwfsCandidatesArea(candidatesAreaCss, posAngle, candidatesVisibilityCss)
            case _                                         =>
              SortedMap.empty

        val baseShapes = shapesForMode(posAngle, Offset.Zero) ++ candidatesArea

        val probe = gs.map: gs =>
          val gsOffset   = referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset
          val probeShape = conf.map(_.guideProbe(trackType)) match
            case Some(p @ (GuideProbe.PWFS1 | GuideProbe.PWFS2)) =>
              pwfsProbeShapes(p, gsOffset, Offset.Zero)
            case _                                               =>
              SortedMap.empty[Css, ShapeExpression]

          val positions = Ags.generatePositions(
            referenceCoordinates.some,
            blindOffset,
            NonEmptyList.one(posAngle),
            none,
            scienceOffsets.map(ScienceOffsets.apply)
          )

          val patrolFieldIntersection =
            conf
              .map(c => agsParamsFor(c.guideProbe(trackType)))
              .map: params =>
                val calcs = params.posCalculations(positions.value.toNonEmptyList)
                PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

          patrolFieldIntersection.fold(probeShape)(probeShape + _)

        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
