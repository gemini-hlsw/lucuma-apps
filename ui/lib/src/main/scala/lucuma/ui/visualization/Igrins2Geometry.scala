// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.igrins2.scienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

object Igrins2Geometry extends PwfsGeometry:

  def shapesForMode(
    posAngle: Angle,
    offset:   Offset
  ): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (Igrins2SvcFov, scienceArea.svcFieldOfView(posAngle, offset)),
      (Igrins2ScienceSlit, scienceArea.scienceSlitFOV(posAngle, offset))
    )

  def commonShapes(
    posAngle:      Angle,
    extraCss:      Css,
    configuration: Option[BasicConfiguration],
    trackType:     Option[TrackType]
  ): SortedMap[Css, ShapeExpression] =
    configuration
      .map: c =>
        c.guideProbe(trackType) match
          case GuideProbe.PWFS2 | GuideProbe.PWFS1 =>
            pwfsCandidatesArea(Flamingos2CandidatesArea, posAngle, extraCss)
          case _                                   =>
            SortedMap.empty[Css, ShapeExpression]
      .getOrElse(SortedMap.empty[Css, ShapeExpression])

  def probeShapes(
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[BasicConfiguration],
    trackType:       Option[TrackType]
  ): SortedMap[Css, ShapeExpression] =
    mode match
      case Some(m: BasicConfiguration.Igrins2LongSlit.type) =>
        m.guideProbe(trackType) match
          case p @ (GuideProbe.PWFS1 | GuideProbe.PWFS2) =>
            pwfsProbeShapes(p, guideStarOffset, offsetPos)
          case _                                         =>
            SortedMap.empty
      case _                                                =>
        SortedMap.empty

  def igrins2Geometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    gs.map(_.posAngle)
      .orElse(fallbackPosAngle)
      .map { posAngle =>
        val baseShapes: SortedMap[Css, ShapeExpression] =
          shapesForMode(posAngle, Offset.Zero) ++
            commonShapes(posAngle, candidatesVisibilityCss, conf, trackType)

        val probe = gs
          .map: gs =>
            val gsOffset =
              referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset

            val probeShape =
              probeShapes(gsOffset, Offset.Zero, conf, trackType)

            val positions = Ags.generatePositions(
              referenceCoordinates.some,
              blindOffset,
              NonEmptyList.one(posAngle),
              none,
              scienceOffsets.map(ScienceOffsets.apply)
            )

            val agsParams =
              conf.map: c =>
                c.guideProbe(trackType) match
                  case GuideProbe.PWFS1 =>
                    AgsParams.Igrins2LongSlit().withPWFS1
                  case GuideProbe.PWFS2 =>
                    AgsParams.Igrins2LongSlit().withPWFS2
                  case _                =>
                    AgsParams.Igrins2LongSlit()

            val patrolFieldIntersection =
              agsParams.map: params =>
                val calcs = params.posCalculations(positions.value.toNonEmptyList)
                PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

            patrolFieldIntersection.fold(probeShape)(probeShape + _)

        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
      }
