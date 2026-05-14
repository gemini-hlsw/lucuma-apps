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
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.visitors.maroonXScienceArea
import lucuma.core.geom.visitors.visitorScienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

object VisitorGeometry extends WithPwfsGeometry:

  private def fovShape(
    mode:       VisitorObservingModeType,
    posAngle:   Angle,
    offset:     Offset,
    scienceFov: Angle
  ): Option[ShapeExpression] =
    mode match
      case VisitorObservingModeType.MaroonX =>
        maroonXScienceArea.shapeAt(posAngle, offset).some
      case _                                =>
        visitorScienceArea.shapeAt(posAngle, offset, scienceFov).some

  def shapesForMode(
    posAngle:      Angle,
    offset:        Offset,
    configuration: Option[BasicConfiguration]
  ): SortedMap[Css, ShapeExpression] =
    configuration match
      case Some(v: BasicConfiguration.Visitor) =>
        fovShape(v.mode, posAngle, offset, v.scienceFov)
          .fold(SortedMap.empty[Css, ShapeExpression]): shape =>
            SortedMap((VisitorScienceFov, shape))
      case _                                   =>
        SortedMap.empty

  private def agsParamsFor(guideProbe: GuideProbe, scienceFov: Angle): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.Visitor(scienceFov).withPWFS1
      case GuideProbe.PWFS2 => AgsParams.Visitor(scienceFov).withPWFS2
      case _                => AgsParams.Visitor(scienceFov)

  def visitorGeometry(
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
      .map: posAngle =>
        val candidatesArea: SortedMap[Css, ShapeExpression] =
          conf.map(_.guideProbe(trackType)) match
            case Some(GuideProbe.PWFS1 | GuideProbe.PWFS2) =>
              pwfsCandidatesArea(VisitorCandidatesArea, posAngle, candidatesVisibilityCss)
            case _                                         =>
              SortedMap.empty

        val baseShapes = shapesForMode(posAngle, Offset.Zero, conf) ++ candidatesArea

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
              .collect:
                case v: BasicConfiguration.Visitor =>
                  agsParamsFor(v.guideProbe(trackType), v.scienceFov)
              .map: params =>
                val calcs = params.posCalculations(positions.value.toNonEmptyList)
                PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

          patrolFieldIntersection.fold(probeShape)(probeShape + _)

        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
