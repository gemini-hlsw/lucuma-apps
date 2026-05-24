// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
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

case class VisitorGeometry(mode: VisitorObservingModeType, scienceFov: Angle) extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset) =
    mode match
      case VisitorObservingModeType.MaroonX =>
        SortedMap(
          (VisitorScienceFov, maroonXScienceArea.shapeAt(posAngle, offset)),
          (ExtendedVignettingArea, maroonXScienceArea.extendedVignettingAreaAt(posAngle, offset))
        )
      case _                                =>
        SortedMap((VisitorScienceFov, visitorScienceArea.shapeAt(posAngle, offset, scienceFov)))

  override protected def candidatesAreaCss: Css = VisitorCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    mode match
      case VisitorObservingModeType.MaroonX =>
        guideProbe match
          case GuideProbe.PWFS1 => AgsParams.MaroonX().withPWFS1
          case GuideProbe.PWFS2 => AgsParams.MaroonX().withPWFS2
          case _                => AgsParams.MaroonX()
      case _                                =>
        guideProbe match
          case GuideProbe.PWFS1 => AgsParams.Visitor(scienceFov).withPWFS1
          case GuideProbe.PWFS2 => AgsParams.Visitor(scienceFov).withPWFS2
          case _                => AgsParams.Visitor(scienceFov)

object VisitorGeometry:

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
    conf
      .collect:
        case BasicConfiguration.Visitor(mode = mode, scienceFov = fov) =>
          new VisitorGeometry(mode, fov)
      .flatMap:
        _.instrumentGeometry(
          referenceCoordinates,
          blindOffset,
          scienceOffsets,
          fallbackPosAngle,
          conf,
          trackType,
          gs,
          candidatesVisibilityCss
        )
