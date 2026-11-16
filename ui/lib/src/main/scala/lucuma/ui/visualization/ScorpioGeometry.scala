// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.implicits.catsKernelOrderingForOrder
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ScorpioFpu
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.scorpio.scienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

case class ScorpioGeometry(fpu: ScorpioFpu) extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (ScorpioScienceSlit, scienceArea.longSlitMode.shapeAt(posAngle, offset, fpu))
    )

  override protected def candidatesAreaCss: Css = ScorpioCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.ScorpioLongSlit(fpu).withPWFS1
      case GuideProbe.PWFS2 => AgsParams.ScorpioLongSlit(fpu).withPWFS2
      case _                => AgsParams.ScorpioLongSlit(fpu)

object ScorpioGeometry:

  def scorpioGeometry(
    referenceCoordinates:    Coordinates,
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    guideProbe:              Option[GuideProbe],
    gs:                      Option[AgsAnalysis.Usable],
    patrolFieldIntersection: Option[ShapeExpression],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    conf
      .collect:
        case BasicConfiguration.ScorpioLongSlit(fpu) => ScorpioGeometry(fpu)
      .flatMap:
        _.instrumentGeometry(
          referenceCoordinates,
          fallbackPosAngle,
          guideProbe,
          gs,
          patrolFieldIntersection,
          candidatesVisibilityCss
        )
