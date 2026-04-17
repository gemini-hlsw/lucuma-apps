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

  def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (Igrins2SvcFov, scienceArea.svcFieldOfView(posAngle, offset)),
      (Igrins2ScienceSlit, scienceArea.scienceSlitFOV(posAngle, offset))
    )

  override protected def candidatesAreaCss: Css = Flamingos2CandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.Igrins2LongSlit().withPWFS1
      case GuideProbe.PWFS2 => AgsParams.Igrins2LongSlit().withPWFS2
      case _                => AgsParams.Igrins2LongSlit()

  def igrins2Geometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ) = instrumentGeometry(referenceCoordinates,
                         blindOffset,
                         scienceOffsets,
                         fallbackPosAngle,
                         conf,
                         trackType,
                         gs,
                         candidatesVisibilityCss
  )
