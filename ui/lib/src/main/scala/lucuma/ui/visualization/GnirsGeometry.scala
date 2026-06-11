// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gnirs.scienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

case class GnirsGeometry(fpu: GnirsFpuSlit, camera: GnirsCamera, prism: GnirsPrism)
    extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (GnirsScienceSlit, scienceArea.longSlitShapeAt(posAngle, offset, fpu, camera, prism))
    )

  override protected def candidatesAreaCss: Css = GnirsCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.GnirsLongSlit(fpu, camera, prism).withPWFS1
      case GuideProbe.PWFS2 => AgsParams.GnirsLongSlit(fpu, camera, prism).withPWFS2
      case _                => AgsParams.GnirsLongSlit(fpu, camera, prism)

object GnirsGeometry:

  def gnirsGeometry(
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
        case BasicConfiguration.GnirsLongSlit(fpu = fpu, camera = camera, prism = prism) =>
          GnirsGeometry(fpu, camera, prism)
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
