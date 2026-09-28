// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gnirs.scienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

case class GnirsGeometry(
  fpu:    GnirsFpuSlit,
  camera: GnirsCamera,
  prism:  GnirsPrism,
  altair: Option[AltairMode]
) extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (GnirsScienceSlit, scienceArea.longSlitShapeAt(posAngle, offset, fpu, camera, prism))
    )

  override protected def candidatesAreaCss: Css = GnirsCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    AgsParams.GnirsLongSlit(fpu, camera, prism).guidedBy(guideProbe.some, altair)

case class GnirsImagingGeometry(
  camera: GnirsCamera,
  filter: GnirsFilter,
  altair: Option[AltairMode]
) extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (GnirsScienceSlit, scienceArea.imagingShapeAt(posAngle, offset, camera, filter))
    )

  override protected def candidatesAreaCss: Css = GnirsCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    AgsParams.GnirsImaging(camera, filter).guidedBy(guideProbe.some, altair)

case class GnirsIfuGeometry(ifu: GnirsFpuIfu, altair: Option[AltairMode]) extends PwfsGeometry:

  override def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (GnirsScienceSlit, scienceArea.ifuShapeAt(posAngle, offset, ifu))
    )

  override protected def candidatesAreaCss: Css = GnirsCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    AgsParams.GnirsIfu(ifu).guidedBy(guideProbe.some, altair)

object GnirsGeometry:

  def gnirsGeometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    guideProbe:              Option[GuideProbe],
    altair:                  Option[AltairMode],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    conf
      .collect:
        // The long slit, imaging and IFU all have a science-area/AGS geometry.
        case BasicConfiguration.GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Slit(slit),
                                                  camera = camera,
                                                  prism = prism
            ) =>
          GnirsGeometry(slit, camera, prism, altair)
        case BasicConfiguration.GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Ifu(ifu)) =>
          GnirsIfuGeometry(ifu, altair)
        case BasicConfiguration.GnirsImaging(filters = filters, camera = camera)        =>
          GnirsImagingGeometry(camera, AgsParams.GnirsImaging.representativeFilter(filters), altair)
      .flatMap:
        _.instrumentGeometry(
          referenceCoordinates,
          blindOffset,
          scienceOffsets,
          fallbackPosAngle,
          guideProbe,
          gs,
          candidatesVisibilityCss
        )
