// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos
import lucuma.core.geom.gmos.oiwfs
import lucuma.core.geom.syntax.shapeexpression.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

/**
 * GMOS geometry for visualization
 */
object GmosGeometry extends PwfsGeometry:

  // Shape to display for a specific mode
  def shapesForMode(
    posAngle:      Angle,
    offset:        Offset,
    configuration: Option[BasicConfiguration],
    port:          PortDisposition
  ): SortedMap[Css, ShapeExpression] =
    val base =
      configuration
        .map(conf => SortedMap((GmosPatrolField, patrolField(posAngle, offset, conf, port))))
        .getOrElse(SortedMap.empty[Css, ShapeExpression])
    configuration match {
      case Some(BasicConfiguration.GmosNorthLongSlit(fpu = fpu)) =>
        base +
          (GmosFpu -> gmos.scienceArea.longSlitMode.shapeAt(posAngle, offset, fpu.asLeft))
      case Some(BasicConfiguration.GmosSouthLongSlit(fpu = fpu)) =>
        base +
          (GmosFpu -> gmos.scienceArea.longSlitMode.shapeAt(posAngle, offset, fpu.asRight))
      case Some(BasicConfiguration.GmosNorthImaging(_)) |
          Some(BasicConfiguration.GmosSouthImaging(_)) =>
        base
          + ((GmosCcdVisible |+| GmosScienceCcd) -> gmos.scienceArea.imaging ⟲ posAngle)
      case _                                                     =>
        SortedMap.empty
    }

  // Shape for the patrol field at a single position and mode
  private def patrolField(
    posAngle:      Angle,
    offset:        Offset,
    configuration: BasicConfiguration,
    port:          PortDisposition
  ): ShapeExpression =
    configuration match {
      case BasicConfiguration.GmosNorthLongSlit(fpu = fpu) =>
        oiwfs.patrolField.longSlitMode.patrolFieldAt(posAngle, offset, fpu.asLeft, port)
      case BasicConfiguration.GmosSouthLongSlit(fpu = fpu) =>
        oiwfs.patrolField.longSlitMode.patrolFieldAt(posAngle, offset, fpu.asRight, port)
      case BasicConfiguration.GmosNorthImaging(_)          =>
        oiwfs.patrolField.imagingMode.patrolFieldAt(posAngle, offset, port)
      case BasicConfiguration.GmosSouthImaging(_)          =>
        oiwfs.patrolField.imagingMode.patrolFieldAt(posAngle, offset, port)
      case _                                               =>
        ShapeExpression.Empty
    }

  private def oiwfsCandidatesArea(posAngle: Angle, extraCss: Css) =
    SortedMap(
      (GmosCandidatesArea |+| extraCss, gmos.candidatesArea.candidatesAreaAt(posAngle, Offset.Zero))
    )

  // Shape to display always
  def commonShapes(
    posAngle:  Angle,
    extraCss:  Css,
    conf:      Option[BasicConfiguration],
    trackType: Option[TrackType]
  ): SortedMap[Css, ShapeExpression] =
    conf
      .map: c =>
        c.guideProbe(trackType) match
          case GuideProbe.GmosOIWFS                =>
            oiwfsCandidatesArea(posAngle, extraCss)
          case GuideProbe.PWFS2 | GuideProbe.PWFS1 =>
            pwfsCandidatesArea(GmosCandidatesArea, posAngle, extraCss)
          case _                                   =>
            SortedMap.empty[Css, ShapeExpression]
      .getOrElse(oiwfsCandidatesArea(posAngle, extraCss))

  def probeShapes(
    posAngle:        Angle,
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[BasicConfiguration],
    trackType:       Option[TrackType],
    port:            PortDisposition
  ): SortedMap[Css, ShapeExpression] =
    mode
      .flatMap: c =>
        (c, c.guideProbe(trackType)) match
          case (_, p @ (GuideProbe.PWFS1 | GuideProbe.PWFS2))                          =>
            pwfsProbeShapes(p, guideStarOffset, offsetPos).some
          case (BasicConfiguration.GmosNorthLongSlit(fpu = fpu), GuideProbe.GmosOIWFS) =>
            SortedMap(
              (GmosProbeArm,
               gmos.probeArm.longSlit
                 .shapeAt(posAngle, guideStarOffset, offsetPos, fpu.asLeft, port)
              )
            ).some
          case (BasicConfiguration.GmosSouthLongSlit(fpu = fpu), GuideProbe.GmosOIWFS) =>
            SortedMap(
              (GmosProbeArm,
               gmos.probeArm.longSlit
                 .shapeAt(posAngle, guideStarOffset, offsetPos, fpu.asRight, port)
              )
            ).some
          case (BasicConfiguration.GmosSouthImaging(_) | BasicConfiguration.GmosNorthImaging(_),
                GuideProbe.GmosOIWFS
              ) =>
            SortedMap(
              (GmosProbeArm,
               gmos.probeArm.imaging
                 .shapeAt(posAngle, guideStarOffset, offsetPos, port)
              )
            ).some
          case _                                                                       =>
            none
      .getOrElse(SortedMap.empty[Css, ShapeExpression])

  // Full geometry for GMOS
  def gmosGeometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    acquisitionOffsets:      Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    port:                    PortDisposition,
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    gs.map(_.posAngle)
      .orElse(fallbackPosAngle)
      .map { posAngle =>

        // Shapes at base position
        val baseShapes: SortedMap[Css, ShapeExpression] =
          shapesForMode(posAngle, Offset.Zero, conf, port) ++
            commonShapes(posAngle, candidatesVisibilityCss, conf, trackType)

        // Don't show the probe if there is no usable GS
        val probe = gs
          .map: gs =>
            val gsOffset   =
              referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset
            val probeShape =
              probeShapes(posAngle, gsOffset, Offset.Zero, conf, trackType, port)

            val positions = Ags.generatePositions(
              referenceCoordinates.some,
              blindOffset,
              NonEmptyList.one(posAngle),
              acquisitionOffsets.map(AcquisitionOffsets.apply),
              scienceOffsets.map(ScienceOffsets.apply)
            )

            val patrolFieldIntersection =
              conf.flatMap: c =>
                val agsParams =
                  (c, c.guideProbe(trackType)) match
                    case (BasicConfiguration.GmosSouthLongSlit(fpu = fpu), GuideProbe.PWFS1) =>
                      AgsParams.GmosLongSlit(fpu.asRight, port).withPWFS1.some
                    case (BasicConfiguration.GmosNorthLongSlit(fpu = fpu), GuideProbe.PWFS1) =>
                      AgsParams.GmosLongSlit(fpu.asLeft, port).withPWFS1.some
                    case (BasicConfiguration.GmosSouthLongSlit(fpu = fpu), GuideProbe.PWFS2) =>
                      AgsParams.GmosLongSlit(fpu.asRight, port).withPWFS2.some
                    case (BasicConfiguration.GmosNorthLongSlit(fpu = fpu), GuideProbe.PWFS2) =>
                      AgsParams.GmosLongSlit(fpu.asLeft, port).withPWFS2.some
                    case (BasicConfiguration.GmosSouthLongSlit(fpu = fpu), _)                =>
                      AgsParams.GmosLongSlit(fpu.asRight, port).some
                    case (BasicConfiguration.GmosNorthLongSlit(fpu = fpu), _)                =>
                      AgsParams.GmosLongSlit(fpu.asLeft, port).some
                    case (BasicConfiguration.GmosNorthImaging(_) |
                          BasicConfiguration.GmosSouthImaging(_),
                          GuideProbe.PWFS1
                        ) =>
                      AgsParams.GmosImaging(port).withPWFS1.some
                    case (BasicConfiguration.GmosNorthImaging(_) |
                          BasicConfiguration.GmosSouthImaging(_),
                          GuideProbe.PWFS2
                        ) =>
                      AgsParams.GmosImaging(port).withPWFS2.some
                    case (BasicConfiguration.GmosNorthImaging(_) |
                          BasicConfiguration.GmosSouthImaging(_),
                          GuideProbe.GmosOIWFS
                        ) =>
                      AgsParams.GmosImaging(port).some
                    case _                                                                   =>
                      none
                agsParams.map: agsParams =>
                  val calcs = agsParams.posCalculations(positions.value.toNonEmptyList)
                  PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

            patrolFieldIntersection.fold(probeShape)(probeShape + _)
        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
      }
