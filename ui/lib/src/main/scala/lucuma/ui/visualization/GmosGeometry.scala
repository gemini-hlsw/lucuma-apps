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
        .map(conf =>
          SortedMap((GmosScienceCcd, gmos.scienceArea.imaging ⟲ posAngle),
                    (GmosPatrolField, patrolField(posAngle, offset, conf, port))
          )
        )
        .getOrElse(SortedMap.empty[Css, ShapeExpression])
    configuration match {
      case Some(BasicConfiguration.GmosNorthLongSlit(fpu = fpu)) =>
        base +
          (GmosFpu -> gmos.scienceArea.shapeAt(posAngle, offset, fpu.asLeft.some))
      case Some(BasicConfiguration.GmosSouthLongSlit(fpu = fpu)) =>
        base +
          (GmosFpu -> gmos.scienceArea.shapeAt(posAngle, offset, fpu.asRight.some))
      case Some(BasicConfiguration.GmosNorthImaging(_)) |
          Some(BasicConfiguration.GmosSouthImaging(_)) =>
        base
      case _                                                     =>
        SortedMap.empty
    }

  // Shape for the patrol field at a single position
  def patrolField(
    posAngle:      Angle,
    offset:        Offset,
    configuration: BasicConfiguration,
    port:          PortDisposition
  ): ShapeExpression =
    configuration match {
      case BasicConfiguration.GmosNorthLongSlit(fpu = fpu) =>
        oiwfs.patrolField.patrolFieldAt(posAngle, offset, fpu.asLeft.some, port)
      case BasicConfiguration.GmosSouthLongSlit(fpu = fpu) =>
        oiwfs.patrolField.patrolFieldAt(posAngle, offset, fpu.asRight.some, port)
      case BasicConfiguration.GmosNorthImaging(_)          =>
        oiwfs.patrolField.patrolFieldAt(posAngle, offset, none, port)
      case BasicConfiguration.GmosSouthImaging(_)          =>
        oiwfs.patrolField.patrolFieldAt(posAngle, offset, none, port)
      case _                                               =>
        ShapeExpression.Empty
    }

  def oiwfsCandidatesArea(posAngle: Angle, extraCss: Css) =
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

  // Shape to display always
  def probeShapes(
    posAngle:        Angle,
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[BasicConfiguration],
    trackType:       Option[TrackType],
    port:            PortDisposition
  ): SortedMap[Css, ShapeExpression] =
    mode
      .map:
        case m @ BasicConfiguration.GmosNorthLongSlit(fpu = fpu) =>
          (fpu.asLeft.some, m.guideProbe(trackType))
        case m @ BasicConfiguration.GmosSouthLongSlit(fpu = fpu) =>
          (fpu.asRight.some, m.guideProbe(trackType))
        case m @ BasicConfiguration.GmosNorthImaging(_)          =>
          (none, m.guideProbe(trackType))
        case m @ BasicConfiguration.GmosSouthImaging(_)          =>
          (none, m.guideProbe(trackType))
        case _                                                   =>
          (none, GuideProbe.GmosOIWFS)
      .map:
        case (fpu @ Some(_), GuideProbe.GmosOIWFS)          =>
          SortedMap(
            (GmosProbeArm, gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, fpu, port))
          )
        case (_, p @ (GuideProbe.PWFS1 | GuideProbe.PWFS2)) =>
          pwfsProbeShapes(p, guideStarOffset, offsetPos)
        case _                                              =>
          SortedMap.empty[Css, ShapeExpression]
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

            val fpu =
              conf match
                case Some(BasicConfiguration.GmosNorthLongSlit(fpu = fpu)) => fpu.asLeft.some
                case Some(BasicConfiguration.GmosSouthLongSlit(fpu = fpu)) => fpu.asRight.some
                case _                                                     => none

            val patrolFieldIntersection =
              conf.map: c =>
                val agsParams =
                  c.guideProbe(trackType) match
                    case GuideProbe.PWFS1 =>
                      AgsParams.GmosAgsParams(fpu, port).withPWFS1
                    case GuideProbe.PWFS2 =>
                      AgsParams.GmosAgsParams(fpu, port).withPWFS2
                    case _                =>
                      AgsParams.GmosAgsParams(fpu, port)
                val calcs     = agsParams.posCalculations(positions.value.toNonEmptyList)
                PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

            patrolFieldIntersection.fold(probeShape)(probeShape + _)
        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
      }
