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
import lucuma.ags.GuidedOffset
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2
import lucuma.core.geom.flamingos2.scienceArea
import lucuma.core.geom.pwfs
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

/**
 * Object to produce flamingos2 geometry for visualization
 */
object Flamingos2Geometry extends WithPwfsGeometry:

  // Shape to display for a specific mode
  def shapesForMode(
    posAngle:      Angle,
    offset:        Offset,
    configuration: Option[BasicConfiguration]
  ): SortedMap[Css, ShapeExpression] =
    configuration match
      case Some(m: BasicConfiguration.Flamingos2LongSlit) =>
        SortedMap(
          (Flamingos2ScienceArea,
           scienceArea.shapeAt(posAngle,
                               offset,
                               Flamingos2LyotWheel.F16,
                               Flamingos2FpuMask.Builtin(m.fpu)
           )
          )
        )
      case Some(_: BasicConfiguration.Flamingos2Imaging)  =>
        SortedMap(
          (Flamingos2ScienceArea,
           scienceArea.shapeAt(posAngle, offset, Flamingos2LyotWheel.F16, Flamingos2FpuMask.Imaging)
          )
        )
      case _                                              =>
        SortedMap.empty

  def oiwfsCandidatesArea(lw: Flamingos2LyotWheel, posAngle: Angle, extraCss: Css) =
    SortedMap(
      (Flamingos2CandidatesArea |+| extraCss,
       flamingos2.candidatesArea.candidatesAreaAt(lw, posAngle, Offset.Zero)
      )
    )

  // Shape to display always
  def commonShapes(
    lw:            Flamingos2LyotWheel,
    posAngle:      Angle,
    extraCss:      Css,
    configuration: Option[BasicConfiguration],
    trackType:     Option[TrackType]
  ): SortedMap[Css, ShapeExpression] =
    configuration
      .map: c =>
        c.guideProbe(trackType) match
          case Some(GuideProbe.Flamingos2OIWFS)                =>
            oiwfsCandidatesArea(lw, posAngle, extraCss)
          case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
            pwfsCandidatesArea(Flamingos2CandidatesArea, posAngle, extraCss)
          case _                                               =>
            SortedMap.empty[Css, ShapeExpression]
      .getOrElse(oiwfsCandidatesArea(lw, posAngle, extraCss))

  // Shape for the patrol field at a single position
  def patrolField(
    posAngle:      Angle,
    offset:        Offset,
    configuration: BasicConfiguration,
    lyotWheel:     Flamingos2LyotWheel,
    port:          PortDisposition,
    trackType:     Option[TrackType]
  ): ShapeExpression =
    configuration match
      case _: BasicConfiguration.Flamingos2LongSlit | _: BasicConfiguration.Flamingos2Imaging =>
        configuration.guideProbe(trackType) match
          case Some(GuideProbe.Flamingos2OIWFS)                =>
            flamingos2.patrolField.patrolFieldAt(posAngle, offset, lyotWheel, port)
          case Some(GuideProbe.PWFS1) | Some(GuideProbe.PWFS2) =>
            pwfs.patrolField.patrolFieldAt(posAngle, offset)
          case _                                               =>
            ShapeExpression.Empty
      case _                                                                                  =>
        ShapeExpression.Empty

  // Shape to display always
  def probeShapes(
    posAngle:        Angle,
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[BasicConfiguration],
    trackType:       Option[TrackType],
    port:            PortDisposition,
    lyotWheel:       Flamingos2LyotWheel // in practice this is always F16
  ): SortedMap[Css, ShapeExpression] =
    mode match
      case Some(
            m @ (_: BasicConfiguration.Flamingos2LongSlit | _: BasicConfiguration.Flamingos2Imaging)
          ) =>
        m.guideProbe(trackType).fold(SortedMap.empty[Css, ShapeExpression]) { p =>
          p match
            case GuideProbe.Flamingos2OIWFS          =>
              SortedMap(
                (Flamingos2ProbeArm,
                 flamingos2.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, lyotWheel, port)
                )
              )
            case GuideProbe.PWFS1 | GuideProbe.PWFS2 =>
              pwfsProbeShapes(p, guideStarOffset, offsetPos)
            case _                                   =>
              SortedMap.empty
        }
      case _ =>
        SortedMap.empty

  // Full geometry for flamingos2
  def f2Geometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    acquisitionOffsets:      Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    port:                    PortDisposition,
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css,
    lyotWheel:               Flamingos2LyotWheel = Flamingos2LyotWheel.F16 // in practice this is always F16
  ): Option[SortedMap[Css, ShapeExpression]] =
    gs.map(_.posAngle)
      .orElse(fallbackPosAngle)
      .map: posAngle =>
        // Shapes at base position
        val baseShapes: SortedMap[Css, ShapeExpression] =
          shapesForMode(posAngle, Offset.Zero, conf) ++
            commonShapes(lyotWheel, posAngle, candidatesVisibilityCss, conf, trackType)

        // Don't show the probe if there is no usable GS
        val probe = gs
          .map: gs =>
            val gsOffset =
              referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset

            val probeShape =
              probeShapes(posAngle, gsOffset, Offset.Zero, conf, trackType, port, lyotWheel)

            val positions = Ags.generatePositions(
              referenceCoordinates.some,
              blindOffset,
              NonEmptyList.one(posAngle),
              acquisitionOffsets.map(AcquisitionOffsets.apply),
              scienceOffsets.map(ScienceOffsets.apply)
            )

            val patrolFieldIntersection =
              conf
                .flatMap: c =>
                  c.agsParams(port, trackType)
                .map: params =>
                  val calcs = params.posCalculations(positions.value.toNonEmptyList)
                  PatrolFieldIntersection -> calcs.head._2.intersectionPatrolField

            patrolFieldIntersection.fold(probeShape)(probeShape + _)

        baseShapes ++ probe.getOrElse(SortedMap.empty[Css, ShapeExpression])
