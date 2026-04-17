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
import lucuma.core.geom.ghost
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.visualization.VisualizationStyles.*

import scala.collection.immutable.SortedMap

object GhostGeometry extends PwfsGeometry:

  def shapesForMode(posAngle: Angle, offset: Offset): SortedMap[Css, ShapeExpression] =
    SortedMap((GhostScienceArea, ghost.scienceArea.fovAt(posAngle, offset)))

  override protected def candidatesAreaCss: Css = GhostCandidatesArea

  override protected def agsParamsFor(guideProbe: GuideProbe): SingleProbeAgsParams =
    guideProbe match
      case GuideProbe.PWFS1 => AgsParams.GhostIfu().withPWFS1
      case GuideProbe.PWFS2 => AgsParams.GhostIfu().withPWFS2
      case _                => AgsParams.GhostIfu()

  // GHOST-specific: draws IFU1 / IFU2 patrol fields at each target's offset rather than the base
  def ghostGeometry(
    referenceCoordinates:    Coordinates,
    blindOffset:             Option[Coordinates],
    scienceOffsets:          Option[NonEmptySet[GuidedOffset]],
    fallbackPosAngle:        Option[Angle],
    conf:                    Option[BasicConfiguration],
    trackType:               Option[TrackType],
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css,
    ifu1Coords:              Option[Coordinates],
    ifu2Coords:              Option[Coordinates]
  ): Option[SortedMap[Css, ShapeExpression]] =
    instrumentGeometry(
      referenceCoordinates,
      blindOffset,
      scienceOffsets,
      fallbackPosAngle,
      conf,
      trackType,
      gs,
      candidatesVisibilityCss
    ).flatMap: baseGeometries =>
      posAngle(gs, fallbackPosAngle).map: posAngle =>
        def ifuOffset(c: Option[Coordinates]): Option[Offset] =
          c.map(c => referenceCoordinates.diff(c).offset)

        val ifus = List(
          ifuOffset(ifu1Coords).map: o =>
            (GhostIfu1PatrolField, ghost.GhostIfuPatrolField.ifu1PatrolFieldAt(posAngle, o)),
          ifuOffset(ifu2Coords).map: o =>
            (GhostIfu2PatrolField, ghost.GhostIfuPatrolField.ifu2PatrolFieldAt(posAngle, o))
        ).collect:
          case Some((c, s)) => (c, s)

        baseGeometries ++ SortedMap.from(ifus)
