// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuidedOffset
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ghost
import lucuma.core.geom.syntax.shapeexpression.*
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

  def ifu2PatrolFieldShape(posAngle: Angle): ShapeExpression =
    ghost.GhostIfuPatrolField.ifu2PatrolFieldAt(posAngle, Offset.Zero)

  // One 102" minimum-separation keep-out disk per science-target offset.
  def skyExclusionShapes(scienceOffsets: List[Offset]): List[ShapeExpression] =
    val r    = ghost.MinimumIfuArmSeparation
    val disk = ShapeExpression.centeredEllipse(r + r, r + r)
    scienceOffsets.map(disk.translate)

  /** The 102" minimum-separation keep-out disk, unioned across the science-target offsets. */
  def skyExclusionShape(scienceOffsets: List[Offset]): Option[ShapeExpression] =
    skyExclusionShapes(scienceOffsets).reduceOption(_ ∪ _)

  /** The clickable IFU2 sky region: the patrol field with the keep-out disk(s) removed. */
  def ifu2SkySlotShape(posAngle: Angle, scienceOffsets: List[Offset]): ShapeExpression =
    val full = ifu2PatrolFieldShape(posAngle)
    skyExclusionShape(scienceOffsets).fold(full)(full - _)

  private def isTooClose(distance: Angle): Boolean =
    distance.toMicroarcseconds < ghost.MinimumIfuArmSeparation.toMicroarcseconds

  /** Whether two coordinates are closer than the minimum IFU arm separation. */
  def tooClose(a: Coordinates, b: Coordinates): Boolean =
    isTooClose(a.angularDistance(b))

  /** Whether any two of the given offsets are closer than the minimum IFU arm separation. */
  def anyTooClose(offsets: List[Offset]): Boolean =
    offsets
      .combinations(2)
      .exists:
        case List(a, b) => isTooClose(a.distance(b))
        case _          => false

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
    ifu2Coords:              Option[Coordinates],
    ifu1Selected:            Boolean,
    ifu2Selected:            Boolean,
    showIfu2Area:            Boolean
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
        // ifu patrol fields are anchored at the base, i.e. offset zero
        val ifus = List(
          ifu1Coords.map: _ =>
            (GhostIfu1PatrolField |+| GhostIfuPatrolFieldSelected.when_(ifu1Selected),
             ghost.GhostIfuPatrolField.ifu1PatrolFieldAt(posAngle, Offset.Zero)
            ),
          Option.when(ifu2Coords.isDefined || showIfu2Area):
            (GhostIfu2PatrolField |+| GhostIfuPatrolFieldSelected.when_(ifu2Selected),
             ifu2PatrolFieldShape(posAngle)
            )
        ).collect:
          case Some((c, s)) => (c, s)

        baseGeometries ++ SortedMap.from(ifus)
