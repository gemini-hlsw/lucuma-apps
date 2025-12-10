// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.AGSDebugVisibility
import explore.model.ConfigurationForVisualization
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsVisualization
import lucuma.ags.GeometryType
import lucuma.ags.SingleProbeAgsParams
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2
import lucuma.core.geom.gmos
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.react.common.Css
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.reusability.given
import lucuma.ui.visualization.*

import scala.collection.immutable.SortedMap

// Hooks for shapes
def usePatrolFieldShapes(
  vizConf:         Option[ConfigurationForVisualization],
  selectedGS:      Option[AgsAnalysis.Usable],
  baseCoordinates: Option[Coordinates],
  blindOffset:     Option[Coordinates],
  pfVisibility:    AGSDebugVisibility,
  anglesToTest:    Option[NonEmptyList[Angle]]
): HookResult[Option[SortedMap[Css, ShapeExpression]]] =

  def createAgsParams(
    conf: BasicConfiguration,
    port: PortDisposition
  ): Option[SingleProbeAgsParams] =
    conf match
      case m: BasicConfiguration.GmosNorthLongSlit  =>
        AgsParams.GmosAgsParams(m.fpu.asLeft.some, port).some
      case m: BasicConfiguration.GmosSouthLongSlit  =>
        AgsParams.GmosAgsParams(m.fpu.asRight.some, port).some
      case _: BasicConfiguration.GmosNorthImaging   =>
        AgsParams.GmosAgsParams(none, port).some
      case _: BasicConfiguration.GmosSouthImaging   =>
        AgsParams.GmosAgsParams(none, port).some
      case m: BasicConfiguration.Flamingos2LongSlit =>
        AgsParams
          .Flamingos2AgsParams(
            Flamingos2LyotWheel.F16,
            Flamingos2FpuMask.Builtin(m.fpu),
            port
          )
          .some

  extension (geometryType: GeometryType)
    def css: Css = geometryType match
      case GeometryType.Base         => VisualizationStyles.PatrolFieldBase
      case GeometryType.BlindOffset  => VisualizationStyles.PatrolFieldBlindOffset
      case GeometryType.AcqOffset    => VisualizationStyles.PatrolFieldAcquisitionOffset
      case GeometryType.SciOffset    => VisualizationStyles.PatrolFieldScienceOffset
      case GeometryType.Intersection => VisualizationStyles.PatrolFieldIntersectionDebug
      case GeometryType.Vignetting   => VisualizationStyles.DebugScienceVignetting

  useMemo(
    (vizConf, selectedGS, baseCoordinates, blindOffset, pfVisibility, anglesToTest)
  ) { (vizConf, selectedGS, baseCoordinates, blindOffset, _, _) =>
    val fallbackPA = vizConf.map(_.posAngle).map(NonEmptyList.one)

    val allAngles =
      if (pfVisibility.showAllAngles.value)
        anglesToTest.orElse(fallbackPA)
      else
        selectedGS
          .flatMap(_.posAngle.some)
          .map(NonEmptyList.one)
          .orElse(fallbackPA)

    for
      conf       <- vizConf.map(_.configuration)
      agsParams  <- createAgsParams(conf, PortDisposition.Side)
      baseCoords <- baseCoordinates
      paAngles   <- allAngles
    yield
      val positions = Ags.generatePositions(
        baseCoords,
        blindOffset,
        paAngles,
        vizConf.flatMap(_.asAcqOffsets),
        vizConf.flatMap(_.asSciOffsets)
      )

      val visualizations =
        AgsVisualization.patrolFieldGeometries(agsParams, positions)

      val individualFields = visualizations.toList
        .filter: pfv =>
          pfv.position.geometryType match
            case GeometryType.Base         => pfVisibility.showBase.value
            case GeometryType.BlindOffset  => pfVisibility.showBlindOffset.value
            case GeometryType.AcqOffset    => pfVisibility.showAcquisitionOffset.value
            case GeometryType.SciOffset    => pfVisibility.showScienceOffset.value
            case GeometryType.Intersection => pfVisibility.showIntersection.value
            case GeometryType.Vignetting   => false
        .zipWithIndex
        .map: (pfv, idx) =>
          val baseCss = pfv.position.geometryType.css
          val idxCss  = Css(s"pf-idx-$idx")
          (baseCss |+| idxCss, pfv.posPatrolField)

      val intersections =
        if (pfVisibility.showIntersection.value)
          visualizations.toList
            .groupBy(_.position.posAngle)
            .values
            .zipWithIndex
            .map: (pfvs, idx) =>
              val idxCss = Css(s"pf-intersection-$idx")
              (VisualizationStyles.PatrolFieldIntersectionDebug |+| idxCss,
               pfvs.head.paIntersection
              )
            .toList
        else
          List.empty

      // We need a hidden achor centered at 0, 0
      val anchor = conf.obsModeType match
        case ObservingModeType.Flamingos2LongSlit                                      =>
          (VisualizationStyles.Anchor,
           flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16)
          )
        case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
          (VisualizationStyles.Anchor, gmos.candidatesArea.candidatesArea)
        case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging   =>
          (VisualizationStyles.Anchor, gmos.candidatesArea.candidatesArea)

      SortedMap.from(anchor :: (individualFields ++ intersections))
  }.map(_.value)

def useVisualizationShapes(
  vizConf:         Option[ConfigurationForVisualization],
  baseCoordinates: Option[Coordinates],
  blindOffset:     Option[Coordinates],
  agsOverlay:      Boolean,
  selectedGS:      Option[AgsAnalysis.Usable]
): HookResult[Option[(Css, Option[SortedMap[Css, ShapeExpression]])]] =
  useMemo(
    (vizConf, baseCoordinates, blindOffset, agsOverlay, selectedGS)
  ) { (vizConf, baseCoordinates, blindOffset, agsOverlay, selectedGS) =>
    val candidatesVisibilityCss =
      ExploreStyles.GuideStarCandidateVisible.when_(agsOverlay)

    (vizConf.map(_.configuration.obsModeType), baseCoordinates).mapN: (conf, baseCoords) =>
      conf match
        case ObservingModeType.Flamingos2LongSlit                                      =>
          (Css.Empty,
           Flamingos2Geometry.f2Geometry(
             baseCoords,
             blindOffset,
             vizConf.flatMap(_.guidedSciOffsets),
             vizConf.flatMap(_.guidedAcqOffsets),
             vizConf.map(_.posAngle),
             vizConf.map(_.configuration),
             PortDisposition.Side,
             selectedGS,
             candidatesVisibilityCss
           )
          )
        case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
          (Css.Empty,
           GmosGeometry.gmosGeometry(
             baseCoords,
             blindOffset,
             vizConf.flatMap(_.guidedSciOffsets),
             vizConf.flatMap(_.guidedAcqOffsets),
             vizConf.map(_.posAngle),
             vizConf.map(_.configuration),
             PortDisposition.Side,
             selectedGS,
             candidatesVisibilityCss
           )
          )
        case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging   =>
          (VisualizationStyles.GmosCcdVisible,
           GmosGeometry.gmosGeometry(
             baseCoords,
             blindOffset,
             vizConf.flatMap(_.guidedSciOffsets),
             vizConf.flatMap(_.guidedAcqOffsets),
             vizConf.map(_.posAngle),
             vizConf.map(_.configuration),
             PortDisposition.Side,
             selectedGS,
             candidatesVisibilityCss
           )
          )
  }.map(_.value)
