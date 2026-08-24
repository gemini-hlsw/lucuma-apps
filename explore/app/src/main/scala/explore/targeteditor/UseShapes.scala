// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.AGSVisibility
import explore.model.ConfigurationForVisualization
import explore.model.GhostSkySlot
import explore.model.MaskDesign
import explore.model.MaskDesignSlit
import explore.model.enums.AgsState
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsVisualization
import lucuma.ags.DebugShape
import lucuma.ags.PatrolFieldVisualization
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.MosSlitPriority
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2
import lucuma.core.geom.ghost
import lucuma.core.geom.gmos
import lucuma.core.geom.mos.MosMaskGeometry
import lucuma.core.geom.offsets.GeometryType
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.offsets.OffsetPositions
import lucuma.core.geom.pwfs
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.Css
import lucuma.schemas.model.SlotId
import lucuma.ui.reusability.given
import lucuma.ui.visualization.*

import scala.collection.immutable.SortedMap

private given Reusability[Map[SlotId, Coordinates]] = Reusability.map

// Hooks for shapes
def usePatrolFieldShapes(
  vizConf:                Option[ConfigurationForVisualization],
  selectedGS:             Option[AgsAnalysis.Usable],
  baseCoordinates:        Option[Coordinates],
  blindOffset:            Option[Coordinates],
  scienceOffsetsFromBase: List[Offset],
  pfVisibility:           AGSVisibility,
  anglesToTest:           Option[NonEmptyList[Angle]],
  agsState:               Option[AgsState]
): HookResult[Option[SortedMap[Css, ShapeExpression]]] =

  extension (geometryType: GeometryType)
    def css: Css = geometryType match
      case GeometryType.Base              => VisualizationStyles.PatrolFieldBase
      case GeometryType.BlindOffset       => VisualizationStyles.PatrolFieldBlindOffset
      case GeometryType.AcqGuidedOffset   => VisualizationStyles.PatrolFieldAcquisitionOffset
      case GeometryType.AcqUnguidedOffset => VisualizationStyles.PatrolFieldAcquisitionOffset
      case GeometryType.SciGuidedOffset   => VisualizationStyles.PatrolFieldScienceOffset
      case GeometryType.SciUnguidedOffset => VisualizationStyles.PatrolFieldScienceOffset
      case GeometryType.AgsIntersection   => VisualizationStyles.PatrolFieldIntersectionDebug
      case GeometryType.AgsVignetting     => VisualizationStyles.DebugScienceVignetting
      case GeometryType.NoZone            => VisualizationStyles.PatrolFieldNoZones

  val isVisible: Boolean =
    pfVisibility.showBase.value || pfVisibility.showBlindOffset.value ||
      pfVisibility.showAcquisitionOffset.value || pfVisibility.showScienceOffset.value ||
      pfVisibility.showIntersection.value || pfVisibility.showNoZones.value

  useMemo(
    (vizConf,
     selectedGS,
     baseCoordinates,
     blindOffset,
     scienceOffsetsFromBase,
     pfVisibility,
     anglesToTest,
     agsState
    )
  ) { (vizConf, selectedGS, baseCoordinates, blindOffset, scienceOffsetsFromBase, _, _, _) =>
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
      conf       <- vizConf.map(_.configuration).filter(_ => isVisible)
      agsParams  <- conf.agsParams(PortDisposition.Side, vizConf.flatMap(_.trackType))
      baseCoords <- baseCoordinates
      paAngles   <- allAngles
    yield
      val positions: OffsetPositions =
        Ags.generatePositions(
          baseCoords.some,
          blindOffset,
          paAngles,
          vizConf.flatMap(_.asAcqOffsets),
          vizConf.flatMap(_.asSciOffsets)
        )

      val noZoneOffsets: List[OffsetPosition] =
        val sciOffsets   = scienceOffsetsFromBase
          .map(OffsetPosition(GeometryType.NoZone, _, Angle.Angle0))
        val blindOffsets = blindOffset
          .map(o => baseCoords.diff(o).offset)
          .map(o => OffsetPosition(GeometryType.NoZone, o, Angle.Angle0))
          .toList
        (sciOffsets ++ blindOffsets).distinct

      val visualizations: NonEmptyList[DebugShape] =
        AgsVisualization.patrolFieldGeometries(agsParams,
                                               positions.value.toNonEmptyList
        ) ++ AgsVisualization.noZoneShapes(agsParams, noZoneOffsets)

      val individualFields: List[(Css, ShapeExpression)] =
        visualizations.toList
          .filter: pfv =>
            pfv.position.geometryType match
              case GeometryType.Base              => pfVisibility.showBase.value
              case GeometryType.BlindOffset       => pfVisibility.showBlindOffset.value
              case GeometryType.AcqGuidedOffset   => pfVisibility.showAcquisitionOffset.value
              case GeometryType.AcqUnguidedOffset => pfVisibility.showAcquisitionOffset.value
              case GeometryType.SciGuidedOffset   => pfVisibility.showScienceOffset.value
              case GeometryType.SciUnguidedOffset => pfVisibility.showScienceOffset.value
              case GeometryType.AgsIntersection   => pfVisibility.showIntersection.value
              case GeometryType.NoZone            => pfVisibility.showNoZones.value
              case GeometryType.AgsVignetting     => false
          .zipWithIndex
          .map: (pfv, idx) =>
            val baseCss = pfv.position.geometryType.css
            val idxCss  = Css(s"pf-idx-$idx")
            (baseCss |+| idxCss, pfv.shape)

      val intersections: List[(Css, ShapeExpression)] =
        if (pfVisibility.showIntersection.value)
          visualizations.toList
            .groupBy(_.position.posAngle)
            .values
            .map:
              _.collectFirst:
                case PatrolFieldVisualization(_, _, i, _) =>
                  (VisualizationStyles.PatrolFieldIntersectionDebug, i)
            .toList
            .flattenOption
        else
          List.empty

      // We need a hidden achor centered at 0, 0
      val anchor: Option[ShapeExpression] =
        conf.obsModeType match
          case ObservingModeType.Flamingos2LongSlit                                      =>
            flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16).some
          case ObservingModeType.Flamingos2Imaging                                       =>
            flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16).some
          case ObservingModeType.Flamingos2Mos                                           =>
            flamingos2.candidatesArea.candidatesArea(Flamingos2LyotWheel.F16).some
          case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
            gmos.candidatesArea.candidatesArea.some
          case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging   =>
            gmos.candidatesArea.candidatesArea.some
          case ObservingModeType.GmosNorthMos | ObservingModeType.GmosSouthMos           =>
            gmos.candidatesArea.candidatesArea.some
          case ObservingModeType.Igrins2LongSlit                                         =>
            pwfs.patrolField.patrolField.some
          case ObservingModeType.GhostIfu                                                =>
            ghost.scienceArea.fov.some
          case ObservingModeType.GnirsImaging | ObservingModeType.GnirsLongSlit |
              ObservingModeType.GnirsIfu =>
            pwfs.patrolField.patrolField.some
          case _: VisitorObservingModeType                                               =>
            pwfs.patrolField.patrolField.some
          case _: ExchangeObservingModeType                                              =>
            none // We won't get this far for exchange observations, anyway

      SortedMap.from(
        anchor.map((VisualizationStyles.Anchor, _)).toList ++ (individualFields ++ intersections)
      )
  }.map(_.value)

// On-sky shapes for a MOS mask design: the slit placement area and the apertures.
// The geometry is fitted from the slits themselves, already in its true sky
// orientation, so no PA rotation is applied.
private def mosMaskShapes(design: MaskDesign): Option[SortedMap[Css, ShapeExpression]] =
  MosMaskGeometry
    .fromSlits(
      design.instrument,
      design.dispersionDirection,
      design.pointing,
      // Every slit goes into the fit, Ignore ones included; only their shapes are dropped.
      design.slits.map: s =>
        MosMaskGeometry.Slit(
          coordinates = s.coordinates,
          x = s.x.toDouble,
          y = s.y.toDouble,
          width = s.width,
          length = s.length,
          offsetAlongSlit = s.offsetAlongSlit,
          offsetAcrossSlit = s.offsetAcrossSlit,
          tilt = s.tilt
        )
    )
    .map: geometry =>
      // The shapes are offsets from the design's pointing, drawn anchored at the base
      // coordinates: identical for a properly configured observation (target = pointing)
      // and still visible when the bound mask belongs to another field.
      val apertures: List[(MaskDesignSlit, ShapeExpression)] =
        design.slits
          .zip(geometry.slits)
          .filter(_._1.priority =!= MosSlitPriority.Ignore)

      val slits: List[(Css, ShapeExpression)] =
        apertures.map: (s, shape) =>
          val css =
            if (s.isAcquisition) ExploreStyles.MosMaskAcquisitionBox
            else ExploreStyles.MosMaskSlit
          (css |+| Css(s"mos-mask-aperture-${s.id}"), shape)

      // The apertures are cut out of the plate, as in the physical mask, so the
      // body's hatch fill does not run beneath the openings.
      val body: ShapeExpression =
        apertures.foldLeft(geometry.outline)((b, aperture) => b - aperture._2)

      SortedMap.from(
        (ExploreStyles.MosMaskOutline, body) :: slits
      )

def useVisualizationShapes(
  vizConf:         Option[ConfigurationForVisualization],
  baseCoordinates: Option[Coordinates],
  blindOffset:     Option[Coordinates],
  slotCoords:      Map[SlotId, Coordinates],
  selectedSlot:    Option[SlotId],
  agsOverlay:      Boolean,
  selectedGS:      Option[AgsAnalysis.Usable]
): HookResult[Option[(Css, Option[SortedMap[Css, ShapeExpression]])]] =
  useMemo(
    (vizConf, baseCoordinates, blindOffset, slotCoords, selectedSlot, agsOverlay, selectedGS)
  ) {
    (
      vizConf,
      baseCoordinates,
      blindOffset,
      slotCoords,
      selectedSlot,
      agsOverlay,
      selectedGS
    ) =>
      val candidatesVisibilityCss: Css =
        ExploreStyles.GuideStarCandidateVisible.when_(agsOverlay)

      (vizConf.map(_.configuration.obsModeType), baseCoordinates).flatMapN: (conf, baseCoords) =>
        val maskShapes: Option[SortedMap[Css, ShapeExpression]] =
          vizConf.flatMap(_.maskDesign).flatMap(mosMaskShapes)

        // The fitted mask replaces the nominal slit placement area the instrument
        // geometry draws, so drop the latter when a design is available.
        def withMaskShapes(
          shapes: Option[SortedMap[Css, ShapeExpression]]
        ): Option[SortedMap[Css, ShapeExpression]] =
          val nominalAreas =
            List(VisualizationStyles.GmosScienceCcd, VisualizationStyles.Flamingos2ScienceArea)
              .map(_.htmlClass)
          (shapes, maskShapes) match
            case (Some(s), Some(m)) =>
              (s.filterNot((css, _) => nominalAreas.exists(css.htmlClass.contains)) ++ m).some
            case (s, m)             => s.orElse(m)

        conf match
          case ObservingModeType.Flamingos2LongSlit                                      =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                VisualizationStyles.Flamingos2ProbeArmVisible

            (probeVisibilityCss,
             Flamingos2Geometry.f2Geometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.flatMap(_.guidedAcqOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               PortDisposition.Side,
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case ObservingModeType.Flamingos2Imaging                                       =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                VisualizationStyles.Flamingos2ProbeArmVisible

            (probeVisibilityCss,
             Flamingos2Geometry.f2Geometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.flatMap(_.guidedAcqOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               PortDisposition.Side,
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case ObservingModeType.Flamingos2Mos                                           =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                VisualizationStyles.Flamingos2ProbeArmVisible

            (probeVisibilityCss,
             withMaskShapes(
               Flamingos2Geometry.f2Geometry(
                 baseCoords,
                 blindOffset,
                 vizConf.flatMap(_.guidedSciOffsets),
                 vizConf.flatMap(_.guidedAcqOffsets),
                 vizConf.map(_.posAngle),
                 vizConf.map(_.configuration),
                 PortDisposition.Side,
                 vizConf.flatMap(_.trackType),
                 selectedGS,
                 candidatesVisibilityCss
               )
             )
            ).some
          case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                Css.Empty

            (probeVisibilityCss,
             GmosGeometry.gmosGeometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.flatMap(_.guidedAcqOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               PortDisposition.Side,
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging   =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.GmosCcdVisible |+| VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                VisualizationStyles.GmosCcdVisible

            (probeVisibilityCss,
             GmosGeometry.gmosGeometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.flatMap(_.guidedAcqOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               PortDisposition.Side,
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case ObservingModeType.GmosNorthMos | ObservingModeType.GmosSouthMos           =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.GmosCcdVisible |+| VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                VisualizationStyles.GmosCcdVisible

            (probeVisibilityCss,
             withMaskShapes(
               GmosGeometry.gmosGeometry(
                 baseCoords,
                 blindOffset,
                 vizConf.flatMap(_.guidedSciOffsets),
                 vizConf.flatMap(_.guidedAcqOffsets),
                 vizConf.map(_.posAngle),
                 vizConf.map(_.configuration),
                 PortDisposition.Side,
                 vizConf.flatMap(_.trackType),
                 selectedGS,
                 candidatesVisibilityCss
               )
             )
            ).some
          case ObservingModeType.Igrins2LongSlit                                         =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                Css.Empty

            (probeVisibilityCss,
             Igrins2Geometry.igrins2Geometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case ObservingModeType.GhostIfu                                                =>
            val probeVisibilityCss = VisualizationStyles.GhostIfuPatrolFieldVisible |+|
              (vizConf.flatMap(_.guideProbe) match
                case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                  VisualizationStyles.PwfsProbeArmVisible
                case _                                               =>
                  Css.Empty)

            val ifu1Coords = slotCoords.get(SlotId.GhostIfu1)
            val ifu2Coords = slotCoords.get(SlotId.GhostIfu2)

            // Only force the (empty) IFU2 patrol field once IFU1 is assigned, i.e. the mode is
            // accepted with a science target. Otherwise the preview would show IFU2 but not IFU1.
            val forceShowIfu2 =
              ifu1Coords.isDefined && vizConf.exists(GhostSkySlot.isIfu2AvailableForSky)

            (probeVisibilityCss,
             GhostGeometry.ghostGeometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss,
               ifu1Coords,
               ifu2Coords,
               selectedSlot.contains(SlotId.GhostIfu1),
               selectedSlot.contains(SlotId.GhostIfu2),
               forceShowIfu2
             )
            ).some
          case ObservingModeType.GnirsImaging | ObservingModeType.GnirsLongSlit |
              ObservingModeType.GnirsIfu =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                Css.Empty

            (probeVisibilityCss,
             GnirsGeometry.gnirsGeometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case _: VisitorObservingModeType                                               =>
            val probeVisibilityCss = vizConf.flatMap(_.guideProbe) match
              case Some(GuideProbe.PWFS2) | Some(GuideProbe.PWFS1) =>
                VisualizationStyles.PwfsProbeArmVisible
              case _                                               =>
                Css.Empty

            (probeVisibilityCss,
             VisitorGeometry.visitorGeometry(
               baseCoords,
               blindOffset,
               vizConf.flatMap(_.guidedSciOffsets),
               vizConf.map(_.posAngle),
               vizConf.map(_.configuration),
               vizConf.flatMap(_.trackType),
               selectedGS,
               candidatesVisibilityCss
             )
            ).some
          case _: ExchangeObservingModeType                                              =>
            none
  }.map(_.value)
