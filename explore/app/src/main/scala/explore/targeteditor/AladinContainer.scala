// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinMouseScroll
import explore.model.AsterismVisualOptions
import explore.model.ConfigurationForVisualization
import explore.model.GlobalPreferences
import explore.model.ObservationTargets
import explore.model.ObservationTargetsCoordinatesAt
import explore.model.enums.Visible
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GeometryType
import lucuma.ags.GuideStarCandidate
import lucuma.core.model.ConstraintSet
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.TargetWithId
import lucuma.ui.aladin.*
import lucuma.ui.reusability
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.visualization.*

import java.time.Instant
import scala.concurrent.duration.*
import scala.scalajs.LinkingInfo

case class AladinContainer(
  obsTargets:             ObservationTargets,
  obsTime:                Instant,
  obsTimeTracking:        Map[Target.Id, Tracking],
  obsTimeCoords:          ObservationTargetsCoordinatesAt,
  vizConf:                Option[ConfigurationForVisualization],
  globalPreferences:      GlobalPreferences,
  options:                AsterismVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis.Usable],
  guideStarCandidates:    List[AgsAnalysis.Usable],
  allCatalogStars:        List[GuideStarCandidate],
  anglesToTest:           Option[NonEmptyList[Angle]],
  constraints:            Option[ConstraintSet],
  agsParams:              Option[AgsParams]
) extends ReactFnProps(AladinContainer.component):
  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, vizConf.flatMap(_.selectedPosAngleConstraint))

  val site = vizConf.map(_.configuration.siteFor).getOrElse(Site.GN)

  val pfVisibility = GlobalPreferences.pfVisibility.get(globalPreferences)

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // Relative sizes for targets passed to the svg layer, in terms of the side of
  // the svg projected onto the focal plane
  private val TargetSize             = 6
  private val CrosshairSize          = 10
  private val OffsetIndicatorSize    = 4
  private val GuideStarSize          = 4
  private val GuideStarCandidateSize = 3
  private val GuideStarCrowdedSize   = 2.7
  private val CrowdedThreshold       = 500

  extension (tr: SiderealTracking)
    private def coordsAtEpoch(epoch: Epoch): Option[Coordinates] =
      tr.at(epoch.toInstant)
    private def atOrBase(at: Instant): Coordinates               =
      tr.at(at).getOrElse(tr.baseCoordinates)

  // We need to detect if the selected GS deserves a refresh, this could be if the
  // selected target changes or if e.g. the pos angle change for the same target
  private given Reusability[AgsAnalysis.Usable] =
    Reusability.by(u => (u.target, u.posAngle))

  private given Reusability[List[AgsAnalysis.Usable]] = Reusability.by(_.length)

  private given Reusability[Map[Target.Id, Tracking]] = Reusability.by(_.toList)

  private def speedCss(gs: GuideSpeed): Css =
    gs match
      case GuideSpeed.Fast   =>
        ExploreStyles.GuideSpeedFast
      case GuideSpeed.Medium =>
        ExploreStyles.GuideSpeedMedium
      case GuideSpeed.Slow   =>
        ExploreStyles.GuideSpeedSlow

  private def svgTargetAndLine(
    obsTimeCoords: Coordinates,
    linePoints:    List[Coordinates],
    targetSVG:     Coordinates => SVGTarget,
    lineStyle:     Css
  ): List[SVGTarget] =
    targetSVG(obsTimeCoords) ::
      linePoints.sliding2.map: (from, to) =>
        pprint.pprintln(s" line ${Angle.arcseconds.get(from.angularDistance(to))} mas $from to $from")
        SVGTarget.LineTo(from, to, lineStyle)

  private def guideStarsSVG(
    g:                          AgsAnalysis.Usable,
    isCrowded:                  Boolean,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    configuration:              Option[BasicConfiguration],
    selectedGS:                 Option[AgsAnalysis.Usable],
    candidatesVisibility:       Css,
    calcSize:                   Double => Double,
    surveyEpoch:                Epoch
  ): List[SVGTarget] = {
    val tracking     = g.target.tracking
    val candidateCss =
      if (configuration.isEmpty) Css.Empty else speedCss(g.guideSpeed)

    val obsTimeCoords: Coordinates    = tracking.atOrBase(siderealDiscretizedObsTime.obsTime)
    val linePoints: List[Coordinates] =
      tracking.coordsAtEpoch(surveyEpoch).foldMap(List(_, obsTimeCoords))

    pprint.pprintln(s"guidestars $isCrowded $surveyEpoch ${siderealDiscretizedObsTime.obsTime}")
    pprint.pprintln(s"gaia id: ${g.target.id}")
    pprint.pprintln(linePoints)

    def guideTargetSVG(coords: Coordinates): SVGTarget =
      if (selectedGS.forall(_.target.id === g.target.id)) {
        SVGTarget.GuideStarTarget(coords, candidateCss, calcSize(GuideStarSize), g)
      } else {
        val css  =
          candidateCss |+| candidatesVisibility |+|
            ExploreStyles.GuideStarCandidateCrowded.when_(isCrowded)
        val size =
          if (isCrowded) calcSize(GuideStarCrowdedSize)
          else calcSize(GuideStarCandidateSize)
        SVGTarget.GuideStarCandidateTarget(coords, css, size, g)
      }

    if (isCrowded) {
      List(guideTargetSVG(obsTimeCoords))
    } else {
      svgTargetAndLine(
        obsTimeCoords,
        linePoints,
        guideTargetSVG,
        ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
      )
    }
  }

  private def guideStars(
    candidates:                 List[AgsAnalysis.Usable],
    visible:                    Boolean,
    fovRA:                      Angle,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    configuration:              Option[BasicConfiguration],
    selectedGS:                 Option[AgsAnalysis.Usable],
    surveyEpoch:                Epoch
  ): List[SVGTarget] = {

    val fov = fovRA.toMicroarcseconds / 1e6

    def calcSize(size: Double): Double = size.max(size * (225 / fov))

    val candidatesVisibility =
      ExploreStyles.GuideStarCandidateVisible.when_(visible)

    val isCrowded = candidates.length >= CrowdedThreshold

    val cd = candidates
      .flatMap:
        guideStarsSVG(
          _,
          isCrowded,
          siderealDiscretizedObsTime,
          configuration,
          selectedGS,
          candidatesVisibility,
          calcSize,
          surveyEpoch
        )
    println(s"candidates ${cd.length}")
    cd
  }

  private val CatalogStarSize = 4

  private def debugCatalogStarsSVG(
    analyses:                   List[AgsAnalysis],
    fovRA:                      Angle,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime
  ): List[SVGTarget] = {
    val fov                            = fovRA.toMicroarcseconds / 1e6
    def calcSize(size: Double): Double = size.max(size * (225 / fov))

    analyses.map { analysis =>
      val coords = analysis.target.tracking
        .at(siderealDiscretizedObsTime.obsTime)
        .getOrElse(analysis.target.tracking.baseCoordinates)
      SVGTarget.DebugCatalogStarTarget(
        coords,
        Css.Empty,
        calcSize(CatalogStarSize),
        analysis
      )
    }
  }

  private case class TargetCoords(
    target:        TargetWithId,
    isSelected:    Boolean,
    obsTimeCoords: Coordinates,
    linePoints:    List[Coordinates]
  ):
    val targetName: String = target.target.name.value

  private def targetCoordinates(
    obsTargets:  ObservationTargets,
    trackingMap: Map[Target.Id, Tracking],
    obsCoords:   ObservationTargetsCoordinatesAt,
    surveyEpoch: Epoch
  ): List[TargetCoords] =
    obsTargets
      .map: t =>
        obsCoords
          .forTarget(t.id)
          .map: coords =>
            val linePoints: List[Coordinates] = trackingMap
              .get(t.id)
              .foldMap: tracking =>
                tracking match
                  case EphemerisTracking(toMap) =>
                    // Show a line for the entire ephemeris - should be the observing day
                    toMap.map((_, ec) => ec.coord).toList
                  case s: SiderealTracking      =>
                    // Show a line from coords at the epoch to the current coords
                    s.coordsAtEpoch(surveyEpoch).foldMap(List(_, coords))
                  case _                        => List.empty

            TargetCoords(t, t.id === obsTargets.focus.id, coords, linePoints)
      .toList
      .flattenOption

  private val CutOff = Wavelength.fromIntMicrometers(1).get

  private def surveyForWavelength(w: Wavelength) =
    if (w > CutOff)
      ImageSurvey.TWOMASS
    else
      ImageSurvey.DSS

  private def positionFromBaseAndOffset(
    base:   Option[Coordinates],
    offset: Offset
  ): Option[Coordinates] =
    base.flatMap(_.offsetBy(Angle.Angle0, offset))

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        currentPos   <-
          useState[Option[Coordinates]](
            positionFromBaseAndOffset(props.obsTimeCoords.baseCoords, props.options.viewOffset)
          )
        survey       <- useMemo(props.vizConf.flatMap(_.centralWavelength.map(_.value))):
                          _.map(surveyForWavelength).getOrElse(ImageSurvey.DSS)
        targetCoords <-
          useMemo(
            (props.obsTargets, survey, props.obsTimeTracking, props.obsTimeCoords)
          ): (obsTargets, s, trackingMap, coords) =>
            targetCoordinates(obsTargets, trackingMap, coords, s.value.epoch)
        // Update coordinates if obsTargets or obsTime or survey changes
        // NOTE: Do not update the dependencies, or you might break sh@t. If this updates
        // too often, `Center on Target` will not work.
        _            <- useEffectWithDeps((props.obsTargets, props.obsTime, survey)): (_, _, _) =>
                          currentPos.setState(
                            positionFromBaseAndOffset(props.obsTimeCoords.baseCoords, props.options.viewOffset)
                          )
        aladinRef    <- useState(none[Aladin])
        // If view offset changes upstream to zero, redraw
        _            <-
          useEffectWithDeps((props.obsTimeCoords.baseCoords, props.options.viewOffset)):
            (baseCoords, offset) =>
              val newCoords = positionFromBaseAndOffset(baseCoords, offset)
              newCoords
                .map: coords =>
                  aladinRef.value
                    .traverse(_.gotoRaDecCB(coords))
                    .void
                    .when_(offset === Offset.Zero)
                .getOrEmpty
        // Memoized svg for visualization shapes
        shapes       <- useVisualizationShapes(
                          props.vizConf,
                          props.obsTimeCoords.baseCoords,
                          props.obsTimeCoords.blindOffsetCoords,
                          props.globalPreferences.agsOverlay,
                          props.selectedGuideStar
                        )
        // patrol field shapes for debugging
        pfShapes     <- usePatrolFieldShapes(
                          props.vizConf,
                          props.selectedGuideStar,
                          props.obsTimeCoords.baseCoords,
                          props.obsTimeCoords.blindOffsetCoords,
                          props.pfVisibility,
                          props.anglesToTest
                        )
        agsPositions <- useMemo(
                          (props.vizConf,
                           props.selectedGuideStar,
                           props.obsTimeCoords.baseCoords,
                           props.obsTimeCoords.blindOffsetCoords
                          )
                        ): (vizConf, selectedGS, baseCoords, blindOffset) =>
                          baseCoords.map: baseCoordinates =>
                            val posAngle = selectedGS
                              .map(_.posAngle)
                              .orElse(vizConf.map(_.posAngle))
                              .getOrElse(Angle.Angle0)
                            // println(s"posAngle $posAngle")

                            val pos = Ags.generatePositions(
                              baseCoordinates,
                              blindOffset,
                              NonEmptyList.one(posAngle),
                              vizConf.flatMap(_.asAcqOffsets),
                              vizConf.flatMap(_.asSciOffsets)
                            )
                            // println(s"pos $pos")
                            pos
        // resize detector
        resize       <- useResizeDetector
        // memoized catalog targets with their proper motions corrected
        candidates   <- useMemo(
                          (props.guideStarCandidates,
                           props.globalPreferences.showCatalog,
                           props.globalPreferences.fullScreen,
                           props.options.fovRA,
                           props.siderealDiscretizedObsTime,
                           props.vizConf.map(_.configuration),
                           props.selectedGuideStar,
                           survey
                          )
                        ):
                          (
                            candidates,
                            visible,
                            _,
                            fovRA,
                            siderealDiscretizedObsTime,
                            configuration,
                            selectedGS,
                            survey
                          ) =>
                            // println(selectedGS.posAngle)
                            selectedGS.posAngle.foldMap: _ =>
                              guideStars(
                                candidates,
                                visible,
                                fovRA,
                                siderealDiscretizedObsTime,
                                configuration,
                                selectedGS,
                                survey.value.epoch
                              )
        // memoized all catalog stars with AGS analysis (debug only)
        allCatalog   <- useMemo(
                          (props.allCatalogStars,
                           props.pfVisibility.showAllCatalogStars,
                           props.options.fovRA,
                           props.siderealDiscretizedObsTime,
                           props.obsTimeCoords,
                           props.vizConf,
                           props.anglesToTest,
                           props.constraints,
                           props.agsParams
                          )
                        ):
                          (
                            allStars,
                            showAll,
                            fovRA,
                            siderealDiscretizedObsTime,
                            obsTimeCoords,
                            vizConf,
                            anglesToTest,
                            constraints,
                            agsParams
                          ) =>
                            if (showAll.value && allStars.nonEmpty)
                              // Run AGS analysis locally for debug visualization
                              // Apply PM correction to candidates before analysis
                              val correctedStars              =
                                allStars.map(_.at(siderealDiscretizedObsTime.obsTime))
                              val analyses: List[AgsAnalysis] =
                                (obsTimeCoords.baseCoords,
                                 vizConf.flatMap(_.centralWavelength.map(_.value)),
                                 anglesToTest,
                                 constraints,
                                 agsParams
                                ).mapN: (baseCoords, wavelength, angles, constraints, params) =>
                                  Ags.agsAnalysis(
                                    constraints,
                                    wavelength,
                                    baseCoords,
                                    obsTimeCoords.scienceCoords,
                                    obsTimeCoords.blindOffsetCoords,
                                    angles,
                                    vizConf.flatMap(_.asAcqOffsets),
                                    vizConf.flatMap(_.asSciOffsets),
                                    params,
                                    correctedStars
                                  )
                                .getOrElse(List.empty)
                              debugCatalogStarsSVG(analyses, fovRA, siderealDiscretizedObsTime)
                            else List.empty
        // Use fov from aladin
        fov          <- useState(none[Fov])
      } yield {
        val baseCoordinates: Option[Coordinates] = props.obsTimeCoords.baseCoords

        /**
         * Called when the position changes, i.e. aladin pans. We want to offset the visualization
         * to keep the internal target correct
         */
        def onPositionChanged(u: PositionChanged): Callback = {
          val viewCoords = Coordinates(u.ra, u.dec)
          val viewOffset = baseCoordinates.map(_.diff(viewCoords).offset)
          currentPos.setState(Some(viewCoords)) *>
            props.updateViewOffset(viewOffset.getOrElse(Offset.Zero))
        }

        def onZoom =
          (v: Fov) => {
            // Sometimes get 0 fov, ignore those
            val ignore =
              (v.x === Angle.Angle0 && v.y === Angle.Angle0) ||
                fov.value.exists(_.isDifferentEnough(v))
            (fov.setState(v.some) *> props.updateFov(v)).unless_(ignore)
          }

        val includeSvg: Aladin => Callback = (v: Aladin) =>
          aladinRef.setState(v.some) *>
            v.onZoomCB(onZoom) *> // re render on zoom
            v.onPositionChangedCB(onPositionChanged) *>
            v.onMouseMoveCB(s =>
              props
                .updateMouseCoordinates(Coordinates(s.ra, s.dec))
                .rateLimit(200.millis, 1)
                .void
            )

        val baseCoordinatesForAladin: String =
          currentPos.value
            .foldMap(Coordinates.fromHmsDms.reverseGet)

        val basePosition =
          baseCoordinates.foldMap: c =>
            List(
              SVGTarget.CrosshairTarget(c, Css.Empty, CrosshairSize)
            )

        val isSelectable: Boolean = props.obsTargets.length > 1

        val scienceTargets: List[SVGTarget] =
          targetCoords
            .filterNot(_.target.disposition === TargetDisposition.BlindOffset)
            .flatMap: tc =>
              def targetSvg(coords: Coordinates) = SVGTarget.ScienceTarget(
                coords,
                ExploreStyles.ScienceTarget,
                ExploreStyles.ScienceSelectedTarget,
                TargetSize,
                tc.isSelected && isSelectable,
                tc.targetName.some
              )
              svgTargetAndLine(
                tc.obsTimeCoords,
                tc.linePoints,
                targetSvg,
                lineStyle = ExploreStyles.PMCorrectionLine
              )

        // Offset indicators calculated and rotated directly by ags
        val offsetPositions = agsPositions.value.toList.flatMap { positions =>
          // Science offsets
          val scienceOffsets =
            if (props.globalPreferences.scienceOffsets.value) {
              positions.toList
                .filter(_.geometryType == GeometryType.SciOffset)
                .zipWithIndex
                .flatMap { case (pos, i) =>
                  for {
                    idx <- refineV[NonNegative](i).toOption
                    // pos.location is already rotated, apply with Angle0
                    c   <- baseCoordinates.flatMap(_.offsetBy(Angle.Angle0, pos.location))
                  } yield SVGTarget.OffsetIndicator(
                    c,
                    idx,
                    pos.offsetPos,
                    SequenceType.Science,
                    ExploreStyles.ScienceOffsetPosition,
                    OffsetIndicatorSize
                  )
                }
            } else Nil

          // Acquisition offsets
          val acquisitionOffsets =
            if (props.globalPreferences.acquisitionOffsets.value) {
              positions.toList
                .filter(_.geometryType == GeometryType.AcqOffset)
                .zipWithIndex
                .flatMap { case (pos, i) =>
                  for {
                    idx <- refineV[NonNegative](i).toOption
                    // pos.location is already rotated, apply with Angle0
                    c   <- baseCoordinates.flatMap(_.offsetBy(Angle.Angle0, pos.location))
                  } yield SVGTarget.OffsetIndicator(
                    c,
                    idx,
                    pos.offsetPos,
                    SequenceType.Acquisition,
                    ExploreStyles.AcquisitionOffsetPosition,
                    OffsetIndicatorSize
                  )
                }
            } else Nil

          // order is important, science to be drawn above acq
          acquisitionOffsets ++ scienceOffsets
        }

        val blindOffsets: List[SVGTarget] =
          targetCoords
            .filter(tc => tc.target.disposition === TargetDisposition.BlindOffset)
            .flatMap: tc =>
              def targetSvg(coords: Coordinates) = SVGTarget.BlindOffsetTarget(
                coords,
                Css.Empty,
                ExploreStyles.BlindOffsetSelectedTarget,
                TargetSize,
                tc.isSelected && isSelectable,
                tc.targetName.some
              )
              svgTargetAndLine(
                tc.obsTimeCoords,
                tc.linePoints,
                targetSvg,
                ExploreStyles.BlindOffsetLine
              )

        // Use explicit reusability that excludes target changes
        given Reusability[AladinOptions] = reusability.withoutTarget

        <.div.withRef(resize.ref)(ExploreStyles.AladinContainerBody)(
          // This is a bit tricky. Sometimes the height can be 0 or a very low number.
          // This happens during a second render. If we let the height to be zero, aladin
          // will take it as 1. This height ends up being a denominator, which, if low,
          // will make aladin request a large amount of tiles and end up freeze explore.
          if (resize.height.exists(_ >= 100)) {
            val screenOffset =
              (currentPos.value, baseCoordinates).mapN(_.diff(_).offset).getOrElse(Offset.Zero)

            ReactFragment(
              aladinRef.value.map(AladinZoomControl(_)),
              HelpIcon("aladin-cell.md".refined, ExploreStyles.AladinHelpIcon),
              <.div(ExploreStyles.AladinSurvey, s"Survey: ${survey.value.name}"),
              (resize.width, resize.height, fov.value, baseCoordinates)
                .mapN(
                  TargetsOverlay(
                    _,
                    _,
                    _,
                    screenOffset,
                    _,
                    // Order matters - allCatalog first so it's rendered behind other targets
                    allCatalog ++ candidates ++ blindOffsets ++ scienceTargets ++ basePosition ++ offsetPositions
                  )
                ),
              (resize.width,
               resize.height,
               fov.value,
               shapes.flatMap(_._2.flatMap(NonEmptyMap.fromMap)),
               shapes.map(_._1)
              )
                .mapN(
                  SVGVisualizationOverlay(
                    _,
                    _,
                    _,
                    screenOffset,
                    _,
                    _
                  )
                ),
              Option.when(LinkingInfo.developmentMode)(
                (resize.width,
                 resize.height,
                 fov.value,
                 pfShapes.flatMap(m => NonEmptyMap.fromMap(m))
                )
                  .mapN(
                    SVGVisualizationOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      _,
                      Css.Empty
                    )
                  )
              ),
              ReactAladin(
                ExploreStyles.TargetAladin,
                AladinOptions(
                  target = baseCoordinatesForAladin,
                  fov = Angle.fromMicroarcseconds(
                    props.options.fovDec.toMicroarcseconds
                      .max(props.options.fovRA.toMicroarcseconds)
                  ),
                  survey = survey.value,
                  showReticle = false,
                  showLayersControl = false,
                  showGotoControl = false,
                  showZoomControl = false,
                  showProjectionControl = false,
                  showSimbadPointerControl = false,
                  showFullscreenControl = false,
                  showCooLocation = false,
                  showFov = false
                ),
                customize = includeSvg,
                panningEnabled = props.globalPreferences.aladinMouseScroll.value
              )
            )
          } else EmptyVdom
        )
      }
}
