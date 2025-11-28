// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinMouseScroll
import explore.model.AsterismVisualOptions
import explore.model.ConfigurationForVisualization
import explore.model.GlobalPreferences
import explore.model.ObservationTargets
import explore.model.enums.Visible
import explore.model.extensions.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis
import lucuma.ags.Ags
import lucuma.ags.GeometryType
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Target
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
  vizConf:                Option[ConfigurationForVisualization],
  globalPreferences:      GlobalPreferences,
  options:                AsterismVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis.Usable],
  guideStarCandidates:    List[AgsAnalysis.Usable],
  anglesToTest:           Option[NonEmptyList[Angle]]
) extends ReactFnProps(AladinContainer.component):
  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, vizConf.flatMap(_.selectedPosAngleConstraint))

  val blindOffset: Option[Coordinates] =
    obsTargets.blindOffsetSiderealTracking.flatMap(_.at(obsTime))

  val pfVisibility = GlobalPreferences.pfVisibility.get(globalPreferences)

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // We need to detect if the selected GS deserves a refresh, this could be if the
  // selected target changes or if e.g. the pos angle change for the same target
  given Reusability[AgsAnalysis.Usable] =
    Reusability.by(u => (u.target, u.posAngle))

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
    surveyCoords:  Option[Coordinates],
    targetSVG:     Coordinates => SVGTarget,
    lineStyle:     Css
  ): List[SVGTarget] =
    targetSVG(obsTimeCoords) ::
      surveyCoords
        .map: source =>
          SVGTarget.LineTo(source, obsTimeCoords, lineStyle)
        .toList

  private def guideStarsSVG(
    g:                          AgsAnalysis.Usable,
    candidates:                 List[AgsAnalysis.Usable],
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

    val (epochCoords, obsTimeCoords) =
      tracking.trackedPositions(surveyEpoch.some, siderealDiscretizedObsTime.obsTime)

    def guideTargetSVG(coords: Coordinates): SVGTarget =
      if (selectedGS.forall(_.target.id === g.target.id)) {
        SVGTarget.GuideStarTarget(coords, candidateCss, calcSize(4), g)
      } else {
        val css  =
          candidateCss |+| candidatesVisibility |+|
            ExploreStyles.GuideStarCandidateCrowded.unless_(candidates.length < 500)
        val size = if (candidates.length < 500) calcSize(3) else calcSize(2.7)
        SVGTarget.GuideStarCandidateTarget(coords, css, size, g)
      }

    if (candidates.length < 500) {
      svgTargetAndLine(
        obsTimeCoords,
        epochCoords,
        guideTargetSVG,
        ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
      )
    } else {
      List(guideTargetSVG(obsTimeCoords))
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

    candidates
      .flatMap:
        guideStarsSVG(
          _,
          candidates,
          siderealDiscretizedObsTime,
          configuration,
          selectedGS,
          candidatesVisibility,
          calcSize,
          surveyEpoch
        )
  }

  private def renderTargets(
    targets:      List[TargetWithId],
    obsTime:      Instant,
    focusId:      Target.Id,
    targetSVG:    (TargetWithId, Boolean, Coordinates) => SVGTarget,
    lineStyle:    Css,
    isSelectable: Boolean,
    surveyEpoch:  Epoch
  ): List[SVGTarget] =
    targets.flatMap: t =>
      t.toSidereal.toList.flatMap: siderealT =>
        val (epochCoords, obsTimeCoords) =
          siderealT.target.tracking.trackedPositions(surveyEpoch.some, obsTime)

        val isSelected = isSelectable && t.id === focusId

        svgTargetAndLine(
          obsTimeCoords,
          epochCoords,
          targetSVG(t, isSelected, _),
          lineStyle
        )

  private def baseAndScience(
    p:           Props,
    surveyEpoch: Epoch
  ): (Option[Coordinates], List[(Boolean, NonEmptyString, Option[Coordinates], Coordinates)]) = {
    // selected, name, start coords, end coords
    val baseObsTimeCoords =
      p.obsTargets.baseTracking
        .flatMap(_.at(p.obsTime))

    val science = p.obsTargets.mapScience: t =>
      t.toSidereal.map: siderealT =>
        val (epochCoords, obsTimeCoords) =
          siderealT.target.tracking
            .trackedPositions(surveyEpoch.some, p.obsTime)

        (t.id === p.obsTargets.focus.id, t.target.name, epochCoords, obsTimeCoords)

    (baseObsTimeCoords, science.flattenOption)
  }

  private val CutOff = Wavelength.fromIntMicrometers(1).get

  // Relative size for targets passed to the svg layer
  private val TargetSize = 6

  private def surveyForWavelength(w: Wavelength) =
    if (w > CutOff)
      ImageSurvey.TWOMASS
    else
      ImageSurvey.DSS

  private val component =
    ScalaFnComponent[Props]: props =>
      val initialSurvey = props.vizConf
        .flatMap(_.centralWavelength)
        .map(w => surveyForWavelength(w.value))
        .getOrElse(ImageSurvey.DSS)

      for {
        // Base coordinates and science targets with pm correction if possible
        baseCoords   <- useState(baseAndScience(props, initialSurvey.epoch))
        // View coordinates base coordinates with pm correction + user panning
        currentPos   <-
          useState(baseCoords.value._1.flatMap(_.offsetBy(Angle.Angle0, props.options.viewOffset)))
        // Survey
        survey       <- useState(initialSurvey)
        // Update coordinates if obsTargets or obsTime or survey changes
        _            <- useEffectWithDeps((props.obsTargets, props.obsTime, survey)): (_, _, s) =>
                          val (base, science) = baseAndScience(props, s.value.epoch)
                          baseCoords.setState((base, science)) *>
                            currentPos.setState(
                              base
                                .flatMap(_.offsetBy(Angle.Angle0, props.options.viewOffset))
                            )
        aladinRef    <- useState(none[Aladin])
        // If view offset changes upstream to zero, redraw
        _            <- useEffectWithDeps((baseCoords, props.options.viewOffset)): (_, offset) =>
                          val newCoords = baseCoords.value._1.flatMap(_.offsetBy(Angle.Angle0, offset))
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
                          baseCoords.value._1,
                          props.blindOffset,
                          props.globalPreferences.agsOverlay,
                          props.selectedGuideStar
                        )
        // Debug patrol field shapes using custom hook
        pfShapes     <- usePatrolFieldShapes(
                          props.vizConf,
                          props.selectedGuideStar,
                          baseCoords.value._1,
                          props.blindOffset,
                          props.pfVisibility,
                          props.anglesToTest
                        )
        // AGS positions for offset indicators (uses single angle)
        agsPositions <- useMemo(
                          (props.vizConf, props.selectedGuideStar, baseCoords, props.blindOffset)
                        ) { (vizConf, selectedGS, baseCoords, blindOffset) =>
                          for {
                            baseCoordinates <- baseCoords.value._1
                          } yield {
                            val posAngle = selectedGS
                              .map(_.posAngle)
                              .orElse(vizConf.map(_.posAngle))
                              .getOrElse(Angle.Angle0)

                            Ags.generatePositions(
                              baseCoordinates,
                              blindOffset,
                              NonEmptyList.one(posAngle),
                              vizConf.flatMap(_.acquisitionOffsets).map(AcquisitionOffsets.apply),
                              vizConf.flatMap(_.scienceOffsets).map(ScienceOffsets.apply)
                            )
                          }
                        }
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
                           baseCoords,
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
                            baseCoords,
                            survey
                          ) =>
                            selectedGS.posAngle.foldMap: _ =>
                              val (_, scienceTargets) = baseCoords.value
                              guideStars(
                                candidates,
                                visible,
                                fovRA,
                                siderealDiscretizedObsTime,
                                configuration,
                                selectedGS,
                                survey.value.epoch
                              )
        // Use fov from aladin
        fov          <- useState(none[Fov])
        // Update survey if conf changes
        _            <- useEffectWithDeps(props.vizConf.flatMap(_.centralWavelength.map(_.value))):
                          _.map(w => survey.setState(surveyForWavelength(w))).getOrEmpty
      } yield {
        val (baseCoordinates, scienceTargets) = baseCoords.value

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
              SVGTarget.CrosshairTarget(c, Css.Empty, 10)
            )

        val targetPositions = renderTargets(
          props.obsTargets.mapScience(identity),
          props.obsTime,
          props.obsTargets.focus.id,
          (t, selected, coords) =>
            SVGTarget.ScienceTarget(
              coords,
              ExploreStyles.ScienceTarget,
              ExploreStyles.ScienceSelectedTarget,
              TargetSize,
              selected,
              t.target.name.value.some
            ),
          ExploreStyles.PMCorrectionLine,
          props.obsTargets.length > 1,
          survey.value.epoch
        )

        // Offset indicators using pre-computed locations from AGS positions
        // pos.location is already rotated: (offsetPos - pivot).rotate(posAngle) + pivot
        val offsetPositions = agsPositions.value.toList.flatMap { positions =>
          // Science offset indicators
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
                    4
                  )
                }
            } else Nil

          // Acquisition offset indicators
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
                    4
                  )
                }
            } else Nil

          // order is important, science to be drawn above acq
          acquisitionOffsets ++ scienceOffsets
        }

        // Render blind offset targets from obsTargets separately
        val blindOffsetTargets = renderTargets(
          props.obsTargets.blindOffsetTargets,
          props.obsTime,
          props.obsTargets.focus.id,
          (t, selected, coords) =>
            SVGTarget.BlindOffsetTarget(
              coords,
              Css.Empty,
              ExploreStyles.BlindOffsetSelectedTarget,
              TargetSize,
              selected,
              t.target.name.value.some
            ),
          ExploreStyles.BlindOffsetLine,
          props.obsTargets.length > 1,
          survey.value.epoch
        )

        val screenOffset =
          (currentPos.value, baseCoordinates).mapN(_.diff(_).offset).getOrElse(Offset.Zero)

        // Use explicit reusability that excludes target changes
        given Reusability[AladinOptions] = reusability.withoutTarget

        <.div.withRef(resize.ref)(ExploreStyles.AladinContainerBody)(
          // This is a bit tricky. Sometimes the height can be 0 or a very low number.
          // This happens during a second render. If we let the height to be zero, aladin
          // will take it as 1. This height ends up being a denominator, which, if low,
          // will make aladin request a large amount of tiles and end up freeze explore.
          if (resize.height.exists(_ >= 100)) {
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
                    // Order matters
                    candidates ++ blindOffsetTargets ++ targetPositions ++ basePosition ++ offsetPositions
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
