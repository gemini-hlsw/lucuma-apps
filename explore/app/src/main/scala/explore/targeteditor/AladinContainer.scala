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
import explore.model.Asterism
import explore.model.AsterismVisualOptions
import explore.model.ConfigurationForVisualization
import explore.model.GlobalPreferences
import explore.model.enums.Visible
import explore.model.extensions.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Tracking
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.aladin.*
import lucuma.ui.reusability
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.visualization.*

import java.time.Instant
import scala.concurrent.duration.*

case class AladinContainer(
  asterism:               Asterism,
  asterismTracking:       Tracking,
  obsTime:                Instant,
  vizConf:                Option[ConfigurationForVisualization],
  globalPreferences:      GlobalPreferences,
  options:                AsterismVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis.Usable],
  guideStarCandidates:    List[AgsAnalysis.Usable]
) extends ReactFnProps(AladinContainer.component):
  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, vizConf.flatMap(_.selectedPosAngleConstraint))

  val blindOffset: Option[SiderealTracking] = vizConf.flatMap(_.blindOffset)

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // We need to detect if the selected GS deserves a refresh, this could be if the
  // selected target changes or if e.g. the pos angle change for the same target
  private given Reusability[AgsAnalysis.Usable] =
    Reusability.by(u => (u.target, u.posAngle))

  private given Reusability[List[AgsAnalysis.Usable]] = Reusability.by(_.length)

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

    val (surveyCoords, obsTimeCoords) =
      tracking.trackedPositions(siderealDiscretizedObsTime.obsTime, surveyEpoch.some)

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
        surveyCoords,
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
    scienceTargets:             List[(Boolean, NonEmptyString, Option[Coordinates], Coordinates)],
    surveyEpoch:                Epoch
  ): List[SVGTarget] = {

    val fov = fovRA.toMicroarcseconds / 1e6

    def calcSize(size: Double): Double = size.max(size * (225 / fov))

    val candidatesVisibility =
      ExploreStyles.GuideStarCandidateVisible.when_(visible)

    candidates
      // TODO This should be done in AGS proper
      .filterNot: x =>
        scienceTargets.contains(x.target.tracking.baseCoordinates)
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

  private def baseAndScience(
    p: Props,
    surveyEpoch: Epoch
  ): (Coordinates, List[(Boolean, NonEmptyString, Option[Coordinates], Coordinates)]) = {
    val baseObsTimeCoords =
      p.asterismTracking
        .at(p.obsTime)
        .getOrElse(p.asterismTracking.baseCoordinates)

    val science = p.asterism.toSidereal
      .map: t =>
        val (surveyCoords, obsTimeCoords) =
          t.target.tracking.trackedPositions(p.obsTime, surveyEpoch.some)

        (t.id === p.asterism.focus.id, t.target.name, surveyCoords, obsTimeCoords)

    (baseObsTimeCoords, science)
  }

  private val CutOff = Wavelength.fromIntMicrometers(1).get

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
        baseCoords <- useState(baseAndScience(props, initialSurvey.epoch))
        // View coordinates base coordinates with pm correction + user panning
        currentPos <-
          useState(baseCoords.value._1.offsetBy(Angle.Angle0, props.options.viewOffset))
        // Survey
        survey     <- useState(initialSurvey)
        // Update coordinates if asterism or obsTime or survey changes
        _          <- useEffectWithDeps((props.asterism, props.obsTime, survey)): (_, _, s) =>
                        val (base, science) = baseAndScience(props, s.value.epoch)
                        baseCoords.setState((base, science)) *>
                          currentPos.setState:
                            base.offsetBy(Angle.Angle0, props.options.viewOffset)
        // Ref to the aladin component
        aladinRef  <- useState(none[Aladin])
        // If view offset changes upstream to zero, redraw
        _          <-
          useEffectWithDeps((baseCoords, props.options.viewOffset)): (_, offset) =>
            val newCoords = baseCoords.value._1.offsetBy(Angle.Angle0, offset)
            newCoords
              .map: coords =>
                aladinRef.value
                  .traverse(_.gotoRaDecCB(coords))
                  .void
                  .when_(offset === Offset.Zero)
              .getOrEmpty
        // Memoized svg for visualization shapes
        shapes     <-
          useMemo(
            (baseCoords, props.vizConf, props.globalPreferences.agsOverlay, props.selectedGuideStar)
          ) { _ =>
            val candidatesVisibilityCss =
              ExploreStyles.GuideStarCandidateVisible.when_(props.globalPreferences.agsOverlay)

            props.vizConf.map(_.configuration.obsModeType).map {
              case ObservingModeType.Flamingos2LongSlit                                      =>
                (Css.Empty,
                 Flamingos2Geometry.f2Geometry(
                   baseCoords.value._1,
                   props.vizConf.flatMap(_.scienceOffsets),
                   props.vizConf.flatMap(_.acquisitionOffsets),
                   props.vizConf.map(_.posAngle),
                   props.vizConf.map(_.configuration),
                   PortDisposition.Side,
                   props.selectedGuideStar,
                   candidatesVisibilityCss
                 )
                )
              case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
                (Css.Empty,
                 GmosGeometry.gmosGeometry(
                   baseCoords.value._1,
                   props.vizConf.flatMap(_.scienceOffsets),
                   props.vizConf.flatMap(_.acquisitionOffsets),
                   props.vizConf.map(_.posAngle),
                   props.vizConf.map(_.configuration),
                   PortDisposition.Side,
                   props.selectedGuideStar,
                   candidatesVisibilityCss
                 )
                )
              case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging   =>
                (VisualizationStyles.GmosCcdVisible,
                 GmosGeometry.gmosGeometry(
                   baseCoords.value._1,
                   props.vizConf.flatMap(_.scienceOffsets),
                   props.vizConf.flatMap(_.acquisitionOffsets),
                   props.vizConf.map(_.posAngle),
                   props.vizConf.map(_.configuration),
                   PortDisposition.Side,
                   props.selectedGuideStar,
                   candidatesVisibilityCss
                 )
                )
            }
          }
        // resize detector
        resize     <- useResizeDetector
        // memoized catalog targets with their proper motions corrected
        candidates <- useMemo(
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
                              scienceTargets,
                              survey.value.epoch
                            )

        // Use fov from aladin
        fov    <- useState(none[Fov])
        // Update survey if conf changes
        _      <-
          useEffectWithDeps(props.vizConf.flatMap(_.centralWavelength.map(_.value))): w =>
            w.map(w => survey.setState(surveyForWavelength(w))).getOrEmpty
      } yield {
        val (baseCoordinates, scienceTargets) = baseCoords.value

        /**
         * Called when the position changes, i.e. aladin pans. We want to offset the visualization
         * to keep the internal target correct
         */
        def onPositionChanged(u: PositionChanged): Callback = {
          val viewCoords = Coordinates(u.ra, u.dec)
          val viewOffset = baseCoordinates.diff(viewCoords).offset
          currentPos.setState(Some(viewCoords)) *>
            props.updateViewOffset(viewOffset)
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
            .map(Coordinates.fromHmsDms.reverseGet)
            .getOrElse(Coordinates.fromHmsDms.reverseGet(baseCoordinates))

        val baseSurveyEpochCoords =
          props.asterismTracking.at(survey.value.epoch.toInstant)

        val basePosition =
          List(
            SVGTarget.CrosshairTarget(baseCoordinates, Css.Empty, 10),
            SVGTarget.CircleTarget(baseCoordinates, ExploreStyles.BaseTarget, 3)
          ) ++ baseSurveyEpochCoords.map: surveyCoords =>
            SVGTarget.LineTo(
              surveyCoords,
              baseCoordinates,
              ExploreStyles.PMCorrectionLine
            )

        val sciencePositions =
          if (scienceTargets.length > 1)
            scienceTargets.flatMap { (selected, name, surveyCoords, obsTimeCoords) =>
              svgTargetAndLine(
                obsTimeCoords,
                surveyCoords,
                coords =>
                  SVGTarget.ScienceTarget(
                    coords,
                    ExploreStyles.ScienceTarget,
                    ExploreStyles.ScienceSelectedTarget,
                    3,
                    selected,
                    name.value.some
                  ),
                ExploreStyles.PMCorrectionLine
              )
            }
          else Nil

        def offsetIndicators(
          f:       ConfigurationForVisualization => Option[NonEmptyList[Offset]],
          oType:   SequenceType,
          css:     Css,
          visible: Visible
        ) =
          props.vizConf.foldMap(f).foldMap(_.toList).zipWithIndex.map { case (o, i) =>
            for {
              idx       <- refineV[NonNegative](i).toOption
              gs        <- props.selectedGuideStar
              baseCoords = if (oType === SequenceType.Acquisition) {
                             props.blindOffset
                               .flatMap(_.at(props.obsTime))
                               .getOrElse(baseCoordinates)
                           } else {
                             baseCoordinates
                           }
              c         <- baseCoords.offsetBy(gs.posAngle, o) if visible
            } yield SVGTarget.OffsetIndicator(c, idx, o, oType, css, 4)
          }

        val scienceOffsetIndicators =
          offsetIndicators(
            _.scienceOffsets,
            SequenceType.Science,
            ExploreStyles.ScienceOffsetPosition,
            props.globalPreferences.scienceOffsets
          )

        val acquisitionOffsetIndicators =
          offsetIndicators(
            _.acquisitionOffsets,
            SequenceType.Acquisition,
            ExploreStyles.AcquisitionOffsetPosition,
            props.globalPreferences.acquisitionOffsets
          )

        val blindOffsetIndicator: List[SVGTarget] =
          props.blindOffset
            .foldMap: bo =>
              val (epochCoords, obsTimeCoords) =
                bo.trackedPositions(props.siderealDiscretizedObsTime.obsTime, Some(bo.epoch))

              svgTargetAndLine(
                obsTimeCoords,
                epochCoords,
                coords => SVGTarget.BlindOffsetTarget(coords, ExploreStyles.BlindOffsetTarget, 6),
                ExploreStyles.BlindOffsetLine
              )

        val offsetTargets =
          // order is important, scienc to be drawn above acq
          (acquisitionOffsetIndicators |+| scienceOffsetIndicators).flattenOption

        val screenOffset =
          currentPos.value.map(_.diff(baseCoordinates).offset).getOrElse(Offset.Zero)

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
              (resize.width, resize.height, fov.value)
                .mapN(
                  TargetsOverlay(
                    _,
                    _,
                    _,
                    screenOffset,
                    baseCoordinates,
                    // Order matters
                    candidates ++ basePosition ++ blindOffsetIndicator ++ sciencePositions ++ offsetTargets
                  )
                ),
              (resize.width,
               resize.height,
               fov.value,
               shapes.value.flatMap(_._2.flatMap(NonEmptyMap.fromMap)),
               shapes.value.map(_._1)
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
