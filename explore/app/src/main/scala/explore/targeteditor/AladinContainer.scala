// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinMouseScroll
import explore.model.AppContext
import explore.model.AsterismVisualOptions
import explore.model.ConfigurationForVisualization
import explore.model.GlobalPreferences
import explore.model.InteractiveRegion
import explore.model.ObservationTargets
import explore.model.ObservationTargetsCoordinatesAt
import explore.model.RegionOrTrackingMap
import explore.model.RegionOrTrackingMap.*
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.given
import fs2.Stream
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.TargetDisposition
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.offsets.GeometryType
import lucuma.core.geom.offsets.OffsetPositions
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.EphemerisCoordinates
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.util.Timestamp
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetWithId
import lucuma.ui.aladin.*
import lucuma.ui.reusability
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.visualization.*
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant
import scala.collection.MapView
import scala.collection.immutable.SortedMap
import scala.scalajs.LinkingInfo

case class AladinContainer(
  obsTargets:             ObservationTargets,
  obsTime:                Instant,
  obsDuration:            Option[Duration],
  obsTimeTracking:        RegionOrTrackingMap,
  obsTimeCoords:          ObservationTargetsCoordinatesAt,
  vizConf:                Option[ConfigurationForVisualization],
  globalPreferences:      GlobalPreferences,
  options:                AsterismVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  mouseCoords:            Option[SignallingRef[IO, Option[Coordinates]]],
  interactiveRegions:     List[InteractiveRegion],
  pendingSlots:           Set[SlotId],
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis.Usable],
  agsResults:             AgsCalculationResults,
  anglesToTest:           Option[NonEmptyList[Angle]],
  agsState:               Option[AgsState],
  isStaffOrAdmin:         Boolean
) extends ReactFnProps(AladinContainer.component):
  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, vizConf.flatMap(_.selectedPosAngleConstraint))

  val site = vizConf.map(_.configuration.siteFor).getOrElse(Site.GN)

  val agsVisibility = GlobalPreferences.agsVisibility.get(globalPreferences)

  val guideStarCandidates: List[AgsAnalysis.Usable] =
    agsResults.constrained.toOption.orEmpty

  val guideStarsUnconstrained: List[AgsAnalysis.Usable] =
    val constrainedIds = guideStarCandidates.map(_.target.id).toSet
    // remove from unconstrained stars present on the constrained set
    agsResults.unconstrained.toOption.orEmpty
      .filterNot(gs => constrainedIds.contains(gs.target.id))

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // Relative sizes for targets passed to the svg layer, in terms of the side of
  // the svg projected onto the focal plane
  private val TargetSize                 = 6
  private val CrosshairSize              = 10
  private val OffsetIndicatorSize        = 4
  private val GuideStarSize              = 4
  private val GuideStarCandidateSize     = 3
  private val GuideStarCrowdedSize       = 2.7
  private val CrowdedCandidatesThreshold = 500

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

  // ShapeExpression has no Eq; key region reuse on (slot, posAngle, exclusionOffsets) like AgsAnalysis.Usable.
  private given Reusability[InteractiveRegion] =
    Reusability.by(r => (r.slot, r.posAngle, r.exclusionOffsets))

  private def speedCss(gs: GuideSpeed): Css =
    gs match
      case GuideSpeed.Fast   => ExploreStyles.GuideSpeedFast
      case GuideSpeed.Medium => ExploreStyles.GuideSpeedMedium
      case GuideSpeed.Slow   => ExploreStyles.GuideSpeedSlow

  private def svgTargetAndProperMotionLine(
    obsTimeCoords: Coordinates,
    surveyCoords:  Option[Coordinates],
    targetSVG:     Coordinates => SvgTarget,
    lineStyle:     Css
  ): List[SvgTarget] =
    targetSVG(obsTimeCoords) ::
      surveyCoords
        .map: source =>
          SvgTarget.LineTo(source, obsTimeCoords, lineStyle)
        .toList

  private def svgTargetAndEphemerisTrack(
    obsTime:       Instant,
    obsDuration:   Option[Duration],
    obsTimeCoords: Coordinates,
    ephemeris:     EphemerisTracking,
    targetSVG:     Coordinates => SvgTarget
  ): List[SvgTarget] =
    val endTime: Option[Timestamp]                            =
      obsDuration.map(obsTime.plus).flatMap(Timestamp.fromInstantTruncated)
    val endCoords: Option[EphemerisCoordinates]               = endTime.flatMap(ephemeris.get)
    // make sure the obstime and end time are in the ephemeris to properly visualize the track during the observation duration
    val coordsMap: SortedMap[Timestamp, EphemerisCoordinates] =
      (Timestamp.fromInstantTruncated(obsTime), endTime, endCoords)
        .mapN: (ot, et, ec) =>
          ephemeris.toMap
            .updated(ot,
                     EphemerisCoordinates(obsTimeCoords, Offset.Zero) // offset doesn't matter
            )
            .updated(et, ec)
        .getOrElse(ephemeris.toMap)
    targetSVG(obsTimeCoords) ::
      coordsMap.toList.sliding2
        .map { case ((t1, ec1), (t2, ec2)) =>
          val style =
            if endTime.exists(et => obsTime <= t1.toInstant && t2.toInstant <= et.toInstant) then
              ExploreStyles.EphemerisTrackInObservation
            else ExploreStyles.EphemerisTrack
          SvgTarget.LineTo(ec1.coord, ec2.coord, style)
        }

  private def candidateSVG(
    g:                          AgsAnalysis.Usable,
    isCrowded:                  Boolean,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    calcSize:                   Double => Double,
    surveyEpoch:                Epoch,
    targetBuilder:              (Coordinates, Double) => SvgTarget,
    lineCss:                    Css
  ): List[SvgTarget] =
    val tracking                         = g.target.tracking
    val obsTimeCoords: Coordinates       = tracking.atOrBase(siderealDiscretizedObsTime.obsTime)
    val epochCoords: Option[Coordinates] = tracking.coordsAtEpoch(surveyEpoch)

    val size =
      if (isCrowded) calcSize(GuideStarCrowdedSize)
      else calcSize(GuideStarCandidateSize)

    if (isCrowded)
      List(targetBuilder(obsTimeCoords, size))
    else
      svgTargetAndProperMotionLine(obsTimeCoords, epochCoords, targetBuilder(_, size), lineCss)

  private def guideStarsSVG(
    g:                          AgsAnalysis.Usable,
    isCrowded:                  Boolean,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    configuration:              Option[BasicConfiguration],
    selectedGS:                 Option[AgsAnalysis.Usable],
    candidatesVisibility:       Css,
    calcSize:                   Double => Double,
    surveyEpoch:                Epoch
  ): List[SvgTarget] =
    val tracking     = g.target.tracking
    val candidateCss = if (configuration.isEmpty) Css.Empty else speedCss(g.guideSpeed)

    if (selectedGS.forall(_.target.id === g.target.id))
      val obsTimeCoords = tracking.atOrBase(siderealDiscretizedObsTime.obsTime)
      val epochCoords   = tracking.coordsAtEpoch(surveyEpoch)
      val vignettedCss  = ExploreStyles.VignettedGS.when_(g.vignetting.toMicroarcsecondsSquared > 0L)
      svgTargetAndProperMotionLine(
        obsTimeCoords,
        epochCoords,
        SvgTarget.GuideStarTarget(_, candidateCss |+| vignettedCss, calcSize(GuideStarSize), g),
        ExploreStyles.PMGSCorrectionLine |+| VisualizationStyles.GuideStarCandidateVisible
      )
    else
      val css = candidateCss |+| candidatesVisibility |+|
        ExploreStyles.GuideStarCandidateCrowded.when_(isCrowded)
      candidateSVG(
        g,
        isCrowded,
        siderealDiscretizedObsTime,
        calcSize,
        surveyEpoch,
        (coords, size) => SvgTarget.GuideStarCandidateTarget(coords, css, size, g),
        ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
      )

  private def guideStars(
    candidates:                 List[AgsAnalysis.Usable],
    visible:                    Boolean,
    fovRA:                      Angle,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    configuration:              Option[BasicConfiguration],
    selectedGS:                 Option[AgsAnalysis.Usable],
    surveyEpoch:                Epoch
  ): List[SvgTarget] =
    val fov                            = fovRA.toMicroarcseconds / 1e6
    def calcSize(size: Double): Double = size.max(size * (225 / fov))
    val candidatesVisibility           = ExploreStyles.GuideStarCandidateVisible.when_(visible)
    val isCrowded                      = candidates.length >= CrowdedCandidatesThreshold

    candidates.flatMap:
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

  private def unconstrainedGuideStars(
    candidates:                 List[AgsAnalysis.Usable],
    visible:                    Boolean,
    fovRA:                      Angle,
    siderealDiscretizedObsTime: SiderealDiscretizedObsTime,
    surveyEpoch:                Epoch
  ): List[SvgTarget] =
    val fov                            = fovRA.toMicroarcseconds / 1e6
    def calcSize(size: Double): Double = size.max(size * (225 / fov))
    val candidatesVisibility           = ExploreStyles.GuideStarUnconstrained.when_(visible)
    val isCrowded                      = candidates.length >= CrowdedCandidatesThreshold

    candidates.flatMap: g =>
      val css = candidatesVisibility |+| speedCss(g.guideSpeed)
      candidateSVG(
        g,
        isCrowded,
        siderealDiscretizedObsTime,
        calcSize,
        surveyEpoch,
        (coords, size) => SvgTarget.GuideStarCandidateTarget(coords, css, size, g),
        ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
      )

  private case class TargetCoords(
    target:            TargetWithId,
    isSelected:        Boolean,
    obsTimeCoords:     Coordinates,
    surveyOrEphemeris: Either[Option[Coordinates], EphemerisTracking]
  ):
    val targetName: String = target.target.name.value

  private def targetCoordinates(
    obsTargets:  ObservationTargets,
    trackingMap: RegionOrTrackingMap,
    obsCoords:   ObservationTargetsCoordinatesAt,
    surveyEpoch: Epoch
  ): List[TargetCoords] =
    obsTargets
      .map: t =>
        obsCoords
          .forTarget(t.id)
          .map: coords =>
            val surveyOrEphemeris: Either[Option[Coordinates], EphemerisTracking] = trackingMap
              .trackingFor(t.id)
              // we should have tracking for all targets here
              .toOption
              .fold(none.asLeft)(tracking =>
                tracking match
                  case et: EphemerisTracking => et.asRight
                  case s: SiderealTracking   =>
                    s.coordsAtEpoch(surveyEpoch).asLeft
                  case _                     => none.asLeft // shouldn't be anything else
              )

            TargetCoords(t, t.id === obsTargets.focus.id, coords, surveyOrEphemeris)
      .toList
      .flattenOption

  private val CutOff = Wavelength.fromIntMicrometers(1).get

  private def surveyForWavelength(w: Wavelength) =
    if w > CutOff then ImageSurvey.TWOMASS else ImageSurvey.DSS

  private def positionFromBaseAndOffset(
    base:   Option[Coordinates],
    offset: Offset
  ): Option[Coordinates] =
    base.flatMap(_.offsetBy(Angle.Angle0, offset))

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx                     <- useContext(AppContext.ctx)
        hoveredSlot             <- useStateView(none[SlotId])
        // Hold the last click position on a SignallingRef instead of react state
        clickSignal             <- useEffectResultOnMount(SignallingRef.of[IO, Option[Coordinates]](none))
        currentPos              <-
          useState[Option[Coordinates]](
            positionFromBaseAndOffset(props.obsTimeCoords.baseOrBlindCoords,
                                      props.options.viewOffset
            )
          )
        survey                  <- useMemo(props.vizConf.map(_.conditionsWavelength)):
                                     _.map(surveyForWavelength).getOrElse(ImageSurvey.DSS)
        targetCoords            <-
          useMemo(
            (props.obsTargets, survey, props.obsTimeTracking, props.obsTimeCoords)
          ): (obsTargets, s, trackingMap, coords) =>
            targetCoordinates(obsTargets, trackingMap, coords, s.value.epoch)
        // Update coordinates if obsTargets or obsTime or survey changes
        // NOTE: Do not update the dependencies, or you might break sh@t. If this updates
        // too often, `Center on Target` will not work.
        _                       <- useEffectWithDeps((props.obsTargets, props.obsTime, survey)): (_, _, _) =>
                                     currentPos.setState(
                                       positionFromBaseAndOffset(props.obsTimeCoords.baseOrBlindCoords,
                                                                 props.options.viewOffset
                                       )
                                     )
        aladinRef               <- useState(none[Aladin])
        // If view offset changes upstream to zero, redraw
        _                       <-
          useEffectWithDeps((props.obsTimeCoords.baseOrBlindCoords, props.options.viewOffset)):
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
        shapes                  <- useVisualizationShapes(
                                     props.vizConf,
                                     props.obsTimeCoords.baseOrBlindCoords,
                                     props.obsTimeCoords.blindOffsetCoords,
                                     props.obsTimeCoords.slotCoords,
                                     props.obsTimeCoords.slotForTarget(props.obsTargets.focus.id),
                                     props.globalPreferences.agsOverlay,
                                     props.selectedGuideStar
                                   )
        // Track which interactive region (if any) the mouse is over, off the React render path.
        _                       <- useEffectStreamResourceWithDeps(
                                     (props.obsTimeCoords.baseOrBlindCoords, props.interactiveRegions)
                                   ): (baseCoords, regions) =>
                                     import ctx.given
                                     (baseCoords, props.mouseCoords).tupled.fold(
                                       Resource.pure(Stream.eval(hoveredSlot.set(none).to[IO]))
                                     ): (base, signal) =>
                                       val evaluated = regions.map(r => r.slot -> r.shape.eval)
                                       Resource.pure(
                                         signal.discrete
                                           .map: mouseOpt =>
                                             mouseOpt.flatMap: m =>
                                               Option
                                                 .unless(m === base)(())
                                                 .flatMap: _ =>
                                                   evaluated.collectFirst:
                                                     case (slot, s) if s.contains(base.diff(m).offset) =>
                                                       slot
                                           .changes
                                           .evalMap(slot => hoveredSlot.set(slot).to[IO])
                                       )
        // When clicking inside an interactive region, run its onClick (e.g. assign a sky position).
        _                       <- useEffectStreamResourceWithDeps(
                                     (props.obsTimeCoords.baseOrBlindCoords,
                                      props.interactiveRegions,
                                      clickSignal.value.value.toOption.isDefined
                                     )
                                   ): (baseCoords, regions, _) =>
                                     (baseCoords, clickSignal.value.value.toOption).tupled
                                       .fold(
                                         Resource.pure(Stream.empty.covary[IO])
                                       ): (base, signal) =>
                                         val evaluated = regions.map(r => r.shape.eval -> r.onClick)
                                         Resource
                                           .eval(Ref.of[IO, Boolean](false))
                                           .map: submitting =>
                                             signal.discrete.unNone.evalMap: c =>
                                               signal.set(none) *>
                                                 evaluated
                                                   .collectFirst:
                                                     case (s, onClick) if c =!= base && s.contains(base.diff(c).offset) =>
                                                       onClick(c)
                                                   .map: act =>
                                                     submitting
                                                       .getAndSet(true)
                                                       .flatMap {
                                                         case true  => IO.unit // submit in flight; ignore
                                                         case false =>
                                                           act.attempt *> submitting.set(false)
                                                       }
                                                   .getOrElse(IO.unit)
        // patrol field shapes for debugging
        pfShapes                <- usePatrolFieldShapes(
                                     props.vizConf,
                                     props.selectedGuideStar,
                                     props.obsTimeCoords.baseOrBlindCoords,
                                     props.obsTimeCoords.blindOffsetCoords,
                                     props.obsTimeCoords.scienceOffsetsFromBase,
                                     props.agsVisibility,
                                     props.anglesToTest,
                                     props.agsState
                                   )
        offsetPositions         <- useMemo(
                                     (props.vizConf,
                                      props.selectedGuideStar,
                                      props.obsTimeCoords.baseOrBlindCoords,
                                      props.obsTimeCoords.blindOffsetCoords
                                     )
                                   ): (vizConf, selectedGS, baseCoords, blindOffset) =>
                                     baseCoords.map: baseCoordinates =>
                                       val posAngle: Angle = selectedGS
                                         .map(_.posAngle)
                                         .orElse(vizConf.map(_.posAngle))
                                         .getOrElse(Angle.Angle0)

                                       OffsetPositions.fromTelescopeConfigs(
                                         baseCoordinates.some,
                                         blindOffset,
                                         posAngle,
                                         vizConf.flatMap(_.acquisitionOffsets),
                                         vizConf.flatMap(_.scienceOffsets)
                                       )
        // resize detector
        resize                  <- useResizeDetector
        // memoized catalog targets with their proper motions corrected
        candidates              <- useMemo(
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
        // memoized unconstrained guide star candidates
        unconstrainedCandidates <- useMemo(
                                     (props.guideStarsUnconstrained,
                                      props.globalPreferences.showCatalog,
                                      props.options.fovRA,
                                      props.siderealDiscretizedObsTime,
                                      survey
                                     )
                                   ):
                                     (
                                       candidates,
                                       visible,
                                       fovRA,
                                       siderealDiscretizedObsTime,
                                       survey
                                     ) =>
                                       unconstrainedGuideStars(
                                         candidates,
                                         visible,
                                         fovRA,
                                         siderealDiscretizedObsTime,
                                         survey.value.epoch
                                       )
        // Use fov from aladin
        fov                     <- useState(none[Fov])
      } yield {
        val baseCoordinates: Option[Coordinates] = props.obsTimeCoords.baseOrBlindCoords

        // Shade the sky keep-out zone while adding a sky position, or in regular mode when an
        // existing sky position falls inside it, or when two science targets are themselves
        // too close..
        val keepOutZone = InteractiveRegion.skyKeepOutZone(props.vizConf, props.obsTimeCoords)

        val skyInKeepOut: Boolean =
          props.obsTimeCoords.scienceCoords.exists(sci =>
            props.obsTimeCoords.skyCoords.exists(sky => GhostGeometry.tooClose(sci, sky))
          )

        val scienceTargetsTooClose: Boolean =
          InteractiveRegion.scienceTargetsTooClose(props.obsTimeCoords)

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

        def onZoom: Fov => Callback =
          (v: Fov) => {
            // Sometimes get 0 fov, ignore those
            val ignore =
              (v.x === Angle.Angle0 && v.y === Angle.Angle0) ||
                fov.value.exists(_.isDifferentEnough(v))
            (fov.setState(v.some) *> props.updateFov(v)).unless_(ignore)
          }

        // Record the last non-drag click
        val onAladinClick: AladinClick => Callback = c =>
          import ctx.given
          clickSignal.value.value.toOption
            .foldMap(_.set(c.coordinates.some).runAsync)
            .unless_(c.isDragging)

        val includeSvg: Aladin => Callback = (v: Aladin) =>
          aladinRef.setState(v.some) *>
            v.onZoomCB(onZoom) *> // re render on zoom
            v.onPositionChangedCB(onPositionChanged) *>
            v.onMouseMoveCB(s => props.updateMouseCoordinates(Coordinates(s.ra, s.dec))) *>
            v.onClickCB(onAladinClick)

        val baseCoordinatesForAladin: String =
          currentPos.value
            .foldMap(Coordinates.fromHmsDms.reverseGet)

        def basePosition(css: Css, title: Option[String] = None) =
          baseCoordinates.foldMap: c =>
            List(SvgTarget.CrosshairTarget(c, css, CrosshairSize, title))

        val isSelectable: Boolean = props.obsTargets.length > 1

        // Only label the crosshair as the base position for an asterism; for a
        // single target it coincides with the target itself.
        val basePositionTitle: Option[String] =
          Option.when(props.obsTargets.science.length > 1)("Base position")

        val targetLabels: Map[Target.Id, String] =
          props.vizConf.foldMap(_.targetVisualization.labels)

        val scienceTargets: List[SvgTarget] =
          targetCoords
            .filterNot(_.target.disposition === TargetDisposition.BlindOffset)
            .flatMap { tc =>
              // Some modes like GHOST want to add a label to the targets
              val title = targetLabels
                .get(tc.target.id)
                .fold(tc.targetName)(l => s"$l: ${tc.targetName}")

              def targetSvg(coords: Coordinates) =
                SvgTarget.ScienceTarget(
                  coords,
                  ExploreStyles.ScienceTarget,
                  ExploreStyles.ScienceSelectedTarget,
                  TargetSize,
                  tc.isSelected && isSelectable,
                  title.some
                )

              tc.surveyOrEphemeris
                .fold(
                  oc =>
                    svgTargetAndProperMotionLine(
                      tc.obsTimeCoords,
                      oc,
                      targetSvg,
                      lineStyle = ExploreStyles.PMCorrectionLine
                    ),
                  et =>
                    svgTargetAndEphemerisTrack(
                      props.obsTime,
                      props.obsDuration,
                      tc.obsTimeCoords,
                      et,
                      targetSvg
                    )
                )
            }

        def offsetStyle(geometryType: GeometryType): Css =
          geometryType match
            case GeometryType.AcqGuidedOffset   => ExploreStyles.AcquisitionOffsetPosition
            case GeometryType.AcqUnguidedOffset => ExploreStyles.AcquisitionOffsetPosition
            case GeometryType.SciGuidedOffset   => ExploreStyles.ScienceOffsetPosition
            case _                              => ExploreStyles.ScienceUnguidedOffsetPosition

        // Offset indicators calculated and rotated directly by AGS
        val configOffsets: List[SvgTarget.OffsetIndicator] =
          offsetPositions.value
            .map(_.value.toList)
            .map { positions =>
              val offsetIndicators: MapView[GeometryType, List[SvgTarget.OffsetIndicator]] =
                positions
                  .groupBy(_.geometryType)
                  .view
                  .mapValues:
                    _.toList.zipWithIndex.flatMap: (pos, i) =>
                      for
                        idx <- refineV[NonNegative](i).toOption
                        c   <- positionFromBaseAndOffset(baseCoordinates, pos.rotatedOffset.value)
                      yield SvgTarget.OffsetIndicator(
                        c,
                        idx,
                        pos.offsetPos,
                        if Set(GeometryType.AcqGuidedOffset, GeometryType.AcqUnguidedOffset)
                            .contains_(pos.geometryType)
                        then SequenceType.Acquisition
                        else SequenceType.Science,
                        offsetStyle(pos.geometryType),
                        OffsetIndicatorSize
                      )
              // order is important, science to be drawn above acq, guided over unguided
              (offsetIndicators.get(GeometryType.AcqUnguidedOffset).orEmpty ++
                offsetIndicators.get(GeometryType.AcqGuidedOffset).orEmpty)
                .filter(_ => props.globalPreferences.acquisitionOffsets.value) ++
                (offsetIndicators.get(GeometryType.SciUnguidedOffset).orEmpty ++
                  offsetIndicators.get(GeometryType.SciGuidedOffset).orEmpty)
                  .filter(_ => props.globalPreferences.scienceOffsets.value)
            }
            .orEmpty

        val blindOffsets: List[SvgTarget] =
          targetCoords
            .filter(tc => tc.target.disposition === TargetDisposition.BlindOffset)
            .flatMap: tc =>
              def targetSvg(coords: Coordinates) = SvgTarget.BlindOffsetTarget(
                coords,
                Css.Empty,
                ExploreStyles.BlindOffsetSelectedTarget,
                TargetSize,
                tc.isSelected && isSelectable,
                tc.targetName.some
              )
              // blind offsets are always sidereal
              tc.surveyOrEphemeris
                .fold(oc =>
                        svgTargetAndProperMotionLine(
                          tc.obsTimeCoords,
                          oc,
                          targetSvg,
                          ExploreStyles.BlindOffsetLine
                        ),
                      _ => List.empty
                )

        val skyPositionTargets: List[SvgTarget] =
          props.obsTimeCoords.skySlots.map: (slot, c) =>
            val raStr  = MathValidators.truncatedRA.reverseGet(c.ra)
            val decStr = MathValidators.truncatedDec.reverseGet(c.dec)
            // Dim/dash the marker while its assignment is still optimistic (mutation in flight).
            val css    = ExploreStyles.SkyPositionPending.when_(props.pendingSlots.contains(slot))
            SvgTarget
              .SkyPositionTarget(c, css, TargetSize, s"${slot.shortName} sky: $raStr $decStr".some)

        // Use explicit reusability that excludes target changes
        given Reusability[AladinOptions] = reusability.withoutTarget

        <.div.withRef(resize.ref)(
          ExploreStyles.AladinContainerBody |+|
            ExploreStyles.AddSkyModeInvalidCursor.when_(props.interactiveRegions.nonEmpty) |+|
            ExploreStyles.AddSkyModeCursor.when_(hoveredSlot.get.isDefined)
        )(
          // This is a bit tricky. Sometimes the height can be 0 or a very low number.
          // This happens during a second render. If we let the height to be zero, aladin
          // will take it as 1. This height ends up being a denominator, which, if low,
          // will make aladin request a large amount of tiles and end up freeze explore.
          if (resize.height.exists(_ >= 100)) {
            val screenOffset =
              (currentPos.value, baseCoordinates).mapN(_.diff(_).offset).getOrElse(Offset.Zero)

            val staleCss = ExploreStyles.VisualizationStale.when_(
              props.agsState.exists(_ === AgsState.Calculating)
            )

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
                    candidates ++ blindOffsets ++ scienceTargets ++ skyPositionTargets ++
                      basePosition(Css.Empty, basePositionTitle) ++ configOffsets
                  )
                ),
              // Separate overlay for unconstrained guide star candidates (available at other PAs)
              Option.when(unconstrainedCandidates.nonEmpty)(
                (resize.width, resize.height, fov.value, baseCoordinates)
                  .mapN(
                    TargetsOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      _,
                      basePosition(ExploreStyles.Hidden) ++ unconstrainedCandidates
                    )
                  )
              ),
              // Instrument shapes
              (resize.width,
               resize.height,
               fov.value,
               shapes.flatMap(_._2.flatMap(NonEmptyMap.fromMap)),
               shapes.map(_._1 |+| staleCss)
              )
                .mapN(
                  SvgVisualizationOverlay(
                    _,
                    _,
                    _,
                    screenOffset,
                    _,
                    _
                  )
                ),
              // Sky keep-out zone
              keepOutZone
                .filter(_ =>
                  props.interactiveRegions.nonEmpty || skyInKeepOut || scienceTargetsTooClose
                )
                .flatMap: shapes =>
                  (resize.width, resize.height, fov.value)
                    .mapN(
                      SvgVisualizationOverlay(
                        _,
                        _,
                        _,
                        screenOffset,
                        shapes,
                        Css.Empty
                      )
                    ),
              // Interactive regions like ifu2 for ghost
              hoveredSlot.get
                .flatMap(slot => props.interactiveRegions.find(_.slot === slot))
                .flatMap: region =>
                  (resize.width, resize.height, fov.value)
                    .mapN(
                      SvgVisualizationOverlay(
                        _,
                        _,
                        _,
                        screenOffset,
                        NonEmptyMap.of(region.shapeCss -> region.shape),
                        region.hoverCss
                      )
                    ),
              // Patrol field shapes for debugging
              Option.when(LinkingInfo.developmentMode || props.isStaffOrAdmin)(
                (resize.width,
                 resize.height,
                 fov.value,
                 pfShapes.flatMap(m => NonEmptyMap.fromMap(m))
                )
                  .mapN(
                    SvgVisualizationOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      _,
                      staleCss
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
