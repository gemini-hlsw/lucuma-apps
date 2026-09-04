// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.Pot.Ready
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationTile
import explore.config.MosMaskContext
import explore.config.sequence.SequenceTile
import explore.findercharts.FinderChartsTile
import explore.itc.ItcEmptyTile
import explore.itc.ItcImagingTile
import explore.itc.ItcSpectroscopyTile
import explore.model.*
import explore.model.GuideStarSelection.*
import explore.model.enums.AgsState
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.itc.ItcTarget
import explore.model.layout.*
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ScienceModes
import explore.observationtree.obsEditAttachments
import explore.plots.ElevationPlotTile
import explore.plots.ObjectPlotData
import explore.plots.PlotData
import explore.schedulingWindows.*
import explore.syntax.ui.*
import explore.targeteditor.ObservationTargetsEditorTile
import explore.targeteditor.UseAgs.useAgs
import explore.targeteditor.UseTrackingMap.useAsterismTracking
import explore.targeteditor.UseTrackingMap.useObsTargetsCoords
import explore.targeteditor.UseTrackingMap.useTrackingMap
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.conditions.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.skycalc.averageParallacticAngle
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostIfuMappingSyntax.*
import lucuma.core.model.sequence.ghost.IfuMappingContext
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.*
import lucuma.refined.*
import lucuma.schemas.model.AGSWavelength
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetVisualization
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.UndoSetter
import lucuma.ui.visualization.GhostGeometry
import monocle.Iso
import monocle.Optional
import queries.schemas.itc.syntax.itcTarget

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ObsTabTiles(
  vault:            Option[UserVault],
  programId:        Program.Id,
  programType:      ProgramType,
  modes:            ScienceModes,
  backButton:       VdomNode,
  observation:      UndoSetter[Observation],
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  attachments:      View[AttachmentList],
  programSummaries: ProgramSummaries,
  focusedTarget:    Option[Target.Id],
  searching:        View[Set[Target.Id]],
  selectedGSName:   View[Option[NonEmptyString]],
  resize:           UseResizeDetectorReturn,
  userPreferences:  View[UserPreferences],
  readonly:         Boolean
) extends ReactFnProps(ObsTabTiles.component):
  val isStaffOrAdminUser: Boolean = vault.isStaffOrAdmin
  val obsIsReadonly: Boolean      =
    readonly || (observation.get.isExecuted && !isStaffOrAdminUser) || observation.get.isCompleted
  val obsId: Observation.Id       = observation.get.id

  val allConstraintSets: Set[ConstraintSet] = programSummaries.constraintGroups.map(_._2).toSet

  val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    programSummaries.targetObservations

  val obsTargets: TargetList = programSummaries.obsTargets.get(obsId).getOrElse(SortedMap.empty)

  val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    programSummaries.obsAttachmentAssignments

  val scienceTargetsForTracking: Option[NonEmptyList[Target]] =
    observation.get.scienceTargetsForTracking(obsTargets)

  val posAngleConstraint: PosAngleConstraint = observation.get.posAngleConstraint

  val calibrationRole: Option[CalibrationRole] = observation.zoom(Observation.calibrationRole).get

  val constraintSet = observation.zoom(Observation.constraints)

  val centralWavelength: Option[CentralWavelength] =
    observation.get.basicConfiguration.flatMap(_.centralWv)

  val conditionsWavelength: Option[Wavelength] =
    observation.get.basicConfiguration.map(_.conditionsWavelength)

  val agsWavelength: Option[AGSWavelength] =
    observation.get.basicConfiguration.map(_.agsWavelength)

  private val asterismAsNel: Option[ObservationTargets] =
    ObservationTargets.fromTargets:
      obsTargets.toList.map((_, t) => t)

  val scienceTargets: List[TargetWithId] = asterismAsNel.map(_.science).orEmpty

  def targetCoords(obsTime: Instant, optTracking: Option[Tracking]): Option[Coordinates] =
    optTracking.flatMap(_.at(obsTime))

  def site: Option[Site] = observation.get.basicConfiguration.flatMap(_.siteFor)

  val basicConfiguration: Option[BasicConfiguration] = observation.get.basicConfiguration

  // The IFU mapping for ghost. TODO: Add support for explicit base
  def ghostIfuMapping(obsTimeOrNow: Instant): Option[GhostIfuMapping] =
    observation.get.observingMode.toOption.flatten match
      case Some(ghost: ObservingMode.GhostIfu) =>
        val ctx = IfuMappingContext(
          ghost.resolutionMode,
          ghost.skyPosition,
          posAngleConstraint,
          none,
          Timestamp.fromInstantTruncatedAndBounded(obsTimeOrNow)
        )

        // Whether `sky` is within the minimum IFU-arm separation of any science target.
        def tooCloseToScience(sky: Coordinates): Boolean =
          scienceTargets.exists: t =>
            t.target.asSidereal
              .flatMap(_.tracking.at(obsTimeOrNow))
              .exists(GhostGeometry.tooClose(_, sky))

        GhostIfuMapping.derive(ctx, scienceTargets.map(t => (t.id, t.target))) match
          case Right(mapping)                                         =>
            mapping.some
          // Derivation fails when the sky is too close to the science target.
          // Fall back to TargetPlusSky so the sky marker stays visible and
          // the keep-out zone can flag it
          case Left(_) if ghost.skyPosition.exists(tooCloseToScience) =>
            (scienceTargets.headOption.map(_.id), ghost.skyPosition)
              .mapN(GhostIfuMapping.TargetPlusSky.apply)
          case Left(_)                                                =>
            none
      case _                                   => none

  def targetVisualization(obsTimeOrNow: Instant): TargetVisualization =
    basicConfiguration
      .map(_.targetVisualization(scienceTargets, ghostIfuMapping(obsTimeOrNow)))
      .getOrElse(TargetVisualization.Empty)

  // The explicit duration if set, else the remaining time from the digest.
  def obsDuration: Option[TimeSpan] =
    observation.get.observationDuration
      .orElse(observation.get.execution.digest.remainingObsTime.value)

  // Average PA over the science part of the observation, i.e. after setup.
  def averagePA(obsTimeOrNow: Instant, optTracking: Option[Tracking]): Option[AveragePABasis] =
    (site, optTracking, obsDuration, observation.get.execution.digest.fullSetupTime.value)
      .flatMapN: (site, baseTracking, fullDuration, setupDuration) =>
        fullDuration
          .subtract(setupDuration)
          .filter(_ > TimeSpan.Zero)
          .flatMap: scienceDuration =>
            val scienceStartTime = obsTimeOrNow.plusNanos(setupDuration.toMicroseconds * 1000)
            posAngleConstraint match
              case PosAngleConstraint.AverageParallactic =>
                averageParallacticAngle(
                  site.place,
                  baseTracking,
                  scienceStartTime,
                  scienceDuration
                ).map(AveragePABasis(scienceStartTime, scienceDuration, _))
              case _                                     => none

  def acqConfigs: Option[NonEmptySet[TelescopeConfig]] =
    NonEmptySet.fromSet:
      Execution.acqConfigs
        .getOption(observation.get.execution)
        .orEmpty

  def sciConfigs: Option[NonEmptySet[TelescopeConfig]] =
    NonEmptySet.fromSet:
      Execution.sciConfigs
        .getOption(observation.get.execution)
        .orEmpty

  private def obsIQLikelihood(
    optCoordinates: Option[Coordinates]
  ): Option[IntCentiPercent] =
    (conditionsWavelength, optCoordinates.map(_.dec), site).mapN((cw, dec, site) =>
      site
        .minimumAirMassFor(dec)
        .fold(IntCentiPercent.Min): airMass =>
          constraintSet.get.imageQuality.toImageQuality.percentile(cw, airMass)
    )

  private def obsConditionsLikelihood(
    optCoordinates: Option[Coordinates]
  ): Option[IntCentiPercent] =
    (conditionsWavelength, optCoordinates.map(_.dec), site).mapN((cw, dec, site) =>
      conditionsLikelihood(
        constraintSet.get.skyBackground,
        constraintSet.get.cloudExtinction.toCloudExtinction,
        constraintSet.get.waterVapor,
        constraintSet.get.imageQuality.toImageQuality,
        cw,
        dec,
        site
      )
    )

object ObsTabTiles:
  private type Props = ObsTabTiles

  private val ghostSkyPositionLens: Optional[Observation, Option[Coordinates]] =
    Observation.observingModeOption.some
      .andThen(ObservingMode.ghostIfu)
      .andThen(ObservingMode.GhostIfu.skyPosition)

  def roleLayout(
    userPreferences: UserPreferences,
    calibrationRole: Option[CalibrationRole]
  ): (GridLayoutSection, LayoutsMap, LayoutsMap) =
    def result(section: GridLayoutSection) =
      (section,
       ExploreGridLayouts.sectionLayout(section),
       UserPreferences.gridLayouts
         .index(section)
         .getOption(userPreferences)
         .getOrElse(ExploreGridLayouts.sectionLayout(section))
      )

    calibrationRole match
      case Some(CalibrationRole.SpectroPhotometric) =>
        result(GridLayoutSection.ObservationsSpecPhotoLayout)
      case Some(CalibrationRole.Twilight)           =>
        result(GridLayoutSection.ObservationsTwilightLayout)
      case _                                        =>
        result(GridLayoutSection.ObservationsLayout)

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                  <- useContext(AppContext.ctx)
        agsState             <- useStateView[AgsState](AgsState.Idle)
        // the configuration the user has selected from the spectroscopy modes table, if any
        selectedConfig       <- useStateView(ConfigSelection.Empty)
        // selected target for imaging, shared between the itc tile and the modes table
        selectedItcTarget    <-
          useStateView[Option[ItcTarget]](
            props.obsTargets.values.flatMap(_.target.itcTarget.toOption).headOption
          )
        customSedTimestamps  <-
          // The updatedAt timestamps for any custom seds.
          useMemo((props.asterismAsNel, props.attachments.get)): (asterism, attachments) =>
            asterism.foldMap:
              _.map:
                _.target.sourceProfile.customSedId.flatMap(attachments.get).map(_.updatedAt)
              .toList.flattenOption
        sequenceChanged      <- useStateView(().ready) // Signal that the sequence has changed
        // if the timestamp for a custom sed attachment changes, it means either a new custom sed
        // has been assigned, OR a new version of the custom sed has been uploaded. This is to
        // catch the latter case.
        _                    <- useEffectWithDeps(customSedTimestamps): _ =>
                                  sequenceChanged.set(pending)
        obsTimeOrNowPot      <- useEffectKeepResultWithDeps(props.observation.model.get.observationTime):
                                  vizTime => IO(vizTime.getOrElse(Instant.now()))
        trackingMapPot       <-
          useTrackingMap(props.asterismAsNel, props.site, obsTimeOrNowPot.value.toOption)(ctx)
        // Store guide star selection in a view for fast local updates
        // This is not the ideal place for this but we need to share the selected guide star
        // across the configuration and target tile
        guideStarSelection   <- useStateView:
                                  props.selectedGSName.get.fold(GuideStarSelection.Default)(
                                    RemoteGSSelection.apply
                                  )
                                .map: gss =>
                                  import ctx.given

                                  // We tell the backend and the local cache of changes to the selected guidestar
                                  // In some cases when we do a real override
                                  gss.withOnMod {
                                    (_, _) match {
                                      // Change of override
                                      case (AgsOverride(m, _, _), AgsOverride(n, _, _)) if m =!= n =>
                                        props.selectedGSName.set(n.some) *>
                                          odbApi
                                            .setGuideTargetName(props.obsId, n.some)
                                            .runAsyncAndForget
                                      // Going from automatic to manual selection
                                      case (AgsSelection(_), AgsOverride(n, _, _))                 =>
                                        props.selectedGSName.set(n.some) *>
                                          odbApi
                                            .setGuideTargetName(props.obsId, n.some)
                                            .runAsyncAndForget
                                      // Going from manual to automated selection
                                      case (AgsOverride(n, _, _), AgsSelection(_))                 =>
                                        props.selectedGSName.set(none) *>
                                          odbApi
                                            .setGuideTargetName(props.obsId, none)
                                            .runAsyncAndForget
                                      case _                                                       =>
                                        // All other combinations
                                        Callback.empty
                                    }
                                  }
        // Some of the fields of an observation are expensive and we can
        // load them on demand. For now it is only the guideStar selection name
        _                    <- useEffectWithDeps(props.obsId): obsId =>
                                  import ctx.given
                                  odbApi
                                    .guideTargetName(obsId)
                                    .flatMap: name =>
                                      (props.selectedGSName.set(name) >>
                                        guideStarSelection.set(
                                          name.fold(GuideStarSelection.Default)(RemoteGSSelection.apply)
                                        )).toAsync
        // The mask design is only stored on the attachment, fetched on demand for MOS obs.
        maskDesignPot        <-
          useEffectKeepResultWithDeps((props.obsId, props.observation.get.maskAttachmentId)):
            (obsId, maskId) =>
              import ctx.given
              maskId.flatTraverse(odbApi.maskDesign(obsId, _))
        roleLayouts          <- useState(roleLayout(props.userPreferences.get, props.calibrationRole))
        _                    <- useEffectWithDeps(props.calibrationRole): role =>
                                  roleLayouts.setState(roleLayout(props.userPreferences.get, role))
        isEditingAcquisition <- useStateView(IsEditing.False)
        isEditingScience     <- useStateView(IsEditing.False)
        oBaseTracking        <- useAsterismTracking(props.asterismAsNel, trackingMapPot)
        averagePA             =
          obsTimeOrNowPot.value.toOption.flatMap(props.averagePA(_, oBaseTracking.value))
        trackType             = oBaseTracking.value.map(_.trackType)
        paProps               =
          PAProperties(props.obsId, guideStarSelection, agsState, props.posAngleConstraint)
        obsConf               =
          ObsConfiguration(
            props.basicConfiguration,
            selectedConfig.get,
            paProps.some,
            props.constraintSet.get.some,
            props.sciConfigs,
            props.acqConfigs,
            averagePA,
            props.obsDuration.map(_.toDuration),
            props.observation.get.needsAGS(props.obsTargets),
            props.observation.get.selectedGSName,
            props.observation.get.calibrationRole,
            trackType,
            obsTimeOrNowPot.value.toOption
              .map(props.targetVisualization)
              .getOrElse(TargetVisualization.Empty),
            props.observation.get.explicitBase,
            props.observation.get.cassRotator,
            maskDesignPot.value.toOption.flatten
          )
        focusedTargets        = props.asterismAsNel.map: targets =>
                                  props.focusedTarget.fold(targets)(targets.focusOn)
        obsCoords            <- useObsTargetsCoords(
                                  focusedTargets,
                                  obsTimeOrNowPot.value.toOption,
                                  trackingMapPot,
                                  obsConf.targetViz.some,
                                  obsConf.explicitBase
                                )
        // AGS follows the observation, not the target tile, so the guide star keeps up with time
        // and configuration changes while the tile is minimized.
        agsData              <- useAgs(
                                  focusedTargets,
                                  obsTimeOrNowPot.value.toOption,
                                  obsCoords,
                                  oBaseTracking,
                                  obsConf,
                                  guideStarSelection
                                )(ctx)
      yield
        import ctx.given

        val (section, defaultLayout, layout) = roleLayouts.value

        obsTimeOrNowPot.value.renderPot: obsTimeOrNow =>
          val globalPreferences = props.userPreferences.zoom(UserPreferences.globalPreferences)

          val asterismIds: View[SortedSet[Target.Id]] =
            props.observation.model.zoom(Observation.scienceTargetIds)

          // ETM normalized to science requirements so it matches the table rows on ===.
          val revertedInstrumentConfig: List[ItcInstrumentConfig] =
            val rowEtm: ExposureTimeMode =
              props.observation.get.scienceRequirements.exposureTimeMode
                .getOrElse(ItcInstrumentConfig.PlaceholderEtm)
            props.observation.get
              .toInstrumentConfig(props.obsTargets)
              .map(_.setSingleExposureTimeMode(rowEtm))

          val obsTimeView: View[Option[Instant]] =
            props.observation.model.zoom(Observation.observationTime)

          val obsDurationView: View[Option[TimeSpan]] =
            props.observation.model.zoom(Observation.observationDuration)

          val attachmentsView =
            props.observation.model.zoom(Observation.attachmentIds).withOnMod { ids =>
              obsEditAttachments(props.obsId, ids).runAsync
            }

          val digest = props.observation.get.execution.digest

          // For average and allow flip we need to read the flip from the selected star
          def flipIfNeeded(angle: Option[Angle]): Option[Angle] =
            if (paProps.selectedPA.exists(a => angle.forall(_ === a.flip))) paProps.selectedPA
            else angle

          // The angle used for `Align to PA` in the finder charts tile.
          // For Unbounded, use the PA of the currently selected guide star (if any)
          // For AverageParllactic constraint, use the selected guide star angle if flipped
          // or default to the calculated average PA (if any), otherwise use the angle specified
          // in the constraint
          val pa: Option[Angle] =
            props.posAngleConstraint match
              case PosAngleConstraint.Unbounded                  => paProps.selectedPA
              case PosAngleConstraint.AverageParallactic         => flipIfNeeded(averagePA.map(_.averagePA))
              case PosAngleConstraint.Fixed(angle)               => angle.some
              case PosAngleConstraint.AllowFlip(angle)           => flipIfNeeded(angle.some)
              case PosAngleConstraint.ParallacticOverride(angle) => angle.some

          // Science programs only clear this once their proposal is accepted
          // non-Science programs (engineering, calibration, etc.) are always considered past
          val pastProposalReview = props.programSummaries.proposalIsAccepted ||
            props.programSummaries.optProgramDetails.exists(_.programType =!= ProgramType.Science)

          // hide the finder charts and notes tiles for science programs if the proposal has not been accepted
          val hideTiles = !pastProposalReview

          val finderChartsTile =
            FinderChartsTile(
              props.programId,
              props.obsId,
              attachmentsView,
              props.vault.map(_.token),
              props.attachments,
              pa,
              props.readonly || props.observation.get.isCompleted,
              hidden = hideTiles
            )

          val notesView: View[Option[NonEmptyString]] =
            props.observation.model
              .zoom(Observation.observerNotes)
              .withOnMod: notes =>
                odbApi.updateNotes(List(props.obsId), notes).runAsync

          val notesTile = NotesTile(notesView, hidden = hideTiles)

          val sequenceTile =
            SequenceTile(
              props.obsId,
              props.observation.get.execution,
              asterismIds.get,
              customSedTimestamps,
              props.calibrationRole,
              sequenceChanged,
              isEditingAcquisition,
              isEditingScience,
              props.isStaffOrAdminUser,
              props.attachments.get
            )

          val odbOrSelectedConfig: Option[BasicConfiguration] =
            props.basicConfiguration.orElse(selectedConfig.get.toBasicConfiguration())

          val isVisitorMode: Boolean =
            props.basicConfiguration.exists(_.isInstanceOf[BasicConfiguration.Visitor])

          val itcTile =
            odbOrSelectedConfig match
              case Some(_: BasicConfiguration.GmosNorthImaging) |
                  Some(_: BasicConfiguration.GmosSouthImaging) |
                  Some(_: BasicConfiguration.Flamingos2Imaging) |
                  Some(_: BasicConfiguration.GnirsImaging) =>
                ItcImagingTile(
                  props.vault.userId,
                  selectedConfig.get,
                  props.observation.get,
                  props.obsTargets,
                  customSedTimestamps,
                  selectedItcTarget
                ).some
              case Some(_: BasicConfiguration.GmosNorthLongSlit) |
                  Some(_: BasicConfiguration.GmosSouthLongSlit) |
                  Some(_: BasicConfiguration.GmosNorthMos) |
                  Some(_: BasicConfiguration.GmosSouthMos) |
                  Some(_: BasicConfiguration.GmosNorthIfu) |
                  Some(_: BasicConfiguration.GmosSouthIfu) |
                  Some(_: BasicConfiguration.Flamingos2LongSlit) |
                  Some(_: BasicConfiguration.Flamingos2Mos) |
                  Some(_: BasicConfiguration.Igrins2LongSlit.type) |
                  Some(_: BasicConfiguration.GhostIfu) |
                  Some(_: BasicConfiguration.GnirsSpectroscopy) =>
                ItcSpectroscopyTile(
                  props.vault.userId,
                  props.observation.get,
                  selectedConfig.get.configs.headOption.map(_.instrumentConfig),
                  props.obsTargets,
                  customSedTimestamps,
                  globalPreferences
                ).some
              // Visitor & exchange instruments have no ITC, hide the itc tile.
              case Some(_: BasicConfiguration.Visitor) | Some(_: BasicConfiguration.KeckExchange) |
                  Some(_: BasicConfiguration.SubaruExchange) =>
                none
              case None => ItcEmptyTile().some

          val ghostSkyPositionView: Option[View[Option[Coordinates]]] =
            props.observation
              .zoom(ghostSkyPositionLens)
              .map:
                _.undoableView(Iso.id[Option[Coordinates]].asLens)
                  .withOnMod: coords =>
                    ctx.odbApi.updateGhostIfu2SkyPosition(List(props.obsId), coords).runAsync

          // The explicit Base Position override. Undoable, like the sky position
          val baseView: View[Option[Coordinates]] =
            props.observation
              .zoom(Observation.explicitBase)
              .undoableView(Iso.id[Option[Coordinates]].asLens)
              .withOnMod: coords =>
                ctx.odbApi.updateExplicitBase(List(props.obsId), coords).runAsync

          val ghostIfuMapping: Option[GhostIfuMapping] = props.ghostIfuMapping(obsTimeOrNow)

          // If we have an observation calibration group we want to plot the targets together
          val obsCalibrationGroup: Map[ObjectPlotData.Id, ObjectPlotData] =
            (for {
              gid   <- props.observation.get.groupId
              group <- props.programSummaries.groups.get(gid)
              if group.isObsCalibration
            } yield props.programSummaries.groupsChildren
              .getOrElse(gid.some, Nil)
              // Collect siblings, aka the calibrations
              .collect:
                case Left(obs) if obs.id =!= props.obsId => obs
              .flatMap: siblings =>
                val targets = siblings.scienceTargetIds.toList
                  .flatMap(tid => props.programSummaries.targets.get(tid).map(_.target))
                  // only an unresolved ToO has nothing to plot
                  .filter(_.resolution.isDefined)

                NonEmptyList
                  .fromList(targets)
                  .map: nel =>
                    val name: NonEmptyString =
                      NonEmptyString.from(s"${siblings.title}".take(100)).getOrElse("-".refined)
                    val sites                = siblings.basicConfiguration.toList.flatMap(_.siteFor)

                    ObjectPlotData.Id(siblings.id.asLeft) ->
                      ObjectPlotData(name,
                                     nel,
                                     sites,
                                     elevationOnly = siblings.isCalibration,
                                     filled = false
                      )
              .toMap).getOrElse(Map.empty)

          val plotData: Option[PlotData] =
            props.scienceTargetsForTracking.map: ts =>
              val scienceName =
                if (obsCalibrationGroup.nonEmpty)
                  ts.map(_.name.value).toList.mkString(", ")
                else
                  props.obsId.show

              PlotData:
                Map(
                  ObjectPlotData.Id(props.obsId.asLeft) ->
                    ObjectPlotData(
                      NonEmptyString.from(scienceName).getOrElse("-".refined),
                      ts,
                      obsConf.configuration.flatMap(_.siteFor).foldMap(List(_)),
                      elevationOnly = props.observation.get.isCalibration
                    )
                ) ++ obsCalibrationGroup

          val skyPlotTile: Option[Tile[?]] =
            plotData.map: pd =>
              ElevationPlotTile(
                props.vault.userId,
                ObsTabTileIds.PlotId.id,
                pd,
                props.observation.get.basicConfiguration.flatMap(_.siteFor),
                obsTimeView.get,
                props.obsDuration.map(_.toDuration),
                obsCalibrationGroup.isEmpty,
                props.observation.get.schedulingConstraints.timingWindows,
                globalPreferences.get,
                Constants.NoTargetSelected,
                props.programSummaries.cfpDate
              )

          def getObsInfo(obsId: Observation.Id)(targetId: Target.Id): TargetEditObsInfo =
            TargetEditObsInfo.fromProgramSummaries(
              targetId,
              ObsIdSet.one(obsId).some,
              props.programSummaries
            )

          def setCurrentTarget(
            tid: Option[Target.Id],
            via: SetRouteVia
          ): Callback =
            // Set the route base on the selected target
            ctx.setPageVia(
              (AppTab.Observations,
               props.programId,
               Focused(ObsIdSet.one(props.obsId).some, tid)
              ).some,
              via
            )

          def onCloneTarget(params: OnCloneParameters): Callback =
            setCurrentTarget(params.idToAdd.some, SetRouteVia.HistoryReplace)

          def onAsterismUpdate(params: OnAsterismUpdateParams): Callback =
            val targetForPage: Option[Target.Id] =
              if (params.areAddingTarget) params.targetId.some else none
            setCurrentTarget(targetForPage, SetRouteVia.HistoryReplace)

          // Blind offsets do not participate in undo/redo
          val blindOffsetView = props.observation.model
            .zoom(Observation.blindOffset)
            .withOnMod: bo =>
              // We want to focus the blind offset if the use did a search or is doing next/previous,
              // but not if a new Automatic one is selected.
              if bo.isManual then
                setCurrentTarget(bo.blindOffsetTargetId, SetRouteVia.HistoryReplace)
              else Callback.empty

          // Only ghost has sky positions. this is the only place where we know it is ghost related
          // but it is abstracted away downstream.
          // The sky can be assigned to IFU1 (SkyPlusTarget) or IFU2 (TargetPlusSky) depending on the mapping.
          val skySlot: SlotId =
            ghostIfuMapping match
              case Some(_: GhostIfuMapping.SkyPlusTarget) => SlotId.GhostIfu1
              case _                                      => SlotId.GhostIfu2

          val slotPositions =
            ghostSkyPositionView.map(skySlot -> _).toList :+ (SlotId.Base -> baseView)

          // The telluric star type observed by a telluric calibration, shown next to
          // its system-assigned target. Hidden while the observing mode is hydrating.
          val telluricType: Option[TelluricType] =
            Option
              .when(props.observation.get.calibrationRole.contains(CalibrationRole.Telluric)):
                props.observation.get.observingMode.toOption.flatten
              .flatten
              .flatMap(ObservingMode.telluricType.getOption)

          val targetTile = // : Tile[?] =
            ObservationTargetsEditorTile(
              props.vault.userId,
              ObsTabTileIds.TargetId.id,
              props.programId,
              props.programType,
              ObsIdSet.one(props.obsId),
              props.obsAndTargets,
              obsTimeView,
              obsDurationView,
              obsConf,
              digest,
              props.focusedTarget,
              setCurrentTarget,
              onCloneTarget,
              onAsterismUpdate,
              getObsInfo(props.obsId),
              props.searching,
              "Targets",
              props.userPreferences,
              guideStarSelection,
              props.attachments,
              props.vault.map(_.token),
              props.obsIsReadonly,
              allowEditingOngoing = props.isStaffOrAdminUser,
              isStaffOrAdmin = props.isStaffOrAdminUser,
              telluricType = telluricType,
              slotPositions = slotPositions,
              // Any target changes invalidate the sequence
              sequenceChanged = sequenceChanged.set(pending),
              blindOffsetInfo = (props.obsId, blindOffsetView).some,
              trackingMap = trackingMapPot.some,
              ags = agsData
            )

          // The ExploreStyles.ConstraintsTile css adds a z-index to the constraints tile react-grid wrapper
          // so that the constraints selector dropdown always appears in front of any other tiles. If more
          // than one tile ends up having dropdowns in the tile header, we'll need something more complex such
          // as changing the css classes on the various tiles when the dropdown is clicked to control z-index.
          val optAsterismCoords: Option[Coordinates] =
            props.targetCoords(obsTimeOrNow, oBaseTracking.value)

          val conditionsLikelihood: Option[IntCentiPercent] =
            props.obsConditionsLikelihood(optAsterismCoords)
          val constraintsTile                               =
            ConstraintsTile(
              props.obsId,
              props.constraintSet,
              props.allConstraintSets,
              props.obsIQLikelihood(optAsterismCoords),
              conditionsLikelihood,
              props.centralWavelength,
              props.obsIsReadonly
            )

          val schedulingWindowsTile =
            ObservationSchedulingWindowsTile(
              props.observation,
              props.observation.get.hasTargetOfOpportunity(props.programSummaries.targets),
              props.obsIsReadonly,
              false
            )

          val configurationTile =
            ConfigurationTile(
              props.vault.userId,
              props.programId,
              props.obsId,
              props.observation.zoom(Observation.scienceRequirements),
              props.observation
                .zoom(
                  (Observation.posAngleConstraint, Observation.observingModeOption).disjointZip
                ),
              props.observation.get.scienceTargetIds,
              optAsterismCoords,
              obsConf,
              selectedConfig,
              revertedInstrumentConfig,
              props.modes,
              customSedTimestamps,
              props.obsTargets,
              props.programSummaries.observingModeGroups,
              sequenceChanged.mod {
                case Ready(_) => pending
                case x        => x
              } >> agsState.set(AgsState.Calculating),
              props.readonly, // execution status is taken care of in the configuration tile
              ObsIdSetEditInfo.of(props.observation.get),
              globalPreferences.get.wavelengthUnits,
              props.isStaffOrAdminUser,
              selectedItcTarget,
              props.observation.get.hasMaterializedSequence,
              props.observation.get.observingMode.isPending,
              MosMaskContext(
                props.attachments,
                attachmentsView,
                pastProposalReview
              )
            )

          val alltiles: List[Tile[?]] =
            List(
              notesTile.some,
              targetTile.some,
              Option.unless(props.vault.isGuest)(finderChartsTile),
              skyPlotTile,
              constraintsTile.some,
              schedulingWindowsTile.some,
              configurationTile.some,
              itcTile
            ).flattenOption

          val removedIds = ExploreGridLayouts.observations.removedTiles(props.calibrationRole)

          val tiles =
            alltiles.filterNot(t => removedIds.contains(t.id))

          React.Fragment(
            TileController(
              props.vault.userId,
              props.resize.width.getOrElse(0),
              defaultLayout,
              layout,
              tiles,
              section,
              props.backButton.some
            ),
            if isVisitorMode then EmptyVdom // Visitors have no sequences
            else
              TileController(
                props.vault.userId,
                props.resize.width.getOrElse(0),
                ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationsSequenceLayout),
                props.userPreferences.get.sequenceTileLayout,
                List(sequenceTile),
                GridLayoutSection.ObservationsSequenceLayout,
                renderBackButton = none,
                clazz = ExploreStyles.SequenceTileController.some
              )
          )
