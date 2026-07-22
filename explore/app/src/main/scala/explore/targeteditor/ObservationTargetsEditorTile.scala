// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.components.ColumnSelectorInTitle
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.BlindOffset
import explore.model.ConfigurationForVisualization
import explore.model.GhostSkySlot
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetList
import explore.model.UserPreferences
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import explore.services.OdbObservationApi
import explore.shortcuts.*
import explore.shortcuts.given
import explore.targets.TargetColumns
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetWithId
import lucuma.ui.optics.getWithDefault
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.UndoSetter
import monocle.Iso

import java.time.Instant
import scala.collection.immutable.SortedSet

final case class ObservationTargetsEditorTile(
  userId:              Option[User.Id],
  tileId:              Tile.TileId,
  programId:           Program.Id,
  programType:         ProgramType,
  obsIds:              ObsIdSet,
  obsAndTargets:       UndoSetter[ObservationsAndTargets],
  obsTime:             View[Option[Instant]],
  obsDuration:         View[Option[TimeSpan]],
  obsConf:             ObsConfiguration,
  digest:              CalculatedValue[Option[ExecutionDigest]],
  focusedTargetId:     Option[Target.Id],
  setTarget:           (Option[Target.Id], SetRouteVia) => Callback,
  onCloneTarget:       OnCloneParameters => Callback,
  onAsterismUpdate:    OnAsterismUpdateParams => Callback,
  obsInfo:             Target.Id => TargetEditObsInfo,
  searching:           View[Set[Target.Id]],
  titleText:           String,
  userPreferences:     View[UserPreferences],
  guideStarSelection:  View[GuideStarSelection],
  attachments:         View[AttachmentList],
  authToken:           Option[NonEmptyString],
  readonly:            Boolean,
  allowEditingOngoing: Boolean,
  isStaffOrAdmin:      Boolean,
  slotPositions:    List[(SlotId, View[Option[Coordinates]])] = Nil,
  sequenceChanged:     Callback = Callback.empty,
  blindOffsetInfo:     Option[(Observation.Id, View[BlindOffset])] = None,
  backButton:          Option[VdomNode] = None
)(using val odbApi: OdbObservationApi[IO])
    extends Tile[ObservationTargetsEditorTile](
      tileId,
      titleText,
      renderBackButton = backButton,
      bodyClass = ExploreStyles.TargetTileBody,
      controllerClass = ExploreStyles.TargetTileController
    )(ObservationTargetsEditorTile):
  val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)
  val prefTargetId: Option[Target.Id]    =
    obsIds.single.flatMap(userPreferences.get.observationPreferences.get)

case class SkyAssignmentState private (
  configForViz:  Option[ConfigurationForVisualization],
  availableSlot: Option[SlotId],
  isAssignable:  Boolean
):
  // Whether the sky can be assigned right now: a slot exists and it's in an assignable state.
  def canAssign: Boolean = availableSlot.isDefined && isAssignable

object SkyAssignmentState:
  def apply(
    obsConf:               ObsConfiguration,
    isTargetOfOpportunity: Target.Id => Boolean,
    readonly:              Boolean,
    allAreExecuted:        Boolean,
    hasScienceTargets:     Boolean
  ): SkyAssignmentState =
    val configForViz     = ConfigurationForVisualization.fromObsConfiguration(obsConf)
    val skyAvailableSlot =
      configForViz.flatMap(GhostSkySlot.skySlotAvailable(_, isTargetOfOpportunity))
    // Whether we can allow assigning a sky position
    val isAssignable     = !readonly && !allAreExecuted && hasScienceTargets
    new SkyAssignmentState(configForViz, skyAvailableSlot, isAssignable)

object ObservationTargetsEditorTile
    extends TileComponent[ObservationTargetsEditorTile](
      { (props, tileSize) =>
        for
          ctx                 <- useContext(AppContext.ctx)
          // Memoize the effective observation time (from odb or now)
          // so we don't feed react-datepicker a fresh Instant.now on every render
          obsTimeOrNow        <- useMemo(props.obsTime.get)(_.getOrElse(Instant.now))
          columnVisibility    <- useStateView(TargetColumns.DefaultVisibility)
          // obsEditInfo <- useStateView[Option[ObsIdSetEditInfo]](none)
          adding              <- useStateView(AreAdding(false))
          obsEditInfo         <- useMemo((props.obsIds, props.obsAndTargets.get._1)):
                                   ObsIdSetEditInfo.fromObservationList
          // _            <- useLayoutEffectWithDeps(obsEditInfo): roei =>
          //                   props.obsEditInfo.set(roei.value.some)
          observations        <- useMemo((props.obsIds, props.obsAndTargets.get._1)): (ids, obses) =>
                                   ids.idSet.toList.map(obses.get).flatten
          scienceIds          <- useMemo(observations): os =>
                                   // all of the selected observations must have the same asterism
                                   os.headOption.fold(SortedSet.empty[Target.Id])(_.scienceTargetIds)
          distinctSite        <- useMemo(observations):
                                   _.value.map(_.site) match
                                     case head :: Nil => head
                                     case _           => none
          // Build asterism IDs that include blind offset
          targetIds           <- useMemo(
                                   (scienceIds, props.blindOffsetInfo.flatMap(_._2.get.blindOffsetTargetId))
                                 ): (scienceIds, oBlindId) =>
                                   // Include blind offset target in the IDs if present
                                   scienceIds.value ++ oBlindId.toList
          obsTargets          <- useMemo((targetIds, props.allTargets.get)): (ids, targets) =>
                                   ObservationTargets.fromIdsAndTargets(ids.value, targets)
          _                   <- useLayoutEffectWithDeps(
                                   (targetIds.value.toList, props.focusedTargetId, props.prefTargetId)
                                 ): (allTargetIds, focusedTargetId, preferredTargetOpt) =>
                                   // If the selected targetId is None, or not in the target list, we need
                                   // to select one. Firt select the preferred target if saved or else
                                   // the first science target. If no science target just pick the first overall target
                                   // Need to replace history here.
                                   focusedTargetId.filter(allTargetIds.contains) match
                                     case None =>
                                       val preferred = preferredTargetOpt.filter(allTargetIds.contains)
                                       val tid       =
                                         preferred.orElse(scienceIds.value.headOption).orElse(allTargetIds.headOption)
                                       props.setTarget(tid, SetRouteVia.HistoryReplace)
                                     case _    => Callback.empty
          skySelected         <- useStateView(none[SlotId])
          fullScreen          <- useStateView(AladinFullScreen.Normal)
          obsToCloneTo        <- useStateView(none[ObsIdSet])
          readonlyForStatuses <- useStateView(false)
          // Slot holding the "add sky" mode enabling alading interactive mode.
          addSkyMode          <- useStateView(none[SlotId])
          // Allow canceling the "add sky" mode with the Escape key.
          _                   <- useGlobalHotkeysWithDeps(addSkyMode.get.isDefined): active =>
                                   val callbacks: ShortcutCallbacks =
                                     case Esc => addSkyMode.set(none).when_(active)
                                   UseHotkeysProps(List(Esc).toHotKeys, callbacks)
          skyState            <- useMemo(
                                   (props.obsConf,
                                    props.allTargets.get,
                                    props.readonly,
                                    obsEditInfo.allAreExecuted,
                                    scienceIds.value.nonEmpty
                                   )
                                 ): (obsConf, targets, readonly, allAreExecuted, hasScienceTargets) =>
                                   SkyAssignmentState(
                                     obsConf,
                                     targets.get(_).exists(_.isTargetOfOpportunity),
                                     readonly,
                                     allAreExecuted,
                                     hasScienceTargets
                                   )
          // Leave the mode if the slot stops being assignable
          _                   <- useEffectWithDeps(skyState.canAssign): available =>
                                   addSkyMode.set(none).unless_(available)
        yield
          import ctx.given

          // The effective instant to display. Memoized in the hook above
          val obsTime: Instant = obsTimeOrNow.value

          val skyPositions: List[(SlotId, Coordinates)] =
            props.slotPositions.flatMap { case (slot, v) => v.get.map(slot -> _) }

          val clearSkyCallbacks: SlotId => Option[Callback] = slot =>
            props.slotPositions.collectFirst { case (`slot`, v) => v.set(None) }

          // Sky selection: combines skySelected and props.focusedTargetId
          val selectedAsterismSelection: View[Option[AsterismSelection]] =
            View(
              skySelected.get
                .map(AsterismSelection.Position.apply)
                .orElse(
                  props.focusedTargetId.map(AsterismSelection.Target.apply)
                ),
              (mod, cb) =>
                val current = skySelected.get
                  .map(AsterismSelection.Position.apply)
                  .orElse(
                    props.focusedTargetId.map(AsterismSelection.Target.apply)
                  )
                mod(current) match
                  case Some(AsterismSelection.Target(tid)) =>
                    skySelected.set(None) >>
                      props.setTarget(tid.some, SetRouteVia.HistoryPush) >>
                      cb(current, Some(AsterismSelection.Target(tid)))
                  case Some(AsterismSelection.Position(slot))   =>
                    skySelected.set(Some(slot)) >>
                      cb(current, Some(AsterismSelection.Position(slot)))
                  case None                                =>
                    skySelected.set(None) >>
                      props.setTarget(None, SetRouteVia.HistoryPush) >>
                      cb(current, None)
            )

          // Save the time here. this works for the obs and target tabs
          // It's OK to save the viz time for executed observations, I think.
          val obsTimeView: View[Instant] =
            View(
              obsTime,
              (f, cb) =>
                val newValue = f(obsTime).some
                props.obsTime.set(newValue) >>
                  cb(obsTime, newValue.getOrElse(obsTime))
            ).withOnMod: ct =>
              props.odbApi
                .updateVisualizationTimeAndDuration(props.obsIds.toList, ct.some, none)
                .runAsync

          val obsDurationView: View[Option[TimeSpan]] =
            props.obsDuration.withOnMod: t =>
              props.odbApi.updateVisualizationDuration(props.obsIds.toList, t).runAsync

          val obsTimeAndDurationView: View[(Instant, Option[TimeSpan])] =
            View(
              (obsTime, props.obsDuration.get),
              (mod, cb) =>
                val oldValue = (obsTime, props.obsDuration.get)
                val newValue = mod(oldValue)
                props.obsTime.set(newValue._1.some) >> props.obsDuration
                  .set(newValue._2) >> cb(oldValue, newValue)
            ).withOnMod: tuple =>
              props.odbApi
                .updateVisualizationTimeAndDuration(props.obsIds.toList, tuple._1.some, tuple._2)
                .runAsync

          val editWarningMsg: Option[String] =
            if (obsEditInfo.allAreOngoing)
              if (obsEditInfo.editing.size > 1)
                "All of the current observations are ongoing. Asterism is readonly.".some
              else "The current observation is ongoing. Asterism is readonly.".some
            else if (obsEditInfo.allAreCompleted)
              if (obsEditInfo.editing.size > 1)
                "All of the current observations have been completed. Asterism is readonly.".some
              else "The current observation has been completed. Asterism is readonly.".some
            else if (obsEditInfo.allAreExecuted)
              if (obsEditInfo.editing.size > 1)
                "All of the current observations have been executed. Asterism is readonly.".some
              else "The current observation has been executed. Asterism is readonly.".some
            else if (obsEditInfo.executed.isDefined)
              "Adding and removing targets will only affect the unexecuted observations.".some
            else none

          val obsTimeEditor = ObsTimeEditor(
            obsTimeView,
            obsDurationView,
            obsTimeAndDurationView,
            props.digest,
            props.obsIds.size > 1
          )

          val title =
            <.div(
              ExploreStyles.AsterismEditorTileTitle,
              if (tileSize.isMinimized)
                obsTimeEditor
              else
                React.Fragment(
                  // only pass in the unexecuted observations. Will be readonly if there aren't any
                  <.span(
                    AddTargetButton(
                      "Target",
                      props.programId,
                      obsEditInfo.unExecuted.getOrElse(props.obsIds),
                      props.obsAndTargets,
                      adding,
                      props.onAsterismUpdate,
                      readOnly = props.readonly || obsEditInfo.allAreExecuted,
                      allowBlindOffset = !props.readonly,
                      buttonClass = ExploreStyles.AddTargetButton,
                      blindOffsetInfo = props.blindOffsetInfo,
                      addSkyInfo = Option.when(skyState.configForViz.isDefined)(
                        AddSkyInfo(skyState.availableSlot, addSkyMode, skyState.canAssign)
                      )
                    )
                  ),
                  obsTimeEditor,
                  <.span(^.textAlign.right)(
                    ColumnSelectorInTitle(TargetColumns.AllColNames.toList, columnVisibility)
                  )
                )
            )

          val editorReadonly = props.readonly || obsEditInfo.allAreExecuted

          // Sky position callbacks used by AladinCell for both sky and target selection.
          // Only active while the user explicitly entered "add sky" mode
          val assignSky: Option[(SlotId, Coordinates) => IO[Unit]] =
            Option.when(!editorReadonly && addSkyMode.get.isDefined): (slot, coords) =>
              props.slotPositions
                .collectFirst:
                  case (`slot`, v) => v
                .foldMap(_.set(coords.some).to[IO]) *>
                addSkyMode.set(none).to[IO]

          val resetSky: Option[SlotId => IO[Unit]] =
            Option.unless(editorReadonly): slot =>
              props.slotPositions
                .collectFirst { case (`slot`, v) => v }
                .foldMap(_.set(None).to[IO])

          // ObservationTargets for AladinCell: focused on the selected target when editing a
          // target, or the full asterism when editing a sky position.
          val obsTargetsForAladin: Option[ObservationTargets] =
            selectedAsterismSelection.get match
              case Some(AsterismSelection.Target(tid)) =>
                ObservationTargets
                  .fromIdsAndTargets(targetIds, props.allTargets.get)
                  .map(_.focusOn(tid))
              case _                                   =>
                ObservationTargets.fromIdsAndTargets(targetIds, props.allTargets.get)

          // Form panel that changes with selection; AladinCell stays at the same tree position.
          val formContent: VdomNode =
            if skySelected.get.isDefined then
              skySelected.get.flatMap: slot =>
                props.slotPositions
                  .collectFirst:
                    case (`slot`, v) => v
                  .flatMap: view =>
                    view.get.map: _ =>
                      val skyCoordsView: View[Coordinates] =
                        view.zoom(getWithDefault(Coordinates.Zero))
                      <.div(LucumaPrimeStyles.FormColumnVeryCompact, ExploreStyles.TargetForm)(
                        PositionCoordinatesEditor(slot, skyCoordsView, editorReadonly)
                      )
            else
              (ObservationTargets.fromIdsAndTargets(targetIds, props.allTargets.get),
               props.focusedTargetId
              ).mapN: (targets, focusedTargetId) =>
                val selectedTargetOpt: Option[UndoSetter[TargetWithId]] =
                  props.allTargets.zoom(Iso.id[TargetList].index(focusedTargetId))

                val obsInfo = props.obsInfo(focusedTargetId)
                (selectedTargetOpt, props.userId).mapN: (targetWithId, userId) =>
                  TargetEditor(
                    props.programId,
                    props.programType,
                    userId,
                    targetWithId,
                    props.obsAndTargets,
                    targets.focusOn(focusedTargetId),
                    props.obsTime.get,
                    props.obsConf.some,
                    props.searching,
                    onClone = props.onCloneTarget,
                    obsInfo = obsInfo,
                    fullScreen = fullScreen,
                    userPreferences = props.userPreferences,
                    guideStarSelection = props.guideStarSelection,
                    attachments = props.attachments,
                    authToken = props.authToken,
                    readonly = props.readonly,
                    allowEditingOngoing = props.allowEditingOngoing,
                    isStaffOrAdmin = props.isStaffOrAdmin,
                    invalidateSequence = props.sequenceChanged,
                    blindOffsetInfo = props.blindOffsetInfo,
                    renderAladin = false,
                    externalObsToCloneTo = obsToCloneTo.some,
                    externalReadonlyForStatuses = readonlyForStatuses.some
                  )

          val body =
            <.div(ExploreStyles.AladinFullScreen.when(fullScreen.get.value))(
              editWarningMsg.map(msg => <.div(ExploreStyles.SharedEditWarning, msg)),
              TargetTable(
                props.userId,
                props.programId,
                obsEditInfo.unExecuted.getOrElse(props.obsIds),
                obsTargets,
                props.obsAndTargets,
                selectedAsterismSelection,
                props.onAsterismUpdate,
                props.obsTime.get,
                distinctSite,
                fullScreen.get,
                editorReadonly,
                props.blindOffsetInfo.map(_._2),
                columnVisibility,
                positions = skyPositions,
                clearPosition = clearSkyCallbacks
              ),
              // Shared-target ("this target is used in N observations") warning belongs with the
              // table, not the editor/aladin area below.
              props.focusedTargetId
                .filterNot(_ => skySelected.get.isDefined)
                .map: tid =>
                  TargetCloneSelector(
                    props.obsInfo(tid),
                    obsToCloneTo,
                    readonlyForStatuses,
                    props.allowEditingOngoing
                  ),
              <.div(ExploreStyles.TargetTileEditor)(
                (obsTargetsForAladin, props.userId).mapN: (aladinTargets, uid) =>
                  <.div(ExploreStyles.TargetGrid)(
                    AladinCell(
                      uid,
                      aladinTargets,
                      obsTime,
                      props.obsConf.some,
                      fullScreen,
                      props.userPreferences,
                      props.guideStarSelection,
                      props.blindOffsetInfo,
                      props.obsAndTargets.model.zoom(ObservationsAndTargets.targets),
                      assignSky,
                      addSkyMode.get,
                      resetSky,
                      props.isStaffOrAdmin,
                      editorReadonly
                    ),
                    formContent
                  )
              )
            )

          TileContents(title, body)
      }
    )
