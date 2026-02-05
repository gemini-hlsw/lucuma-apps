// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.actions.ObservationPasteIntoAsterismAction
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.GuideStarSelection.AgsSelection
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.observationtree.AsterismGroupObsList
import explore.plots.ElevationPlotTile
import explore.plots.ObjectPlotData
import explore.plots.PlotData
import explore.shortcuts.*
import explore.shortcuts.given
import explore.targeteditor.ObservationTargetsEditorTile
import explore.targets.TargetPasteAction
import explore.targets.TargetSummaryTile
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.enums.Site
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Message.Severity
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.schemas.model.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso

import java.time.Instant
import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo

case class TargetTabContents(
  programId:        Program.Id,
  userId:           Option[User.Id],
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  authToken:        Option[NonEmptyString],
  readonly:         Boolean,
  isStaffOrAdmin:   Boolean
) extends ReactFnProps(TargetTabContents.component):
  private val programType: Option[ProgramType] = programSummaries.get.programType

  private val targets: UndoSetter[TargetList] = programSummaries.zoom(ProgramSummaries.targets)

  private val focusedIds: Option[Either[Target.Id, ObsIdSet]] =
    focused match
      case Focused(Some(obsIdSet), _, _)    => obsIdSet.asRight.some
      case Focused(None, Some(targetId), _) => targetId.asLeft.some
      case _                                => none

  val focusedSummaryTargetId: Option[Target.Id] =
    focusedIds.flatMap(_.left.toOption)

  val focusedAsterismTargetId: Option[Target.Id] =
    focused match
      case Focused(Some(_), Some(targetId), _) => targetId.some
      case _                                   => none

  private def sitesForTarget(targetId: Target.Id): List[Site] =
    programSummaries.get.targetObservations
      .get(targetId)
      .foldMap: obsIds =>
        obsIds.toList
          .map: obsId =>
            programSummaries.get.observations
              .get(obsId)
              .flatMap(_.observingMode.map(_.siteFor))
          .flattenOption

  private val obsAndTargets: UndoSetter[ObservationsAndTargets] =
    programSummaries.zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip)

  private val observations: ObservationList = obsAndTargets.get._1

  private val globalPreferences: View[GlobalPreferences] =
    userPreferences.zoom(UserPreferences.globalPreferences)

  private val attachments: View[AttachmentList] =
    programSummaries.model.zoom(ProgramSummaries.attachments)

object TargetTabContents extends TwoPanels:
  private type Props = TargetTabContents

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized) // Two panel state
      .useEffectWithDepsBy((props, _, _) => props.focused): (_, _, selectedPanel) =>
        focused =>
          (focused, selectedPanel.get) match
            case (Focused(Some(_), _, _), _)                    => selectedPanel.set(SelectedPanel.Editor)
            case (Focused(None, Some(_), _), _)                 => selectedPanel.set(SelectedPanel.Editor)
            case (Focused(None, None, _), SelectedPanel.Editor) =>
              selectedPanel.set(SelectedPanel.Summary)
            case _                                              => Callback.empty
      .useStateViewBy((_, _, _) => List.empty[Target.Id]) // Selected targets on table
      .useLayoutEffectWithDepsBy((props, _, _, _) => props.focused.target):
        // If a target enters edit mode, unselect the rows.
        (_, _, _, selTargetIds) => _.foldMap(_ => selTargetIds.set(List.empty))
      .useMemoBy((props, _, _, selTargetIds) => (props.focusedIds, selTargetIds.get)): // Selected observations (right) or targets (left)
        (_, _, _, _) =>
          (focusedIds, selTargetIds) =>
            focusedIds
              .map(_.leftMap(TargetIdSet.one(_)))
              .orElse:
                TargetIdSet.fromTargetIdList(selTargetIds).map(_.asLeft)
      .useState[LocalClipboard](
        LocalClipboard.Empty
      ) // shadowClipboard (a copy of the clipboard as state)
      .useEffectOnMountBy: (_, ctx, _, _, _, shadowClipboard) => // initialize shadowClipboard
        import ctx.given
        ExploreClipboard.get.flatMap(shadowClipboard.setStateAsync)
      .useCallbackWithDepsBy((_, _, _, _, selIdsOpt, _) => selIdsOpt): // COPY Action Callback
        (_, ctx, _, _, _, shadowClipboard) =>
          selIdsOpt =>
            import ctx.given

            selIdsOpt.value
              .map:
                _.fold[(LocalClipboard, String)](
                  tids =>
                    (LocalClipboard.CopiedTargets(tids),
                     s"Copied target(s) ${tids.toList.mkString(", ")}"
                    ),
                  oids =>
                    (LocalClipboard.CopiedObservations(oids),
                     s"Copied observation(s) ${oids.idSet.toList.mkString(", ")}"
                    )
                )
              .map: (newClipboard, toastText) =>
                (ExploreClipboard.set(newClipboard) >>
                  shadowClipboard.setStateAsync(newClipboard))
                  .withToast(toastText)
              .orEmpty
              .runAsync
      .useCallbackWithDepsBy((props, _, _, _, selIdsOpt, _, _) => // PASTE Action Callback
        (selIdsOpt.flatMap(_.left.toOption).map(_.toList).orEmpty,
         props.focused.obsSet,
         props.observations,
         props.targets.get,
         props.programSummaries.get.asterismGroups,
         props.readonly
        )
      ): (props, ctx, _, _, _, _, _) =>
        (selTargetIds, optFocusedObsIds, obsList, targetList, asterismGroups, readonly) =>
          import ctx.given

          val selectObsIds: ObsIdSet => IO[Unit] =
            obsIds =>
              ctx.pushPage((AppTab.Targets, props.programId, Focused.obsSet(obsIds)).some).toAsync

          def hasToO(tids: List[Target.Id]): Boolean =
            tids
              .map(targetList.get)
              .flattenOption
              .exists(_.isTargetOfOpportunity)

          ExploreClipboard.get
            .flatMap {
              case LocalClipboard.CopiedObservations(copiedObsIdSet) =>
                val obsAndTargets: List[(Observation.Id, List[Target.Id])] =
                  optFocusedObsIds
                    .map: focusedObsIdSet => // This with some targets on the tree selected
                      copiedObsIdSet.idSet.toList.map: obsId =>
                        (obsId,
                         obsList // All focused obs have the same asterism, so we can use head
                           .get(focusedObsIdSet.idSet.head)
                           .foldMap(_.scienceTargetIds)
                           .toList
                        )
                    .getOrElse: // These are targets on the table
                      for
                        tid <- selTargetIds
                        oid <- copiedObsIdSet.idSet.toList
                      yield (oid, List(tid))

                IO.whenA(obsAndTargets.nonEmpty): // Apply the obs to selected targets on the tree
                  // Note: this action clones the observations with new targets
                  ObservationPasteIntoAsterismAction(obsAndTargets, props.expandedIds.async.mod)(
                    props.programSummaries
                  ).void.withToastDuring(
                    s"Pasting obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into ${selTargetIds.length} target(s)",
                    s"Pasted obs ${copiedObsIdSet.idSet.toList.mkString(", ")} into ${selTargetIds.length} target(s)".some
                  )

              case LocalClipboard.CopiedTargets(tids) =>
                optFocusedObsIds
                  .foldMap: obsIds =>
                    val observations: List[Observation] =
                      obsIds.idSet.toList.map(obsList.get).flattenOption

                    def hasExistingTargets: Boolean =
                      observations.exists(o =>
                        o.scienceTargetIds.nonEmpty || o.blindOffset.useBlindOffset
                      )

                    if observations.exists(_.isExecuted)
                    then
                      ToastCtx[IO].showToast(
                        "Cannot modify the asterism of executed observations.",
                        Severity.Error
                      )
                    else
                      // Only want to paste targets that aren't already in the target asterism or
                      // undo is messed up.
                      // If all the targets are already there, do nothing.
                      val targetAsterism: Option[AsterismGroup] =
                        asterismGroups.findContainingObsIds(obsIds)
                      targetAsterism
                        .flatMap(ag => tids.removeSet(ag.targetIds))
                        .foldMap: uniqueTids =>
                          if (hasToO(uniqueTids.idSet.toList) && hasExistingTargets)
                            || targetAsterism.exists(ag => hasToO(ag.targetIds.toList))
                          then
                            ToastCtx[IO].showToast(
                              "A Target of Opportunity must be the only target in an observation.",
                              Severity.Error
                            )
                          else
                            TargetPasteAction
                              .pasteTargets(
                                obsIds,
                                uniqueTids,
                                selectObsIds,
                                props.expandedIds
                              )
                              .set(props.programSummaries)(())
                              .toAsync

              case _ => IO.unit
            }
            .runAsync
            .unless_(readonly)
      .useGlobalHotkeysWithDepsBy((_, _, _, _, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback)
      ): (props, ctx, _, _, _, _, _, _) =>
        (copyCallback, pasteCallback) =>
          val callbacks: ShortcutCallbacks =
            case CopyAlt1 | CopyAlt2   => copyCallback
            case PasteAlt1 | PasteAlt2 => pasteCallback
            case GoToSummary           => ctx.pushPage((AppTab.Targets, props.programId, Focused.None).some)

          UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      .useStateView(AladinFullScreen.Normal) // full screen aladin
      .useResizeDetector() // Measure its size
      .useStateView[GuideStarSelection](AgsSelection(none)) // required for aladin but not in use
      .render:
        (
          props,
          ctx,
          selectedPanelView,
          selectedTargetIds,
          selectedIdsOpt,
          shadowClipboard,
          copyCallback,
          pasteCallback,
          fullScreen,
          resize,
          guideStarSelection
        ) =>
          import ctx.given

          def getObsInfo(editing: Option[ObsIdSet])(targetId: Target.Id): TargetEditObsInfo =
            TargetEditObsInfo.fromProgramSummaries(targetId, editing, props.programSummaries.get)

          def findAsterismGroup(obsIds: ObsIdSet, agl: AsterismGroupList): Option[AsterismGroup] =
            agl.find((agObsIds, _) => obsIds.subsetOf(agObsIds)).map(AsterismGroup.fromTuple)

          def setPage(focused: Focused): Callback =
            ctx.pushPage((AppTab.Targets, props.programId, focused).some)

          def selectObservationAndTarget(expandedIds: View[SortedSet[ObsIdSet]])(
            obsId:    Observation.Id,
            targetId: Target.Id
          ): Callback =
            val obsIdSet = ObsIdSet.one(obsId)
            findAsterismGroup(obsIdSet, props.programSummaries.get.asterismGroups)
              .map(ag => expandedIds.mod(_ + ag.obsIds))
              .orEmpty >>
              setPage(Focused(obsIdSet.some, targetId.some))

          def focusTargetId(oTargetId: Option[Target.Id]): Callback =
            oTargetId.fold(
              selectedPanelView.set(SelectedPanel.Summary) *>
                setPage(Focused.None)
            ): targetId =>
              setPage(Focused.target(targetId))

          val targetTree: VdomNode =
            AsterismGroupObsList(
              props.programId,
              props.focused,
              props.expandedIds,
              props.programSummaries,
              selectedIdsOpt,
              shadowClipboard.value,
              focusTargetId,
              selectedTargetIds.set,
              props.programSummaries.undoableView(ProgramSummaries.targets).mod,
              copyCallback,
              pasteCallback,
              props.programSummaries.get.allocatedScienceBands,
              props.readonly
            )

          val backButton: VdomNode =
            makeBackButton(props.programId, AppTab.Targets, selectedPanelView, ctx)

          /**
           * Render the summary table.
           */
          val renderSummary =
            TargetSummaryTile(
              props.userId,
              props.programId,
              props.targets.model,
              props.programSummaries.get.targetObservations,
              selectObservationAndTarget(props.expandedIds),
              selectedTargetIds,
              props.focusedSummaryTargetId,
              focusTargetId,
              props.readonly,
              backButton
            )

          val plotData: PlotData =
            PlotData:
              props.focusedAsterismTargetId
                .map(List(_))
                .getOrElse(selectedTargetIds.get)
                .flatMap: targetId =>
                  props.targets.get
                    .get(targetId)
                    .filterNot(twid => Target.opportunity.getOption(twid.target).isDefined)
                    .map: targetWithId =>
                      ObjectPlotData.Id(targetId.asRight) -> ObjectPlotData(
                        targetWithId.target.name,
                        NonEmptyList.one(targetWithId.target),
                        props.sitesForTarget(targetId)
                      )
                .toMap

          /**
           * Render the asterism editor
           *
           * @param idsToEdit
           *   The observations to include in the edit. This needs to be a subset of the ids in
           *   asterismGroup
           * @param asterismGroup
           *   The AsterismGroup that is the basis for editing. All or part of it may be included in
           *   the edit.
           */
          def renderAsterismEditor(idsToEdit: ObsIdSet): List[Tile[?]] = {
            val getObsTime: ProgramSummaries => Option[Instant] = a =>
              for
                id <- idsToEdit.single
                o  <- a.observations.get(id)
                t  <- o.observationTime
              yield t

            def modObsTime(
              mod: Option[Instant] => Option[Instant]
            ): ProgramSummaries => ProgramSummaries = ps =>
              idsToEdit.single
                .map: i =>
                  ProgramSummaries.observations
                    .filterIndex((id: Observation.Id) => id === i)
                    .andThen(Observation.observationTime)
                    .modify(mod)(ps)
                .getOrElse(ps)

            val obsTimeView: View[Option[Instant]] =
              props.programSummaries.model.zoom(getObsTime)(modObsTime)

            val getObsDuration: ProgramSummaries => Option[TimeSpan] = a =>
              for
                id <- idsToEdit.single
                o  <- a.observations.get(id)
                t  <- o.observationDuration
              yield t

            def modObsDuration(
              mod: Option[TimeSpan] => Option[TimeSpan]
            ): ProgramSummaries => ProgramSummaries = ps =>
              idsToEdit.single
                .map: i =>
                  ProgramSummaries.observations
                    .filterIndex((id: Observation.Id) => id === i)
                    .andThen(Observation.observationDuration)
                    .modify(mod)(ps)
                .getOrElse(ps)

            val obsDurationView: View[Option[TimeSpan]] =
              props.programSummaries.model.zoom(getObsDuration)(modObsDuration)

            val title = idsToEdit.single match {
              case Some(id) => s"Observation $id"
              case None     => s"Editing ${idsToEdit.size} Asterisms"
            }

            val obsConf = idsToEdit.single match {
              case Some(id) =>
                props.programSummaries.get.observations.values.toList
                  .collect:
                    case o @ Observation(
                          id = obsId,
                          constraints = const,
                          observingMode = Some(conf)
                        ) if obsId === id =>
                      (const, conf.toBasicConfiguration, o.needsAGS(props.targets.get))
                  .headOption
              case _        => None
            }

            val constraints                               = obsConf.map(_._1)
            val configuration: Option[BasicConfiguration] = obsConf.map(_._2)
            val needsAGS                                  = obsConf.exists(_._3)

            def setCurrentTarget(oids: Option[ObsIdSet])(
              tid: Option[Target.Id],
              via: SetRouteVia
            ): Callback =
              ctx.setPageVia((AppTab.Targets, props.programId, Focused(oids, tid)).some, via)

            def onCloneTarget4Asterism(params: OnCloneParameters): Callback =
              // props.programSummaries.get will always contain the original groups. On creating,
              val allOriginalGroups = props.programSummaries.get.asterismGroups
                .filterForObsInSet(params.obsIds)
                .map(_._1)
                .toSet
              selectedTargetIds.set(List(params.idToAdd)) >>
                (if (params.areCreating) {
                   val obsIds4Url =
                     ObsIdSet.fromSortedSet(idsToEdit.idSet.intersect(params.obsIds.idSet))
                   // all of the original groups that have any of the cloned ids
                   // Deal with the expanded groups - we'll open all affected groups
                   allOriginalGroups.toList.traverse { ids =>
                     val intersect = ids.idSet.intersect(params.obsIds.idSet)
                     if (intersect === ids.idSet.toSortedSet)
                       // it is the whole group, so make sure it is open
                       props.expandedIds.mod(_ + ids)
                     else
                       // otherwise, close the original and open the subsets
                       ObsIdSet
                         .fromSortedSet(intersect)
                         .foldMap(i => props.expandedIds.mod(_ - ids + i + ids.removeUnsafe(i)))
                   }.void >>
                     setCurrentTarget(obsIds4Url)(params.cloneId.some, SetRouteVia.HistoryReplace)
                 } else {
                   // We'll open all of the original groups who had any observations affected by the cloning.
                   props.expandedIds.mod(_ ++ SortedSet.from(allOriginalGroups)) >>
                     setCurrentTarget(idsToEdit.some)(
                       params.originalId.some,
                       SetRouteVia.HistoryReplace
                     )
                 })

            def onAsterismUpdate(params: OnAsterismUpdateParams): Callback =
              val originalGroups = props.programSummaries.get.asterismGroups
              // props.programSummaries.get will always contain the original groups, so we should find the group
              originalGroups
                .findContainingObsIds(params.obsIds)
                .foldMap(group =>
                  val newAsterism                      =
                    if (params.isAddAction) group.targetIds + params.targetId
                    else group.targetIds - params.targetId
                  val existingGroup                    = originalGroups.findWithTargetIds(newAsterism)
                  val mergedObs                        = existingGroup.map(_.obsIds ++ params.obsIds)
                  val obsIdsAfterAction                = mergedObs.getOrElse(params.obsIds)
                  val unmodified                       = group.obsIds -- params.obsIds
                  val setExpanded                      = unmodified.fold {
                    if (params.isUndo) props.expandedIds.mod(_ - obsIdsAfterAction + group.obsIds)
                    else props.expandedIds.mod(_ - group.obsIds + obsIdsAfterAction)
                  }(unmod =>
                    if (params.isUndo)
                      props.expandedIds.mod(_ - obsIdsAfterAction - unmod + group.obsIds)
                    else props.expandedIds.mod(_ - group.obsIds + obsIdsAfterAction + unmod)
                  )
                  val targetForPage: Option[Target.Id] =
                    if (params.areAddingTarget) params.targetId.some
                    else none // if we're deleting, let UI focus the first one in the asterism
                  val setPage: Callback                =
                    if (params.isUndo)
                      setCurrentTarget(idsToEdit.some)(targetForPage, SetRouteVia.HistoryReplace)
                    else
                      setCurrentTarget(params.obsIds.some)(
                        targetForPage,
                        SetRouteVia.HistoryReplace
                      )
                  setExpanded >> setPage
                )

            val asterismEditorTile = // : Option[Tile[?]] =
              props.programType.map: programType =>
                ObservationTargetsEditorTile(
                  props.userId,
                  TargetTabTileIds.AsterismEditor.id,
                  props.programId,
                  programType,
                  idsToEdit,
                  props.obsAndTargets,
                  obsTimeView,
                  obsDurationView,
                  ObsConfiguration.forPlainTarget(
                    configuration,
                    constraints,
                    needsAGS,
                    none
                  ),
                  none[ExecutionDigest].asReady,
                  props.focused.target,
                  setCurrentTarget(idsToEdit.some),
                  onCloneTarget4Asterism,
                  onAsterismUpdate,
                  getObsInfo(idsToEdit.some),
                  props.searching,
                  title,
                  props.userPreferences,
                  guideStarSelection,
                  props.attachments,
                  props.authToken,
                  props.readonly,
                  allowEditingOngoing =
                    false, // only allow editing of ongoing observations from the obs tab
                  isStaffOrAdmin = props.isStaffOrAdmin,
                  backButton = backButton.some
                )

            val skyPlotTile = // : Tile[?] =
              ElevationPlotTile(
                props.userId,
                TargetTabTileIds.ElevationPlot.id,
                plotData,
                configuration.map(_.siteFor),
                obsTimeView.get,
                none,
                plotData.value.size === 1,
                Nil,
                props.globalPreferences.get,
                Constants.NoTargetSelected
              )

            List(asterismEditorTile, skyPlotTile.some).flattenOption
          }

          // We still want to render these 2 tiles, even when not shown, so as not to mess up the stored layout.
          val dummyTargetTile: Tile[?]    =
            Tile.Dummy(TargetTabTileIds.AsterismEditor.id)
          val dummyElevationTile: Tile[?] =
            Tile.Dummy(TargetTabTileIds.ElevationPlot.id)

          /**
           * Renders a single sidereal target editor without an obs context
           */
          def renderSiderealTargetEditor(targetId: Target.Id): Option[SingleTargetEditorTile] = {
            def onCloneTarget4Target(params: OnCloneParameters): Callback =
              // It's not perfect, but we'll go to whatever url has the "new" id. This means
              // that if the user went elsewhere before doing undo/redo, they will go back to the new target.
              selectedTargetIds.set(List(params.idToAdd)) >>
                ctx.replacePage:
                  (AppTab.Targets, props.programId, Focused.target(params.idToAdd)).some

            (props.targets.zoom(Iso.id[TargetList].index(targetId)), props.programType)
              .mapN: (target, programType) =>
                SingleTargetEditorTile(
                  props.programId,
                  programType,
                  props.userId,
                  target,
                  props.obsAndTargets,
                  props.searching,
                  s"Editing Target ${target.get.target.name.value} [$targetId]",
                  fullScreen,
                  props.userPreferences,
                  guideStarSelection,
                  props.attachments,
                  props.authToken,
                  props.readonly,
                  props.isStaffOrAdmin,
                  getObsInfo(none)(targetId),
                  onCloneTarget4Target
                )
          }

          val skyPlotTile = // : Tile[?] =
            ElevationPlotTile(
              props.userId,
              TargetTabTileIds.ElevationPlot.id,
              plotData,
              selectedTargetIds.get.headOption.flatMap(props.sitesForTarget(_).headOption),
              none,
              none,
              plotData.value.size === 1,
              Nil,
              props.globalPreferences.get,
              Constants.NoTargetSelected
            )

          val rightSide = { (resize: UseResizeDetectorReturn) =>
            val observationSetTargetEditorTile
              : Option[List[Tile[?]]] = // Observations selected on tree
              props.focusedIds
                .flatMap(_.toOption)
                .flatMap: obsIds =>
                  findAsterismGroup(obsIds, props.programSummaries.get.asterismGroups)
                    .map: _ =>
                      renderAsterismEditor(obsIds)

            val singleTargetEditorTile
              : Option[SingleTargetEditorTile] = // Target selected on summary table
              props.focusedSummaryTargetId
                .map:
                  renderSiderealTargetEditor
                .flatten

            val selectedTargetsTiles: List[Tile[?]] =
              List(
                renderSummary,
                singleTargetEditorTile.getOrElse(dummyTargetTile),
                Option // Show plot if and only if the editor is hidden.
                  .when(singleTargetEditorTile.isEmpty)(skyPlotTile)
                  .getOrElse(dummyElevationTile)
              )

            // TODO I think this was always false as skyPlotTile is always defined? Maybe we should remove the onlySummary logic?
            val onlySummary: Boolean =
              observationSetTargetEditorTile.isEmpty && singleTargetEditorTile.isEmpty // && skyPlotTile.isEmpty

            val (tiles, key) =
              observationSetTargetEditorTile
                .map:
                  (_, TargetTabTileIds.AsterismEditor.id)
                .getOrElse:
                  (selectedTargetsTiles, TargetTabTileIds.Summary.id)

            TileController(
              props.userId,
              resize.width.getOrElse(1),
              ExploreGridLayouts.targets.defaultTargetLayouts,
              props.userPreferences.get.targetTabLayout,
              tiles,
              GridLayoutSection.TargetLayout,
              backButton.some,
              Option.when(onlySummary)(ExploreStyles.SingleTileMaximized),
              storeLayout = !onlySummary
            ).withKey(key.value): VdomNode
            // withKey is required for the controller to clear it's state between the different
            // layouts or it can get into an invalid state.
          }

          React.Fragment(
            if (LinkingInfo.developmentMode)
              FocusedStatus(AppTab.Targets, props.programId, props.focused)
            else EmptyVdom,
            makeOneOrTwoPanels(
              selectedPanelView,
              targetTree,
              rightSide,
              RightSideCardinality.Multi,
              resize
            )
          )
