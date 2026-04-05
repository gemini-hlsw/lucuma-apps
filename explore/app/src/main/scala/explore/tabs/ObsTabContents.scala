// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.modes.ScienceModes
import explore.observationtree.*
import explore.plots.ElevationPlotTile
import explore.plots.ObjectPlotData
import explore.plots.PlotData
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.{*, given}
import lucuma.ui.undo.UndoContext
import lucuma.ui.undo.UndoSetter
import lucuma.ui.undo.Undoer
import monocle.Iso
import monocle.Optional

object DeckShown extends NewBoolean { inline def Shown = True; inline def Hidden = False }
type DeckShown = DeckShown.Type

case class ObsTabContents(
  vault:            Option[UserVault],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  modes:            ScienceModes,
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedGroups:   View[Set[Group.Id]],
  readonly:         Boolean
) extends ReactFnProps(ObsTabContents.component)
    with ObsGroupHelper {
  protected val focusedObsId: Option[Observation.Id]      = focused.obsSet.map(_.head)
  private val focusedTarget: Option[Target.Id]            = focused.target
  protected val focusedGroupId: Option[Group.Id]          = focused.group
  protected val observations: UndoSetter[ObservationList] =
    programSummaries.zoom(ProgramSummaries.observations)
  protected val groups: UndoSetter[GroupList]             = programSummaries.zoom(ProgramSummaries.groups)
  private val targets: UndoSetter[TargetList]             = programSummaries.zoom(ProgramSummaries.targets)
  private val globalPreferences: View[GlobalPreferences]  =
    userPreferences.zoom(UserPreferences.globalPreferences)
  private val programType: Option[ProgramType]            = programSummaries.get.programType
  // XXX Workaround for what seems to be a Scala 3.8.2 bug where `ObsGroupHelper` members cannot be otherwise
  // accessed from within the component definition below.
  override val resolvedActiveGroupId: Option[Group.Id]    = super.resolvedActiveGroupId
}

object ObsTabContents extends TwoPanels:
  private type Props = ObsTabContents

  private def flattenScreenOrder(
    children: Map[Option[Group.Id], List[Either[Observation, Group]]],
    parentId: Option[Group.Id]
  ): List[Observation.Id] =
    children
      .get(parentId)
      .orEmpty
      .flatMap:
        case Left(obs)    => List(obs.id)
        case Right(group) => flattenScreenOrder(children, group.id.some)

  private def navigateObs(
    ctx:            AppContext[IO],
    programId:      Program.Id,
    focusedObsId:   Option[Observation.Id],
    groupsChildren: Map[Option[Group.Id], List[Either[Observation, Group]]],
    offset:         Int
  ): Callback =
    val obsKeys = flattenScreenOrder(groupsChildren, none)
    focusedObsId match
      case Some(currentId) =>
        val idx = obsKeys.indexOf(currentId)
        obsKeys.lift(idx + offset).foldMap(id => focusObs(programId, id.some, ctx))
      case None            =>
        (if (offset > 0) obsKeys.headOption else obsKeys.lastOption)
          .foldMap(id => focusObs(programId, id.some, ctx))

  private given Reusability[Map[Option[Group.Id], List[Either[Observation, Group]]]] =
    Reusability.map

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx                    <- useContext(AppContext.ctx)
        twoPanelState          <- useStateView[SelectedPanel](SelectedPanel.Uninitialized)
        _                      <- useEffectWithDeps(props.focusedObsId): focusedObs =>
                                    (focusedObs, twoPanelState.get) match
                                      case (Some(_), _)                 => twoPanelState.set(SelectedPanel.Editor)
                                      case (None, SelectedPanel.Editor) => twoPanelState.set(SelectedPanel.Summary)
                                      case _                            => Callback.empty
        resize                 <- useResizeDetector
        shadowClipboardObs     <- useState(none[ObsIdSet])
        _                      <- useEffectOnMount:
                                    import ctx.given

                                    ExploreClipboard.get.flatMap:
                                      _ match
                                        case LocalClipboard.CopiedObservations(idSet) =>
                                          shadowClipboardObs.setStateAsync(idSet.some)
                                        case _                                        => IO.unit
        selectedObsIds         <- useStateView(List.empty[Observation.Id])
        selectedOrFocusedObsIds =
          props.focusedObsId.map(ObsIdSet.one(_)).orElse(ObsIdSet.fromList(selectedObsIds.get))
        copyCallback           <- useCallbackWithDeps(selectedOrFocusedObsIds): selectedOrFocusedObsIds =>
                                    import ctx.given

                                    selectedOrFocusedObsIds
                                      .map: obsIdSet =>
                                        val msg =
                                          s"Copied observation(s) ${obsIdSet.idSet.toList.mkString(", ")}"

                                        (ExploreClipboard
                                          .set(LocalClipboard.CopiedObservations(obsIdSet)) >>
                                          shadowClipboardObs.setStateAsync(obsIdSet.some)).withToast(msg)
                                      .orUnit
                                      .runAsync
        pasteCallback          <- useCallbackWithDeps(
                                    (props.observations.get, props.resolvedActiveGroupId, props.readonly)
                                  ): (_, resolvedActiveGroupId, readonly) =>
                                    import ctx.given

                                    ExploreClipboard.get
                                      .flatMap:
                                        case LocalClipboard.CopiedObservations(obsIdSet) =>
                                          cloneObs(
                                            props.programId,
                                            obsIdSet.idSet.toList,
                                            resolvedActiveGroupId,
                                            props.observations,
                                            ctx
                                          ).withToastDuring(
                                            s"Duplicating obs ${obsIdSet.idSet.mkString_(", ")}"
                                          )
                                        case _                                           => IO.unit
                                      .runAsync
                                      .unless_(readonly)
        _                      <- useGlobalHotkeysWithDeps(
                                    (copyCallback,
                                     pasteCallback,
                                     props.focusedObsId,
                                     props.programSummaries.get.groupsChildren
                                    )
                                  ): (copyCallback, pasteCallback, focusedObsId, groupsChildren) =>
                                    def callbacks: ShortcutCallbacks = {
                                      case CopyAlt1 | CopyAlt2 => copyCallback

                                      case PasteAlt1 | PasteAlt2 => pasteCallback

                                      case GoToSummary =>
                                        ctx.setPageVia(
                                          (AppTab.Observations, props.programId, Focused.None).some,
                                          SetRouteVia.HistoryPush
                                        )

                                      case Down =>
                                        navigateObs(ctx, props.programId, focusedObsId, groupsChildren, 1)

                                      case Up =>
                                        navigateObs(ctx, props.programId, focusedObsId, groupsChildren, -1)
                                    }
                                    UseHotkeysProps(
                                      ((GoToSummary :: Up :: Down :: Nil) ::: (CopyKeys ::: PasteKeys)).toHotKeys,
                                      callbacks
                                    )
        deckShown              <- useStateView(DeckShown.Shown)
        addingObservation      <- useStateView(AddingObservation(false))
      } yield
        val observationsTree: VdomNode =
          if (deckShown.get === DeckShown.Shown) {
            ObsTree(
              props.programId,
              props.observations,
              props.groups,
              props.programSummaries.get.groupsChildren,
              props.programSummaries.get.parentGroups(_),
              props.programSummaries.get.groupWarnings,
              props.programSummaries: Undoer,
              props.focusedObsId,
              props.focusedTarget,
              props.focusedGroupId,
              selectedObsIds.get,
              twoPanelState.set(SelectedPanel.Summary),
              props.expandedGroups,
              deckShown,
              copyCallback,
              pasteCallback,
              shadowClipboardObs.value,
              props.programSummaries.get.allocatedScienceBands,
              addingObservation,
              props.readonly
            )
          } else
            <.div(ExploreStyles.TreeToolbar)(
              Button(
                severity = Button.Severity.Secondary,
                outlined = true,
                disabled = false,
                tooltip = "Show Observation Tree",
                tooltipOptions = DefaultTooltipOptions,
                icon = Icons.ArrowRightFromLine,
                clazz = ExploreStyles.ObsTreeHideShow,
                onClick = deckShown.mod(_.flip)
              ).mini.compact
            )

        val backButton: VdomNode =
          makeBackButton(props.programId, AppTab.Observations, twoPanelState, ctx)

        val obsSummaryTableTile: Tile[?] =
          ObsSummaryTile(
            props.vault.userId,
            props.programId,
            props.observations,
            selectedObsIds,
            props.groups.model,
            props.targets.get,
            props.programSummaries.get.allocatedScienceBands.nonEmpty,
            props.readonly,
            backButton
          )

        val plotData: PlotData =
          PlotData:
            selectedOrFocusedObsIds
              .foldMap(_.idSet.toList)
              .map(props.observations.get.get(_))
              .flattenOption
              .map: obs =>
                obs
                  .scienceTargetsForTracking(props.programSummaries.get.targets)
                  .map: targets =>
                    ObjectPlotData.Id(obs.id.asLeft) ->
                      ObjectPlotData(
                        NonEmptyString.unsafeFrom(s"${obs.title} (${obs.id.toString})"),
                        targets,
                        obs.basicConfiguration.foldMap(conf => List(conf.siteFor))
                      )
              .flattenOption
              .toMap

        val skyPlotTile: Tile[?] =
          ElevationPlotTile(
            props.vault.userId,
            ObsSummaryTabTileIds.PlotId.id,
            plotData,
            none, // TODO Deduce site from the first target?
            none,
            none,
            props.observations.get.size === 1,
            List.empty,
            props.globalPreferences.get,
            "No observation selected",
            props.programSummaries.get.cfpDate
          )

        val summaryTiles: VdomNode =
          TileController(
            props.vault.map(_.user.id),
            resize.width.getOrElse(0),
            ExploreGridLayouts.sectionLayout(GridLayoutSection.ObservationListLayout),
            props.userPreferences.get.observationListTabLayout,
            List(obsSummaryTableTile, skyPlotTile),
            GridLayoutSection.ObservationListLayout
          )

        def obsEditorTiles(obsId: Observation.Id, resize: UseResizeDetectorReturn): VdomNode = {
          val indexValue: Optional[ObservationList, Observation] =
            Iso.id[ObservationList].index(obsId)

          props.programType.map: programType =>
            props.observations.model
              .zoom(indexValue)
              .mapValue(obsView =>
                // FIXME Find a better mechanism for this.
                // Something like .mapValue but for UndoContext
                val obsUndoSetter: UndoSetter[Observation] =
                  props.observations.zoom(indexValue.getOption.andThen(_.get), indexValue.modify)
                val obs: Observation                       = obsUndoSetter.get
                val obsIsReadonly: Boolean                 =
                  props.readonly || addingObservation.get.value || obs.isCalibration
                ObsTabTiles(
                  props.vault,
                  props.programId,
                  programType,
                  props.modes,
                  backButton,
                  obsUndoSetter,
                  props.programSummaries
                    .zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip),
                  props.programSummaries.model.zoom(ProgramSummaries.attachments),
                  props.programSummaries.get,
                  props.focusedTarget,
                  props.searching,
                  // We need this as a separate view so it doesn't get in the way of undo and can be easily updated by AGS
                  obsView.zoom(Observation.selectedGSName),
                  resize,
                  props.userPreferences,
                  obsIsReadonly
                ).withKey(s"${obsId.show}")
              )
        }

        def groupEditorTiles(groupId: Group.Id, resize: UseResizeDetectorReturn): VdomNode =
          props.groups
            .zoom(Iso.id[GroupList].index(groupId))
            .map: group =>
              ObsGroupTiles(
                props.vault.userId,
                group,
                props.programSummaries.get.groupWarnings.get(group.get.id),
                props.programSummaries.get.groupsChildren.get(groupId.some).map(_.length).orEmpty,
                resize,
                ExploreGridLayouts.sectionLayout(GridLayoutSection.GroupEditLayout),
                props.userPreferences.get.groupEditLayout,
                props.readonly,
                backButton
              )

        def rightSide(resize: UseResizeDetectorReturn): VdomNode =
          (props.focusedObsId, props.focusedGroupId) match
            case (Some(obsId), _)   => obsEditorTiles(obsId, resize)
            case (_, Some(groupId)) => groupEditorTiles(groupId, resize)
            case _                  => summaryTiles

        makeOneOrTwoPanels(
          twoPanelState,
          observationsTree,
          rightSide,
          RightSideCardinality.Multi,
          resize,
          ExploreStyles.ObsHiddenToolbar.when_(deckShown.get === DeckShown.Hidden)
        )
