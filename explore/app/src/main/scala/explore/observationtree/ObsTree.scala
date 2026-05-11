// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.ObsGroupHelper
import explore.components.ActionButtons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.model.enums.GroupWarning
import explore.model.syntax.all.*
import explore.tabs.DeckShown
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.pragmaticdnd.*
import lucuma.react.pragmaticdnd.facade.BaseEventPayload
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node
import lucuma.ui.dnd.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.{*, given}
import lucuma.ui.undo.UndoButtons
import lucuma.ui.undo.UndoSetter
import lucuma.ui.undo.Undoer
import lucuma.ui.utils.*
import monocle.Iso
import org.scalajs.dom

import scala.collection.immutable.SortedSet
import scala.scalajs.js
import explore.model.ObservationsAndGroups

case class ObsTree(
  programId:             Program.Id,
  observationsAndGroups: UndoSetter[ObservationsAndGroups],
  groupsChildren:        Map[Option[Group.Id], List[Either[Observation, Group]]],
  parentGroups:          Either[Observation.Id, Group.Id] => List[Group.Id],
  groupWarnings:         Map[Group.Id, NonEmptySet[GroupWarning]],
  undoer:                Undoer,
  focusedObsId:          Option[Observation.Id], // obs explicitly selected for editing
  focusedTargetId:       Option[Target.Id],
  focusedGroupId:        Option[Group.Id],
  selectedObsIds:        List[Observation.Id],   // obs list selected in table
  setSummaryPanel:       Callback,
  expandedGroups:        View[Set[Group.Id]],
  deckShown:             View[DeckShown],
  copyCallback:          Callback,
  pasteCallback:         Callback,
  clipboardObsContents:  Option[ObsIdSet],
  allocatedScienceBands: SortedSet[ScienceBand],
  addingObservation:     View[AddingObservation],
  readonly:              Boolean
) extends ReactFnProps(ObsTree.component)
    with ObsGroupHelper:
  private val selectedObsIdSet: Option[ObsIdSet] =
    focusedObsId.map(ObsIdSet.one(_)).orElse(ObsIdSet.fromList(selectedObsIds))

  protected val observations: UndoSetter[ObservationList] =
    observationsAndGroups.zoom(ObservationsAndGroups.observations)
  protected val groups: UndoSetter[GroupList]             =
    observationsAndGroups.zoom(ObservationsAndGroups.groups)

  // XXX Workaround for what seems to be a Scala 3.8.? bug where `ObsGroupHelper` members cannot be otherwise
  // accessed from within the component definition below.
  override def resolveGroupId(groupId: Option[Group.Id]): Option[Group.Id] =
    super.resolveGroupId(groupId)
  override val resolvedActiveGroupId: Option[Group.Id]                     = super.resolvedActiveGroupId

  private val focusedObsOrGroup: Option[Either[Observation.Id, Group.Id]] =
    focusedObsId.map(_.asLeft).orElse(focusedGroupId.map(_.asRight))

  private def groupInfo(
    elem: Either[Observation.Id, Group.Id]
  ): Option[(Option[Group.Id], NonNegShort)] =
    elem.fold(
      observations.get.get(_).map(obs => (obs.groupId, obs.groupIndex)),
      groups.get.get(_).map(group => (group.parentId, group.parentIndex))
    )

  private val focusedGroupInfo: Option[(Option[Group.Id], NonNegShort)] =
    focusedObsOrGroup.flatMap(groupInfo)

  private def groupIsEmpty(groupId: Group.Id): Boolean =
    groupsChildren.get(groupId.some).forall(_.isEmpty)

  private val isCalibObsSelected: Boolean                               =
    selectedObsIdSet
      .map(_.idSet.exists(observations.get.get(_).exists(_.isCalibration)))
      .getOrElse(false)
  private val copyDisabled: Boolean                                     = selectedObsIdSet.isEmpty || isCalibObsSelected
  private val pasteDisabled: Boolean                                    = clipboardObsContents.isEmpty
  private val (deleteDisabled: Boolean, deletedTooltip: Option[String]) =
    selectedObsIdSet
      .map(obsIds =>
        if (observations.get.executedOf(obsIds).nonEmpty)
          (true, "- Cannot delete executed observations.".some)
        else if (isCalibObsSelected)
          (true, "- Cannot delete calibration observations.".some)
        else (false, none)
      )
      .orElse(
        focusedGroupId.map(groupId =>
          if (groupIsEmpty(groupId)) (false, none)
          else (true, "- Cannot delete non-empty groups.".some)
        )
      )
      .getOrElse((true, none))

  private def observationsText(observations: ObsIdSet): String =
    observations.idSet.size match
      case 1    => s"observation ${observations.idSet.head}"
      case more => s"$more observations"
  private def groupText(groupId: Group.Id): String             = s"group $groupId"

  private val copyText: Option[String]                               = selectedObsIdSet.map(observationsText)
  private def selectedText(obsIds: Iterable[Observation.Id]): String =
    obsIds.size match
      case 1    => s"observation ${obsIds.head}"
      case more => s"$more observations"
  private val pasteText: Option[String]                              =
    clipboardObsContents
      .map(_.idSet.toSortedSet)
      .map(selectedText)
      .map(_ + resolvedActiveGroupId.map(gid => s" into ${groupText(gid)}").orEmpty)
  private val deleteText: Option[String]                             =
    selectedObsIdSet.map(observationsText).orElse(focusedGroupId.map(groupText))

  private def createNode(
    value:        Either[Observation, Group],
    dragging:     Option[Either[Observation, Group]],
    dragOverPath: List[Group.Id]
  ): Node[Either[Observation, Group]] =
    Tree.Node(
      Tree.Id(value.fold(_.id.toString, _.id.toString)),
      value,
      children = value.toOption
        // Telluric calibration group children are not shown as tree nodes but within the badge.
        .filterNot(_.isTelluricCalibration)
        .map: group =>
          groupsChildren
            .get(group.id.some)
            .map(_.map(createNode(_, dragging, dragOverPath)))
            .orEmpty
        .orEmpty
    )

  // Root elements are already sorted by index
  private val rootElements: List[Either[Observation, Group]] = groupsChildren.get(none).orEmpty

  private def treeNodes(
    dragging:     Option[Either[Observation, Group]],
    dragOverPath: List[Group.Id]
  ): List[Node[Either[Observation, Group]]] =
    rootElements
      .filter(_.fold(_ => true, g => !g.system || g.isTelluricCalibration))
      .map(createNode(_, dragging, dragOverPath))

  private val systemTreeNodes: List[Node[Either[Observation, Group]]] =
    rootElements
      .filter(_.fold(_ => false, g => g.system && !g.isTelluricCalibration))
      .map(createNode(_, dragging = none, dragOverPath = List.empty))

object ObsTree:
  private type Props = ObsTree

  /**
   * Iso to go between Group.Id and Tree.Id
   */
  private val groupTreeIdLens: Iso[Set[Group.Id], Set[Tree.Id]] =
    Iso[Set[Group.Id], Set[Tree.Id]](_.map(gId => Tree.Id(gId.toString)))(
      _.flatMap(v => Group.Id.parse(v.value))
    )

  private def scrollIfNeeded(targetObs: Observation.Id) =
    Callback {
      Option(dom.document.getElementById(s"obs-list-${targetObs.toString}"))
        .filterNot(js.isUndefined)
        .map { obsListElement =>
          val rect = obsListElement.getBoundingClientRect()
          if (rect.top < 0) obsListElement.scrollIntoView()
          if (rect.bottom > dom.window.innerHeight) obsListElement.scrollIntoView(false)
        }
    }

  extension (node: Either[Observation, Group])
    def id: Either[Observation.Id, Group.Id] = node.bimap(_.id, _.id)

  private val component =
    ScalaFnComponent[Props]: props =>
      // refocus if current focus ceases to exist
      inline def refocus(
        prevGroupInfo: UseRef[Option[(Option[Group.Id], NonNegShort)]],
        ctx:           AppContext[IO]
      ): HookResult[Unit] =
        useEffectWithDeps((props.focusedObsOrGroup, props.observations.get, props.groups.get)):
          (focusedObsOrGroup, obsList, groupList) =>
            focusedObsOrGroup.fold(prevGroupInfo.set(none)): elemId =>
              (prevGroupInfo.value, elemId.fold(obsList.contains(_), groupList.contains(_))) match
                case (_, true)           =>
                  prevGroupInfo.set(props.focusedGroupInfo)
                case (Some(prev), false) => // Previously focused element no longer exists
                  val prevGroup: Option[Group.Id]                    = prev._1
                  val prevIndex: NonNegShort                         = prev._2
                  val newElement: Option[Either[Observation, Group]] =
                    props.groupsChildren
                      .get(prevGroup)
                      .flatMap:
                        _.get(math.max(0, prevIndex.value - 1))
                      .orElse:
                        prevGroup.flatMap(groupList.get(_).map(_.asRight))

                  prevGroupInfo.set:
                    newElement.flatMap(e => props.groupInfo(e.id))
                  >>
                    newElement
                      .fold(focusObs(props.programId, none, ctx)):
                        _.fold(
                          obs => focusObs(props.programId, obs.id.some, ctx),
                          group => focusGroup(props.programId, group.id.some, ctx)
                        )
                case _                   => Callback.empty

      for
        ctx               <- useContext(AppContext.ctx)
        obsAndGroupsRef   <- useShadowRef(props.observationsAndGroups)
        groupsChildrenRef <- useShadowRef(props.groupsChildren)
        expandedGroupsRef <- useShadowRef(props.expandedGroups.get)
        dndScope          <- useDragAndDropScope[Either[Observation, Group], Either[Observation, Group]]()
        autoScrollRef     <- useAutoScrollRef(getAllowedAxis = _ => Axes.Vertical)
        // Saved index into the observation tree
        prevGroupInfo     <- useRef(props.focusedGroupInfo)
        // refocus if current focus ceases to exist
        _                 <- refocus(prevGroupInfo, ctx)
        adding            <- useStateView(AddingObservation(false))
        // Scroll to newly created/selected observation
        _                 <- useEffectWithDeps(props.focusedObsId): focusedObs =>
                               focusedObs.map(scrollIfNeeded).getOrEmpty
        // Open the group (and all super-groups) of the focused observation
        _                 <- useEffectWithDeps(props.resolvedActiveGroupId):
                               _.map: activeGroupId =>
                                 props.expandedGroups.mod:
                                   _ ++ props.parentGroups(activeGroupId.asRight) + activeGroupId
                               .orEmpty
      yield
        import ctx.given

        def onDragDrop(
          payload: BaseEventPayload[Either[Observation, Group], Either[Observation, Group]]
        ): Callback =
          val dropTargetData: Option[Data[Either[Observation, Group]]] =
            payload.location.current.dropTargets.headOption.map(_.data)
          val instructionOpt: Option[Instruction]                      = dropTargetData.flatMap(_.extractInstruction)

          (dropTargetData.map(_.value), instructionOpt).tupled.foldMap: (dropTarget, instruction) =>
            val draggedNode: Either[Observation, Group]         = payload.source.data.value
            val draggedNodeId: Either[Observation.Id, Group.Id] = draggedNode.id
            val isDragIntoGroupHeader: Boolean                  =
              dropTarget.isRight && instruction.operation == Operation.Combine
            val dropGroupId: Option[Group.Id]                   = dropTarget.fold(
              obs => obs.groupId,
              group => if isDragIntoGroupHeader then group.id.some else group.parentId
            )

            val children: List[Either[Observation, Group]] =
              groupsChildrenRef.get
                .runNow()
                .get(dropGroupId)
                .orEmpty
                .filter:
                  case Left(_)      => true
                  case Right(group) => !group.system

            // Group indices may not be continuous. So, we compute position and then the index based on the actual children of the group.
            val newParentGroupPosIndex: Int =
              if isDragIntoGroupHeader then 0
              else
                computeIndexInList[Either[Observation, Group]](
                  _.id === dropTarget.id,
                  instruction.operation
                )(children).getOrElse(0)

            val newParentGroupIndex: NonNegShort =
              if newParentGroupPosIndex < children.length then
                children(newParentGroupPosIndex).fold(_.groupIndex, _.parentIndex)
              else
                children.lastOption
                  .map(_.fold(_.groupIndex, _.parentIndex))
                  .map(i => NonNegShort.unsafeFrom((i.value + 1).toShort))
                  .getOrElse(NonNegShort.unsafeFrom(0))

            draggedNodeId
              .fold(
                obsId =>
                  ObsActions
                    .obsGroupInfo(obsId)
                    .set(obsAndGroupsRef.get.runNow())((dropGroupId, newParentGroupIndex).some),
                groupId =>
                  ObsActions
                    .groupParentInfo(groupId)
                    .set(obsAndGroupsRef.get.runNow())((dropGroupId, newParentGroupIndex).some)
              ) >> // Open the group we moved to
              dropGroupId.map(groupId => props.expandedGroups.mod(_ + groupId)).orEmpty
        end onDragDrop

        val expandedGroups: View[Set[Tree.Id]] = props.expandedGroups.as(groupTreeIdLens)

        val deleteObsList: List[Observation.Id] => Callback =
          selectedObsIds =>
            ConfirmDialog.confirmDialog(
              message = <.div(s"This action will delete ${props.selectedText(selectedObsIds)}."),
              header = "Observations delete",
              acceptLabel = "Yes, delete",
              position = DialogPosition.Top,
              accept = ObsActions
                .obsExistence(selectedObsIds, postMessage = ToastCtx[IO].showToast(_))
                .mod(props.observations)(_ => selectedObsIds.map(_ => none)),
              acceptClass = PrimeStyles.ButtonSmall,
              rejectClass = PrimeStyles.ButtonSmall,
              icon = Icons.SkullCrossBones(^.color.red)
            )

        val deleteGroup: Group.Id => Callback = groupId =>
          ObsActions
            .groupExistence(
              groupId,
              gid => focusGroup(props.programId, gid.some, ctx)
            )
            .set(props.groups)(none)
            .showToastCB(s"Deleted group ${groupId.shortName}")

        // This can be invoked for a single observation or for a telluric group.
        def renderObsCard(
          item:         Either[Observation, Group],
          inSystemTree: Boolean
        ): VdomNode =
          val (mainObs, associatedObss): (Option[Observation], List[Observation]) = item.fold(
            obs => (obs.some, List.empty),
            group =>
              val groupObservations: List[Observation] =
                props.groupsChildren
                  .get(group.id.some)
                  .orEmpty
                  .collect:
                    case Left(obs) => obs
              val mainObs: Option[Observation]         = groupObservations.find(_.calibrationRole.isEmpty)
              val associatedObss: List[Observation]    =
                groupObservations.filterNot(o => mainObs.exists(_.id === o.id))
              (mainObs, associatedObss)
          )

          // If we cannot determine the mainObs, render as a regular group.
          // .toOption.get is safe since it will only happen if we are in a group
          mainObs.fold(renderGroup(item.toOption.get, inSystemTree)) { obs =>
            val selected: Boolean = props.focusedObsId.contains_(obs.id)

            val badge =
              <.a(
                ^.id   := s"obs-list-${item.id.toString}",
                ^.href := ctx.pageUrl(
                  (AppTab.Observations,
                   props.programId,
                   Focused.singleObs(obs.id, props.focusedTargetId)
                  ).some
                ),
                ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                ^.onClick ==> linkOverride(focusObs(props.programId, obs.id.some, ctx))
              )(
                ObsBadge(
                  obs,
                  ObsBadge.Layout.ObservationsTab,
                  selected = selected,
                  setStateCB = (
                    (obsId: Observation.Id) =>
                      ObsActions
                        .obsEditState(obsId)
                        .set(props.observations)
                        .compose((_: ObservationWorkflowState).some)
                  ).some,
                  setSubtitleCB = ObsActions
                    .obsEditSubtitle(obs.id)
                    .set(props.observations)
                    .compose((_: Option[NonEmptyString]).some)
                    .some,
                  deleteCB = deleteObsList(List(obs.id)),
                  cloneCB = cloneObs(
                    props.programId,
                    List(obs.id),
                    props.resolveGroupId(obs.groupId), // Clone to the same group
                    props.observations,
                    ctx
                  ).switching(adding.async, AddingObservation(_))
                    .withToastDuring(s"Duplicating obs ${obs.id}")
                    .runAsync
                    .some,
                  setScienceBandCB = (
                    (b: ScienceBand) =>
                      ObsActions.obsScienceBand(obs.id).set(props.observations)(b.some)
                  ).some,
                  allocatedScienceBands = props.allocatedScienceBands,
                  associatedObss = associatedObss,
                  programId = props.programId,
                  hasBlindOffset = obs.hasBlindOffset,
                  focusedObs = props.focusedObsId,
                  readonly = props.readonly
                )
              )

            React.Fragment(
              DraggableDropTarget[Either[Observation, Group], Either[Observation, Group]](
                badge,
                getInitialData = _ => Data(item),
                getData = args => Data(item).attachInstruction(args, Operations.Reorder),
                canDrag = _ => !inSystemTree,
                canDrop = _ => !inSystemTree,
                onDropTargetDrop = onDragDrop(_)
              ),
              (dndScope.dragOver.headOption.map(_.data), dndScope.dragging.map(_.value)).tupled
                .filter: (dragOverData, draggingDataValue) =>
                  dragOverData.value.id === item.id &&
                    dragOverData.value.id =!= draggingDataValue.id
                .flatMap(_._1.extractInstruction)
                .map: instruction =>
                  ListItemDropIndicator(instruction, lineGap = CssSize("4px"))
            )
          }
        end renderObsCard

        def renderGroup(group: Group, inSystemTree: Boolean): VdomNode =
          val badge =
            <.span(^.width := "100%")(
              GroupBadge(
                group,
                props.groupWarnings.get(group.id),
                selected = props.focusedGroupId.contains_(group.id),
                onClickCB = linkOverride:
                  focusGroup(props.programId, group.id.some, ctx)
                ,
                href = ctx.pageUrl:
                  (AppTab.Observations, props.programId, Focused.group(group.id)).some
                ,
                deleteCB = deleteGroup(group.id),
                isEmpty = props.groupIsEmpty(group.id),
                readonly = props.readonly || group.system
              )
            )

          React.Fragment(
            DraggableDropTarget[Either[Observation, Group], Either[Observation, Group]](
              badge,
              getInitialData = _ => Data(group.asRight),
              getData = args =>
                Data(group.asRight).attachInstruction(
                  args, // Avoid capture of exandedGroups (getData is invoked repeatedly when dragging)
                  if !expandedGroupsRef.get.runNow().contains(group.id) then Operations.All
                  else Operations.ReorderOnExpandedGroup
                ),
              // Telluric calibration groups are rendered as observations, which can be dragged.
              canDrag = _ => !inSystemTree || group.isTelluricCalibration,
              canDrop = _ => !inSystemTree && !group.isTelluricCalibration,
              onDropTargetDrop = onDragDrop(_)
            ),
            (dndScope.dragOver.headOption.map(_.data), dndScope.dragging.map(_.value)).tupled
              .filter: (dragOverData, draggingDataValue) =>
                dragOverData.value.id === group.id.asRight &&
                  dragOverData.value.id =!= draggingDataValue.id
              .flatMap(_._1.extractInstruction)
              .map: instruction =>
                ListItemDropIndicator(instruction, lineGap = CssSize("4px"))
          )
        end renderGroup

        def renderItem(nodeValue: Either[Observation, Group], inSystemTree: Boolean): VdomNode =
          nodeValue match
            case Right(group) if group.isTelluricCalibration =>
              renderObsCard(group.asRight, inSystemTree)
            case Right(group)                                => renderGroup(group, inSystemTree)
            case Left(obs)                                   => renderObsCard(obs.asLeft, inSystemTree)

        val expandFocusedGroup: Callback = props.expandedGroups.mod(_ ++ props.focusedGroupId)

        val isSystemGroupFocused: Boolean =
          props.resolvedActiveGroupId
            .flatMap(props.groups.get.get(_))
            .exists(g => g.system && !g.isTelluricCalibration)

        val tree: VdomNode =
          if (props.deckShown.get === DeckShown.Shown) {
            React.Fragment(
              <.div(ExploreStyles.TreeToolbar)(
                React
                  .Fragment(
                    <.span(
                      Button(
                        severity = Button.Severity.Success,
                        icon = Icons.New,
                        label = "Obs",
                        disabled = adding.get.value || isSystemGroupFocused,
                        loading = adding.get.value,
                        tooltip = "Add a new Observation",
                        tooltipOptions = DefaultTooltipOptions,
                        onClick = insertObs(
                          props.programId,
                          props.resolvedActiveGroupId, // Set the active group as the new obs parent if it is selected
                          props.observations,
                          adding,
                          ctx
                        ).runAsync *> expandFocusedGroup
                      ).mini.compact,
                      Button(
                        severity = Button.Severity.Success,
                        icon = Icons.New,
                        label = "Group",
                        disabled = adding.get.value || isSystemGroupFocused,
                        loading = adding.get.value,
                        tooltip = "Add a new Group",
                        tooltipOptions = DefaultTooltipOptions,
                        onClick = insertGroup(
                          props.programId,
                          props.resolvedActiveGroupId, // Set the active group as the new group parent if it is selected
                          props.groups,
                          adding,
                          ctx
                        ).runAsync *> expandFocusedGroup
                      ).mini.compact
                    ),
                    UndoButtons(props.undoer, size = PlSize.Mini, disabled = adding.get.value),
                    ActionButtons(
                      ActionButtons.ButtonProps(
                        props.copyCallback,
                        disabled = props.copyDisabled,
                        tooltipExtra = props.copyText
                      ),
                      ActionButtons.ButtonProps(
                        props.pasteCallback,
                        disabled = props.pasteDisabled,
                        tooltipExtra = props.pasteText
                      ),
                      ActionButtons.ButtonProps(
                        props.selectedObsIdSet
                          .map(obsIdSet => deleteObsList(obsIdSet.idSet.toList))
                          .orElse(props.focusedGroupId.map(deleteGroup))
                          .orEmpty,
                        disabled = props.deleteDisabled,
                        tooltipExtra = props.deletedTooltip.orElse(props.deleteText)
                      )
                    )
                  )
                  .unless(props.readonly),
                Button(
                  severity = Button.Severity.Secondary,
                  outlined = true,
                  disabled = false,
                  tooltip = "Hide Observation Tree",
                  tooltipOptions = DefaultTooltipOptions,
                  icon = Icons.ArrowLeftFromLine,
                  clazz = ExploreStyles.ObsTreeHideShow,
                  onClick = props.deckShown.mod(_.flip)
                ).mini.compact
              ),
              <.div(
                Button(
                  severity = Button.Severity.Secondary,
                  icon = Icons.ListIcon,
                  label = "Observations Summary",
                  onClick = focusObs(props.programId, none, ctx) >> props.setSummaryPanel,
                  clazz = ExploreStyles.ButtonSummary
                )
              ),
              <.div(^.overflow := "auto", ^.untypedRef := autoScrollRef)(
                Tree(
                  props.treeNodes(
                    dndScope.dragging.map(_.value),
                    dndScope.dragOver.headOption
                      .map(_.data.value.id)
                      .foldMap: dragOverIds =>
                        props.parentGroups(dragOverIds) ++ dragOverIds.toOption
                  ),
                  (n, _) => renderItem(n, inSystemTree = false),
                  expandedKeys = expandedGroups.get --
                    dndScope.dragging.flatMap(_.value.toOption.map(g => Tree.Id(g.id.toString))),
                  onToggle = expandedGroups.set
                ),
                Tree(
                  props.systemTreeNodes,
                  (n, _) => renderItem(n, inSystemTree = true),
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set
                )
              )
            )
          } else EmptyVdom

        dndScope.context(<.div(ExploreStyles.ObsTreeWrapper)(tree))
