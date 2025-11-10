// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.BlindOffset
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
import explore.targets.TargetColumns
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.table.ColumnVisibility
import lucuma.schemas.model.TargetWithId
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Iso
import monocle.Lens
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.collection.immutable.SortedSet
import cats.Order.given

object AsterismEditorTile:
  def apply(
    userId:              Option[User.Id],
    tileId:              Tile.TileId,
    programId:           Program.Id,
    obsIds:              ObsIdSet,
    obsAndTargets:       UndoSetter[ObservationsAndTargets],
    obsTime:             View[Option[Instant]],
    obsDuration:         View[Option[TimeSpan]],
    obsConf:             ObsConfiguration,
    digest:              CalculatedValue[Option[ExecutionDigest]],
    currentTarget:       Option[Target.Id],
    setTarget:           (Option[Target.Id], SetRouteVia) => Callback,
    onCloneTarget:       OnCloneParameters => Callback,
    onAsterismUpdate:    OnAsterismUpdateParams => Callback,
    obsInfo:             Target.Id => TargetEditObsInfo,
    searching:           View[Set[Target.Id]],
    title:               String,
    userPreferences:     View[UserPreferences],
    guideStarSelection:  View[GuideStarSelection],
    attachments:         View[AttachmentList],
    authToken:           Option[NonEmptyString],
    readonly:            Boolean,
    allowEditingOngoing: Boolean,
    sequenceChanged:     Callback = Callback.empty,
    blindOffset:         Option[View[BlindOffset]] = None,
    backButton:          Option[VdomNode] = None
  )(using odbApi: OdbObservationApi[IO])(using Logger[IO]): Tile[TileState] = {
    // Save the time here. this works for the obs and target tabs
    // It's OK to save the viz time for executed observations, I think.
    val obsTimeView: View[Instant] =
      View(
        obsTime.get.getOrElse(Instant.now),
        (f, cb) =>
          val oldValue = obsTime.get
          val newValue = f(oldValue.getOrElse(Instant.now)).some
          Callback.log(s"set time from: $oldValue to: $newValue") >>
            obsTime.set(newValue) >> cb(oldValue.getOrElse(Instant.now),
                                        newValue.getOrElse(Instant.now)
            )
      ).withOnMod(ct =>
        Callback
          .log(s"to the db $ct") *> odbApi.updateVisualizationTime(obsIds.toList, ct.some).runAsync
      )

    val obsDurationView: View[Option[TimeSpan]] =
      obsDuration.withOnMod: t =>
        odbApi.updateVisualizationDuration(obsIds.toList, t).runAsync

    val obsTimeAndDurationView: View[(Instant, Option[TimeSpan])] =
      View(
        (obsTime.get.getOrElse(Instant.now), obsDuration.get),
        (mod, cb) =>
          val oldValue = (obsTime.get.getOrElse(Instant.now), obsDuration.get)
          val newValue = mod(oldValue)
          obsTime.set(newValue._1.some) >> obsDuration.set(newValue._2) >> cb(oldValue, newValue)
      ).withOnMod: tuple =>
        odbApi
          .updateVisualizationTimeAndDuration(obsIds.toList, tuple._1.some, tuple._2)
          .runAsync

    Tile(
      tileId,
      title,
      TileState.Initial,
      back = backButton,
      bodyClass = ExploreStyles.TargetTileBody,
      controllerClass = ExploreStyles.TargetTileController
    )(
      tileState =>
        userId.map: uid =>
          Body(
            programId,
            uid,
            obsIds,
            obsAndTargets,
            obsTime.get,
            obsConf,
            currentTarget,
            setTarget,
            onCloneTarget,
            onAsterismUpdate,
            obsInfo,
            searching,
            userPreferences,
            guideStarSelection,
            attachments,
            authToken,
            readonly,
            allowEditingOngoing,
            sequenceChanged,
            blindOffset,
            tileState.zoom(TileState.columnVisibility),
            tileState.zoom(TileState.obsEditInfo)
          ),
      (tileState, tileSize) =>
        Title(
          programId,
          obsIds,
          obsAndTargets,
          onAsterismUpdate,
          readonly,
          obsTimeView,
          obsDurationView,
          obsTimeAndDurationView,
          digest,
          tileState.zoom(TileState.columnVisibility),
          tileState.get.obsEditInfo,
          blindOffset,
          tileSize
        )
    )
  }

  case class TileState(
    columnVisibility: ColumnVisibility,
    obsEditInfo:      Option[ObsIdSetEditInfo]
  )

  object TileState:
    val Initial: TileState = TileState(TargetColumns.DefaultVisibility, none)

    val columnVisibility: Lens[TileState, ColumnVisibility]    =
      Focus[TileState](_.columnVisibility)
    val obsEditInfo: Lens[TileState, Option[ObsIdSetEditInfo]] =
      Focus[TileState](_.obsEditInfo)

  private case class Body(
    programId:           Program.Id,
    userId:              User.Id,
    obsIds:              ObsIdSet,
    obsAndTargets:       UndoSetter[ObservationsAndTargets],
    obsTime:             Option[Instant],
    obsConf:             ObsConfiguration,
    focusedTargetId:     Option[Target.Id],
    setTarget:           (Option[Target.Id], SetRouteVia) => Callback,
    onCloneTarget:       OnCloneParameters => Callback,
    onAsterismUpdate:    OnAsterismUpdateParams => Callback,
    obsInfo:             Target.Id => TargetEditObsInfo,
    searching:           View[Set[Target.Id]],
    userPreferences:     View[UserPreferences],
    guideStarSelection:  View[GuideStarSelection],
    attachments:         View[AttachmentList],
    authToken:           Option[NonEmptyString],
    readonly:            Boolean,
    allowEditingOngoing: Boolean,
    sequenceChanged:     Callback,
    blindOffset:         Option[View[BlindOffset]], // only pass for a single observation
    columnVisibility:    View[ColumnVisibility],
    obsEditInfo:         View[Option[ObsIdSetEditInfo]]
  ) extends ReactFnProps(Body):
    val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)

  private object Body
      extends ReactFnComponent[Body](props =>
        for
          obsEditInfo <- useMemo((props.obsIds, props.obsAndTargets.get._1)):
                           ObsIdSetEditInfo.fromObservationList
          _           <- useLayoutEffectWithDeps(obsEditInfo): roei =>
                           props.obsEditInfo.set(roei.value.some)
          asterismIds <- useMemo((props.obsIds, props.obsAndTargets.get._1)): (ids, obses) =>
                           // all of the selected observations must have the same asterism
                           obses.get(ids.head).fold(SortedSet.empty[Target.Id])(_.scienceTargetIds)
          // Build asterism IDs that include blind offset
          targetIds   <- useMemo(
                           (asterismIds, props.blindOffset.flatMap(_.get.blindOffsetTargetId))
                         ): (scienceIds, oBlindId) =>
                           // Include blind offset target in the IDs if present
                           scienceIds.value ++ oBlindId.toList
          allTargets  <- useMemo(targetIds): ids =>
                           ObservationTargets.fromIdsAndTargets(ids.value, props.allTargets.get)
          _           <- useLayoutEffectWithDeps((targetIds.value.toList, props.focusedTargetId)):
                           (asterismIds, focusedTargetId) =>
                             // If the selected targetId is None, or not in the target list, select the first target (if any).
                             // Need to replace history here.
                             focusedTargetId.filter(asterismIds.contains) match
                               case None =>
                                 props.setTarget(asterismIds.headOption, SetRouteVia.HistoryReplace)
                               case _    => Callback.empty
          fullScreen  <- useStateView(AladinFullScreen.Normal)
        yield
          val selectedTargetView: View[Option[Target.Id]] =
            View(
              props.focusedTargetId,
              (mod, cb) =>
                val oldValue = props.focusedTargetId
                val newValue = mod(props.focusedTargetId)
                props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(oldValue, newValue)
            )

          val editWarningMsg: Option[String] =
            if (obsEditInfo.allAreExecuted)
              if (obsEditInfo.editing.length > 1)
                "All of the current observations are executed. Asterism is readonly.".some
              else "The current observation has been executed. Asterism is readonly.".some
            else if (obsEditInfo.executed.isDefined)
              "Adding and removing targets will only affect the unexecuted observations.".some
            else none

          <.div(ExploreStyles.AladinFullScreen.when(fullScreen.get.value))(
            editWarningMsg.map(msg => <.div(ExploreStyles.SharedEditWarning, msg)),
            // the 'getOrElse doesn't matter. Controls will be readonly if all are executed
            props.obsEditInfo.get
              .map(_.unExecuted.getOrElse(props.obsIds))
              .map: unexecutedObs =>
                TargetTable(
                  props.userId.some,
                  props.programId,
                  unexecutedObs,
                  allTargets,
                  props.obsAndTargets,
                  selectedTargetView,
                  props.onAsterismUpdate,
                  props.obsTime,
                  fullScreen.get,
                  props.readonly || obsEditInfo.allAreExecuted,
                  props.blindOffset,
                  props.columnVisibility
                ),
            allTargets.flatMap: a =>
              props.focusedTargetId.map: focusedTargetId =>
                val selectedTargetOpt: Option[UndoSetter[TargetWithId]] =
                  props.allTargets.zoom(Iso.id[TargetList].index(focusedTargetId))
                val obsInfo                                             = props.obsInfo(focusedTargetId)

                // Always render the container to prevent layout shift when selecting targets
                <.div(
                  ExploreStyles.TargetTileEditor,
                  selectedTargetOpt.map: targetWithId =>
                    TargetEditor(
                      props.programId,
                      props.userId,
                      targetWithId,
                      props.obsAndTargets,
                      a.focusOn(focusedTargetId),
                      props.obsTime,
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
                      invalidateSequence = props.sequenceChanged
                    )
                ).some
          )
      )

  private case class Title(
    programId:              Program.Id,
    obsIds:                 ObsIdSet,
    obsAndTargets:          UndoSetter[ObservationsAndTargets],
    onAsterismUpdate:       OnAsterismUpdateParams => Callback,
    readonly:               Boolean,
    obsTimeView:            View[Instant],
    obsDurationView:        View[Option[TimeSpan]],
    obsTimeAndDurationView: View[(Instant, Option[TimeSpan])],
    digest:                 CalculatedValue[Option[ExecutionDigest]],
    columnVisibility:       View[ColumnVisibility],
    obsEditInfo:            Option[ObsIdSetEditInfo],
    blindOffset:            Option[View[BlindOffset]],
    tileSize:               TileSizeState
  ) extends ReactFnProps(Title.component)

  private object Title extends AsterismModifier:
    private type Props = Title

    private val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx    <- useContext(AppContext.ctx)
          adding <- useStateView(AreAdding(false))
        yield
          import ctx.given

          val obsTimeEditor = ObsTimeEditor(
            props.obsTimeView,
            props.obsDurationView,
            props.obsTimeAndDurationView,
            props.digest,
            props.obsIds.size > 1
          )

          <.div(
            ExploreStyles.AsterismEditorTileTitle,
            if (props.tileSize.isMinimized)
              obsTimeEditor
            else
              React.Fragment(
                // only pass in the unexecuted observations. Will be readonly if there aren't any
                <.span(
                  (props.obsEditInfo, props.obsEditInfo.map(_.unExecuted.getOrElse(props.obsIds)))
                    .mapN: (obsEditInfo, unexecutedObs) =>
                      targetSelectionPopup(
                        "Target",
                        props.programId,
                        unexecutedObs,
                        props.obsAndTargets,
                        adding,
                        props.onAsterismUpdate,
                        props.readonly || obsEditInfo.allAreExecuted,
                        buttonClass = ExploreStyles.AddTargetButton,
                        blindOffset = props.blindOffset
                      )
                ),
                obsTimeEditor,
                <.span(^.textAlign.right)(
                  ColumnSelectorInTitle(TargetColumns.AllColNames.toList, props.columnVisibility)
                )
              )
          )
