// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.components.TileContents
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.GuideStarSelection
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetTabTileIds
import explore.model.UserPreferences
import explore.targeteditor.TargetEditor
import explore.targeteditor.UseTrackingMap.useTrackingMap
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.enums.Site
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.model.TargetWithId
import lucuma.ui.undo.UndoSetter

import java.time.Instant
import java.time.temporal.ChronoUnit

final case class SingleTargetEditorTile(
  programId:          Program.Id,
  programType:        ProgramType,
  userId:             Option[User.Id],
  target:             UndoSetter[TargetWithId],
  obsAndTargets:      UndoSetter[ObservationsAndTargets],
  searching:          View[Set[Target.Id]],
  titleText:          String,
  fullScreen:         View[AladinFullScreen],
  userPreferences:    View[UserPreferences],
  guideStarSelection: View[GuideStarSelection],
  attachments:        View[AttachmentList],
  authToken:          Option[NonEmptyString],
  readonly:           Boolean,
  isStaffOrAdmin:     Boolean,
  obsInfo:            TargetEditObsInfo,
  onClone:            OnCloneParameters => Callback,
  site:               Option[Site],
  backButton:         Option[VdomNode] = none
) extends Tile[SingleTargetEditorTile](
      TargetTabTileIds.AsterismEditor.id,
      titleText,
      renderBackButton = backButton,
      bodyClass = ExploreStyles.TargetTileBody
    )(SingleTargetEditorTile)

object SingleTargetEditorTile
    extends TileComponent[SingleTargetEditorTile]((props, _) =>
      for
        ctx         <- useContext(AppContext.ctx)
        // Same default TargetEditor falls back to when there is no observation time.
        obsTime     <- useMemo(())(_ => Instant.now().truncatedTo(ChronoUnit.DAYS))
        trackingMap <- useTrackingMap(
                         ObservationTargets.one(props.target.get).some,
                         props.site,
                         obsTime.value.some
                       )(ctx)
      yield TileContents:
        <.div(
          ExploreStyles.AladinFullScreen.when(props.fullScreen.get.value),
          <.div(
            ExploreStyles.TargetTileEditor,
            props.userId.map(uid =>
              TargetEditor(
                props.programId,
                props.programType,
                uid,
                props.target,
                props.obsAndTargets,
                ObservationTargets.one(props.target.get),
                obsTime = obsTime.value.some,
                obsConf = none,
                trackingMap = trackingMap,
                searching = props.searching,
                obsInfo = props.obsInfo,
                onClone = props.onClone,
                fullScreen = props.fullScreen,
                userPreferences = props.userPreferences,
                guideStarSelection = props.guideStarSelection,
                attachments = props.attachments,
                authToken = props.authToken,
                readonly = props.readonly,
                // don't allow this when editing non-specifically for an observation
                allowEditingOngoing = false,
                isStaffOrAdmin = props.isStaffOrAdmin
              )
            )
          )
        )
    )
