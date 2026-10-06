// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import explore.ObsGroupHelper
import explore.model.AppContext
import explore.model.Attachment
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.services.OdbObservationApi
import japgolly.scalajs.react.*
import lucuma.core.enums.CloneSequenceMode
import lucuma.core.model.Program
import lucuma.core.util.NewBoolean
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.effect.*
import lucuma.ui.syntax.toast.*
import lucuma.ui.undo.UndoSetter

def focusObs[F[_]](
  programId: Program.Id,
  obsId:     Option[Observation.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage:
    (AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_))).some

def focusGroup[F[_]](
  programId: Program.Id,
  groupId:   Option[Group.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage:
    (AppTab.Observations, programId, groupId.fold(Focused.None)(Focused.group(_))).some

def cloneObs(
  programId:    Program.Id,
  obsIds:       List[Observation.Id],
  newGroupId:   Option[Group.Id],
  observations: UndoSetter[ObservationList],
  ctx:          AppContext[IO],
  sequenceCopy: CloneSequenceMode = CloneSequenceMode.None
): IO[Unit] =
  import ctx.given

  ObsActions
    .cloneObservations(
      obsIds,
      newGroupId,
      sequenceCopy,
      focusObs = obsId => focusObs(programId, obsId.some, ctx),
      postMessage = ToastCtx[IO].showToast(_)
    )(observations)
    .void

/**
 * Duplicates `obs` right away when there is nothing to ask about its sequence. Otherwise sets
 * `duplicating` to `pending`, which opens the `SequenceCopyDialog`.
 */
def requestDuplicate[A](
  obs:         Observation,
  duplicating: View[Option[A]],
  pending:     A
)(duplicate: CloneSequenceMode => Callback): Callback =
  if obs.execution.sequenceCopyChoices.isEmpty then duplicate(CloneSequenceMode.None)
  else duplicating.set(pending.some)

/**
 * Duplicates `obs` into its own program group, for tabs other than Observations. `focusClone` runs
 * when the clone is created or redone, `onRemoved` when it is undone.
 */
def duplicateObs(
  obs:          Observation,
  sequenceCopy: CloneSequenceMode,
  observations: UndoSetter[ObservationList],
  groups:       GroupList,
  focusClone:   Observation.Id => Callback,
  onRemoved:    Callback,
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  ObsActions
    .cloneObservations(
      List(obs.id),
      ObsGroupHelper.resolveGroupId(groups, obs.groupId),
      sequenceCopy,
      focusObs = focusClone,
      postMessage = ToastCtx[IO].showToast(_),
      onRemoved = onRemoved
    )(observations)
    .void
    .withToastDuring(s"Duplicating obs ${obs.id}")

def obsEditAttachments(
  obsId:         Observation.Id,
  attachmentIds: Set[Attachment.Id]
)(using
  odbApi:        OdbObservationApi[IO]
): IO[Unit] =
  odbApi.updateObservations(
    List(obsId),
    ObservationPropertiesInput(attachments = attachmentIds.toList.assign)
  )

object AddingObservation extends NewBoolean
type AddingObservation = AddingObservation.Type

def insertObs(
  programId:    Program.Id,
  parentId:     Option[Group.Id],
  observations: UndoSetter[ObservationList],
  adding:       View[AddingObservation],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  ObsActions
    .insertObservation(
      programId,
      parentId,
      focusObs = obsId => focusObs(programId, obsId.some, ctx),
      postMessage = ToastCtx[IO].showToast(_)
    )(observations)
    .void
    .switching(adding.as(AddingObservation.Value).async)
    .withToastDuring("Creating observation")

def insertGroup(
  programId: Program.Id,
  parentId:  Option[Group.Id],
  groups:    UndoSetter[GroupList],
  adding:    View[AddingObservation],
  ctx:       AppContext[IO]
): IO[Unit] =
  import ctx.given

  ctx.odbApi
    .createGroup(programId, parentId)
    .flatMap: group =>
      ObsActions
        .groupExistence(group.id, g => focusGroup(programId, g.some, ctx))
        .set(groups)(group.some)
        .toAsync
    .void
    .switching(adding.as(AddingObservation.Value).async)
