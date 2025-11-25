// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.BlindOffset
import explore.model.EmptyOpportunityTarget
import explore.model.EmptySiderealTarget
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.TargetList
import explore.services.OdbAsterismApi
import explore.services.OdbObservationApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import lucuma.catalog.clients.SimbadClient
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.react.common.Css
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.SplitButton
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.ui.primereact.*
import org.typelevel.log4cats.Logger

trait AsterismModifier:

  protected def insertTarget(
    programId:        Program.Id,
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    targetWithOptId:  TargetWithOptId,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(using odbApi: OdbTargetApi[IO] & OdbAsterismApi[IO]): IO[Unit] =
    targetWithOptId.optId
      .fold(
        odbApi
          .insertTarget(programId, targetWithOptId.target)
          .map((_, true))
      )(id => IO((id, false)))
      .flatMap((id, created) =>
        (AsterismActions
          .addTargetToAsterisms(
            targetWithOptId.withId(id),
            obsIds,
            created,
            onAsterismUpdate
          )
          .set(obsAndTargets)(false) >>
          // Do the first onAsterismUpdate here so it is synchronous with the setter in the Action.
          // the ".async.toCallback" seems to let the model update before we try changing the UI
          onAsterismUpdate(
            OnAsterismUpdateParams(id, obsIds, true, true)
          ).async.toCallback).toAsync
      )

  protected def insertManualBlindOffset(
    obsId:           Observation.Id,
    targets:         View[TargetList], // does not participate in undo/redo
    targetWithOptId: TargetWithOptId,
    blindOffset:     Option[View[BlindOffset]]
  )(using api: OdbObservationApi[IO]): IO[Unit] =
    api
      .setBlindOffsetTarget(obsId, targetWithOptId.target, BlindOffsetType.Manual)
      .flatMap(id =>
        (targets.mod(_.updated(id, targetWithOptId.withId(id))) >>
          blindOffset.foldMap(_.set(BlindOffset(true, id.some, BlindOffsetType.Manual)))).toAsync
      )

  def targetSelectionPopup(
    label:            String,
    programId:        Program.Id,
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    adding:           View[AreAdding],
    onAsterismUpdate: OnAsterismUpdateParams => Callback,
    readOnly:         Boolean = false,
    buttonClass:      Css = Css.Empty,
    client:           SimbadClient[IO],
    blindOffset:      Option[View[BlindOffset]] = none
  )(using
    odbApi:           OdbTargetApi[IO] & OdbAsterismApi[IO] & OdbObservationApi[IO]
  )(using
    Logger[IO]
  ): TargetSelectionPopup =
    def insertTargetCB(targetWithOptId: TargetWithOptId): Callback =
      insertTarget(programId, obsIds, obsAndTargets, targetWithOptId, onAsterismUpdate)
        .switching(adding.async, AreAdding(_))
        .runAsync

    val menuItems = List(
      MenuItem.Item("Empty Sidereal Target",
                    icon = Icons.Star,
                    command = insertTargetCB(TargetWithOptId.newScience(EmptySiderealTarget))
      ),
      MenuItem.Item("Empty Target of Opportunity",
                    icon = Icons.HourglassClock,
                    command = insertTargetCB(TargetWithOptId.newScience(EmptyOpportunityTarget))
      )
    ) ++
      blindOffset
        .filterNot(_.get.useBlindOffset)
        .map(_ =>
          MenuItem.Item(
            "Manual Blind Offset",
            icon = Icons.Star,
            command = insertManualBlindOffset(
              obsIds.head,
              obsAndTargets.zoom(ObservationsAndTargets.targets).model,
              TargetWithOptId(None, EmptySiderealTarget, TargetDisposition.BlindOffset, None),
              blindOffset
            ).switching(adding.async, AreAdding(_)).runAsync
          )
        )
        .toList

    def triggerButton(openPopup: Callback) =
      SplitButton(
        model = menuItems,
        onClick = openPopup,
        severity = Button.Severity.Success,
        icon = Icons.New,
        disabled = readOnly || adding.get.value,
        loading = adding.get.value,
        label = label,
        clazz = buttonClass |+| ExploreStyles.Hidden.when_(readOnly)
      ).tiny.compact

    TargetSelectionPopup(
      "Add Target",
      TargetSource.FromProgram[IO](obsAndTargets.get._2) ::
        TargetSource.forAllCatalogs[IO](client),
      selectExistingLabel = "Link",
      selectExistingIcon = Icons.Link,
      selectNewLabel = "Add",
      selectNewIcon = Icons.New,
      trigger = triggerButton,
      onSelected = insertTargetCB
    )
