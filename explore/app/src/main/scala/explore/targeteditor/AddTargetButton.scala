// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.BlindOffset
import explore.model.EmptyOpportunityTarget
import explore.model.EmptySiderealTarget
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.PopupState
import explore.model.TargetList
import explore.services.OdbAsterismApi
import explore.services.OdbObservationApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.SplitButton
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.ui.primereact.*

case class AddTargetButton(
  label:            String,
  programId:        Program.Id,
  obsIds:           ObsIdSet,
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  adding:           View[AreAdding],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  readOnly:         Boolean = false,
  buttonClass:      Css = Css.Empty,
  blindOffset:      Option[View[BlindOffset]] = none
) extends ReactFnProps(AddTargetButton)

object AddTargetButton
    extends ReactFnComponent[AddTargetButton](props =>
      def insertTarget(
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

      def insertManualBlindOffset(
        obsId:           Observation.Id,
        targets:         View[TargetList], // does not participate in undo/redo
        targetWithOptId: TargetWithOptId,
        blindOffset:     Option[View[BlindOffset]]
      )(using api: OdbObservationApi[IO]): IO[Unit] =
        api
          .setBlindOffsetTarget(obsId, targetWithOptId.target, BlindOffsetType.Manual)
          .flatMap(id =>
            (targets.mod(_.updated(id, targetWithOptId.withId(id))) >>
              blindOffset
                .foldMap(_.set(BlindOffset(true, id.some, BlindOffsetType.Manual)))).toAsync
          )

      for
        ctx        <- useContext(AppContext.ctx)
        popupState <- useStateView(PopupState.Closed)
        onSelected <- useStateView((_: TargetWithOptId) => Callback.empty)
        sources    <- useStateView:
                        // we'll always set this before opening the popup
                        NonEmptyList
                          .one[TargetSource[IO]](
                            TargetSource.FromProgram[IO](props.obsAndTargets.get._2)
                          )
      yield
        import ctx.given

        def insertTargetCB(targetWithOptId: TargetWithOptId): Callback =
          insertTarget(props.programId,
                       props.obsIds,
                       props.obsAndTargets,
                       targetWithOptId,
                       props.onAsterismUpdate
          )
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        def insertManualBlindOffsetCB(
          targetWithOptId: TargetWithOptId
        ): Callback =
          // the search returns a science target
          val targetAsBlind = targetWithOptId.copy(disposition = TargetDisposition.BlindOffset)
          insertManualBlindOffset(
            props.obsIds.head,
            props.obsAndTargets.zoom(ObservationsAndTargets.targets).model,
            targetAsBlind,
            props.blindOffset
          )
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        val programsAndSimbad = NonEmptyList.of(
          TargetSource.FromProgram[IO](props.obsAndTargets.get._2),
          TargetSource.FromSimbad[IO](ctx.simbadClient)
        )

        val simbad = NonEmptyList.one(
          TargetSource.FromSimbad[IO](ctx.simbadClient)
        )

        // TODO: NONSIDEREAL Uncomment when we have a proxy for Horizons
        // val horizons = NonEmptyList.one(
        //   TargetSource.FromHorizons[IO](ctx.horizonsClient)
        // )

        val menuItems = List(
          // TODO: NONSIDEREAL Uncomment when we have a proxy for Horizons
          // MenuItem.Item("Non-Sidereal Target Search",
          //               icon = Icons.PlanetRinged,
          //               command = onSelected.set(insertTargetCB) >>
          //                 sources.set(horizons) >> popupState.set(PopupState.Open)
          // ),
          MenuItem.Item("Empty Sidereal Target",
                        icon = Icons.Star,
                        command = insertTargetCB(TargetWithOptId.newScience(EmptySiderealTarget))
          ),
          MenuItem.Item("Empty Target of Opportunity",
                        icon = Icons.HourglassClock,
                        command = insertTargetCB(TargetWithOptId.newScience(EmptyOpportunityTarget))
          )
        ) ++
          props.blindOffset
            .filterNot(_.get.useBlindOffset)
            .fold(List.empty)(_ =>
              List(
                MenuItem.Item(
                  "Blind Offset Search",
                  icon = Icons.LocationDot,
                  command = onSelected.set(insertManualBlindOffsetCB) >>
                    sources.set(simbad) >> popupState.set(PopupState.Open)
                ),
                MenuItem.Item(
                  "Empty Blind Offset",
                  icon = Icons.LocationDot,
                  command = insertManualBlindOffsetCB(
                    TargetWithOptId(None, EmptySiderealTarget, TargetDisposition.BlindOffset, None)
                  )
                )
              )
            )

        React.Fragment(
          SplitButton(
            model = menuItems,
            onClick = onSelected.set(insertTargetCB) >> sources.set(programsAndSimbad) >>
              popupState.set(PopupState.Open),
            severity = Button.Severity.Success,
            icon = Icons.New,
            disabled = props.readOnly || props.adding.get.value,
            loading = props.adding.get.value,
            label = props.label,
            clazz = props.buttonClass |+| ExploreStyles.Hidden.when_(props.readOnly)
          ).tiny.compact,
          TargetSelectionPopup(
            "Add Target",
            popupState,
            sources.get,
            selectExistingLabel = "Link",
            selectExistingIcon = Icons.Link,
            selectNewLabel = "Add",
            selectNewIcon = Icons.New,
            onSelected = onSelected.get
          )
        )
    )
