// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
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
import explore.model.Observation
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.PopupState
import explore.model.TargetList
import explore.model.enums.TargetType
import explore.services.OdbAsterismApi
import explore.services.OdbObservationApi
import explore.services.OdbTargetApi
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.LayeredIcon
import lucuma.react.fa.Transform
import lucuma.react.primereact.Button
import lucuma.react.primereact.Icon
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.SplitButton
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.effect.*
import lucuma.ui.undo.UndoSetter
import org.typelevel.log4cats.Logger

case class AddSkyInfo(
  slot:    Option[SlotId],       // The slot the sky position goes into
  mode:    View[Option[SlotId]], // The active "add sky" mode
  enabled: Boolean               // Whether the sky can currently be assigned.
):
  // Key summarizing the fields that affect menu content
  def menuKey: (Option[SlotId], Boolean, Boolean) = (slot, mode.get.isDefined, enabled)

case class AddTargetButton(
  label:            String,
  programId:        Program.Id,
  obsIds:           ObsIdSet,
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  adding:           View[AreAdding],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  readOnly:         Boolean = false,
  allowBlindOffset: Boolean = false, // will be staff only for Ongoing
  buttonClass:      Css = Css.Empty,
  blindOffsetInfo:  Option[(Observation.Id, View[BlindOffset])] = none,
  addSkyInfo:       Option[AddSkyInfo] = none
) extends ReactFnProps(AddTargetButton):
  val targetList: View[TargetList] = obsAndTargets.model.zoom(ObservationsAndTargets.targets)

object AddTargetButton
    extends ReactFnComponent[AddTargetButton](props =>

      def updateTargetList(
        toDelete: Option[Target.Id],
        toAdd:    Option[TargetWithId]
      ): Callback =
        props.targetList.mod: tl =>
          val removed = toDelete.fold(tl)(tl.removed)
          toAdd.fold(removed)(a => removed.updated(a.id, a))

      def updateBlindOffset(
        blindOffset: View[BlindOffset],
        next:        BlindOffset,
        toAdd:       Option[TargetWithId]
      )(using Logger[IO]): IO[Unit] =
        blindOffset.async
          .modAndExtract(prev => (next, prev.blindOffsetTargetId))
          .flatMap(prevId => updateTargetList(prevId, toAdd).toAsync)

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
        targetWithOptId: TargetWithOptId,
        blindOffset:     View[BlindOffset]
      )(using api: OdbObservationApi[IO], logger: Logger[IO]): IO[Unit] =
        api
          .setBlindOffsetTarget(obsId, targetWithOptId.target, BlindOffsetType.Manual)
          .flatMap(id =>
            updateBlindOffset(
              blindOffset,
              BlindOffset(true, id.some, BlindOffsetType.Manual),
              targetWithOptId.withId(id).some
            )
          )

      case class Action(
        label:    String,
        icon:     Icon,
        command:  Callback,
        disabled: Boolean = false
      ):
        def toMenuItem: MenuItem                       =
          MenuItem.Item(label, icon = icon, command = command, disabled = disabled)
        def toButton(initialOnClick: Callback): Button =
          Button(label,
                 icon = icon,
                 onClick = initialOnClick >> command,
                 disabled = disabled
          ).tiny.compact

      // The reusable menu content built by `menuItems`, it was a hard to read tuple before
      case class MenuContent(
        menuActions:        List[Action], // full SplitButton dropdown
        popupActions:       List[Action], // selection-popup buttons
        blindOffsetActions: List[Action], // blind-offset PopupMenu
        insertTarget:       TargetWithOptId => Callback,
        sources:            NonEmptyMap[TargetType, NonEmptyList[TargetSource[IO]]]
      )

      // Build the reusable menu content.
      def menuItems(
        ctx:               AppContext[IO],
        onSelected:        View[TargetWithOptId => Callback],
        sources:           View[NonEmptyMap[TargetType, NonEmptyList[TargetSource[IO]]]],
        popupState:        View[PopupState],
        actionButtons:     View[List[Button]],
        blindOffsetInfo:   Option[(Observation.Id, View[BlindOffset])],
        addSkyInfo:        Option[AddSkyInfo],
        hasTargets:        Boolean,
        hasScienceTargets: Boolean
      ): MenuContent =
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

        def insertManualBlindOffsetCB(obsId: Observation.Id, blindOffset: View[BlindOffset])(
          targetWithOptId: TargetWithOptId
        ): Callback =
          // the search returns a science target
          val targetAsBlind = targetWithOptId.copy(disposition = TargetDisposition.BlindOffset)
          insertManualBlindOffset(
            obsId,
            targetAsBlind,
            blindOffset
          )
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        def initializeAutomaticBlindOffsetCB(
          obsId:       Observation.Id,
          blindOffset: View[BlindOffset]
        ): Callback =
          (ctx.odbApi
            .initializeAutomaticBlindOffset(obsId) >>
            updateBlindOffset(
              blindOffset,
              BlindOffset(true, none, BlindOffsetType.Automatic),
              none
            ))
            .switching(props.adding.async, AreAdding(_))
            .runAsync

        val programSource =
          TargetSource.FromProgram[IO](props.obsAndTargets.get._2, filterToOs = hasTargets)

        val simbad =
          NonEmptyMap.one(
            TargetType.Sidereal,
            NonEmptyList.one(TargetSource.FromSimbad[IO](ctx.simbadClient))
          )

        val all =
          NonEmptyMap.of(
            TargetType.Sidereal    ->
              NonEmptyList.of(programSource, TargetSource.FromSimbad[IO](ctx.simbadClient)),
            TargetType.Nonsidereal ->
              NonEmptyList.of(programSource, TargetSource.FromHorizons[IO](ctx.horizonsClient))
          )

        val blindOffsetActions: List[Action] =
          blindOffsetInfo
            .fold(List.empty)((obsId, blindOffset) =>
              List(
                Option.unless(blindOffset.get.isAutomatic)(
                  Action(
                    "Automatic Blind Offset",
                    Icons.LocationDot,
                    initializeAutomaticBlindOffsetCB(obsId, blindOffset),
                    disabled = !hasScienceTargets
                  )
                ),
                Action(
                  "Blind Offset Search",
                  Icons.LocationDot,
                  onSelected.set(insertManualBlindOffsetCB(obsId, blindOffset)) >>
                    sources.set(simbad) >> popupState.set(PopupState.Open),
                  disabled = !hasScienceTargets
                ).some,
                Action(
                  "Empty Blind Offset",
                  Icons.LocationDot,
                  insertManualBlindOffsetCB(obsId, blindOffset)(
                    TargetWithOptId(None, EmptySiderealTarget, TargetDisposition.BlindOffset, None)
                  ),
                  disabled = !hasScienceTargets
                ).some
              ).flattenOption
            )

        // Toggles the "add sky" mode making the Aladin region clickable.
        val addSkyActions: List[Action] =
          addSkyInfo.toList.map: info =>
            if info.mode.get.isDefined then
              Action("Cancel Adding Sky Position", Icons.Bullseye, info.mode.set(none))
            else
              Action(
                "Add Sky Position",
                Icons.Bullseye,
                info.slot.map(s => info.mode.set(s.some)).getOrEmpty,
                disabled = !info.enabled
              )

        // The search popup-launcher action.
        val targetSearchAction: Action =
          Action(
            "Target Search",
            icon = LayeredIcon()(
              Icons.Star.withTransform(Transform(x = -6, y = -4, size = 15)),
              Icons.PlanetRinged.withTransform(Transform(x = 6, y = 5, size = 15))
            ),
            command = onSelected.set(insertTargetCB) >>
              sources.set(all) >> actionButtons.set(List.empty) >> popupState.set(PopupState.Open)
          )

        // Actions offered inside the TargetSelectionPopup.
        val insertActions: List[Action] =
          List(
            Action(
              "Empty Sidereal Target",
              icon = Icons.Star,
              command = insertTargetCB(TargetWithOptId.newScience(EmptySiderealTarget))
            ),
            Action(
              "Target of Opportunity",
              icon = Icons.HourglassClock,
              command = insertTargetCB(TargetWithOptId.newScience(EmptyOpportunityTarget)),
              disabled = hasTargets
            )
          ) ++ blindOffsetActions

        MenuContent(
          menuActions = targetSearchAction :: insertActions ++ addSkyActions,
          popupActions = insertActions,
          blindOffsetActions = blindOffsetActions,
          insertTarget = insertTargetCB,
          sources = all
        )

      for
        ctx              <- useContext(AppContext.ctx)
        popupState       <- useStateView(PopupState.Closed)
        onSelected       <- useStateView((_: TargetWithOptId) => Callback.empty)
        sources          <- useStateView:
                              // we'll always set this before opening the popup
                              NonEmptyMap.one(
                                TargetType.Sidereal,
                                NonEmptyList.one[TargetSource[IO]]:
                                  TargetSource.FromProgram[IO](props.obsAndTargets.get._2)
                              )
        actionButtons    <- useStateView(List.empty[Button]) // we'll always set this, too
        blindRef         <- usePopupMenuRef
        // Derivations for menu content to detect when to rebuild the items
        observations      = props.obsIds.toList.map(props.obsAndTargets.get._1.get).flattenOption
        hasTargets        =
          observations.headOption.forall(_.scienceTargetIds.nonEmpty) ||
            observations.exists(_.blindOffset.useBlindOffset)
        hasScienceTargets = observations.headOption.exists(_.scienceTargetIds.nonEmpty)
        blindOffsetInfo   = props.blindOffsetInfo
        menus            <- useMemo(
                              (hasTargets,
                               hasScienceTargets,
                               blindOffsetInfo.map(b => (b._1, b._2.get.isAutomatic)),
                               props.addSkyInfo.map(_.menuKey)
                              )
                            ): (hasTargets, hasScienceTargets, _, _) =>
                              menuItems(ctx,
                                        onSelected,
                                        sources,
                                        popupState,
                                        actionButtons,
                                        blindOffsetInfo,
                                        props.addSkyInfo,
                                        hasTargets,
                                        hasScienceTargets
                              )
      yield

        val hasTargetOfOpportunity: Boolean =
          observations.headOption.forall(_.hasTargetOfOpportunity(props.targetList.get))

        val showBlindOffsetButton =
          props.readOnly && props.allowBlindOffset && menus.value.blindOffsetActions.nonEmpty

        val closePopup = popupState.set(PopupState.Closed)

        val buttonList: List[Button] = menus.value.popupActions.map(_.toButton(closePopup))

        // In order for the title bar to look right, we need to have exactly one button in the DOM,
        // although it doesn't need to be visible.
        val button: VdomNode =
          if showBlindOffsetButton then
            Button(
              label = "Blind Offset",
              icon = Icons.New,
              severity = Button.Severity.Success,
              loading = props.adding.get.value,
              onClickE = blindRef.toggle,
              clazz = props.buttonClass
            ).tiny.compact
          else
            SplitButton(
              model = menus.map(_.menuActions.map(_.toMenuItem)),
              onClick =
                onSelected.set(menus.value.insertTarget) >> sources.set(menus.value.sources) >>
                  actionButtons.set(buttonList) >>
                  popupState.set(PopupState.Open),
              severity = Button.Severity.Success,
              icon = Icons.New,
              disabled = props.readOnly || props.adding.get.value || hasTargetOfOpportunity,
              loading = props.adding.get.value,
              label = props.label,
              clazz = props.buttonClass |+| ExploreStyles.Hidden.when_(props.readOnly)
            ).tiny.compact

        React.Fragment(
          button,
          TargetSelectionPopup(
            "Add Target",
            popupState,
            sources.get,
            actionButtons.get,
            selectExistingLabel = "Link",
            selectExistingIcon = Icons.Link,
            selectNewLabel = "Add",
            selectNewIcon = Icons.New,
            onSelected = onSelected.get
          ),
          PopupMenu(model = menus.map(_.blindOffsetActions.map(_.toMenuItem))).withRef(blindRef.ref)
        )
    )
