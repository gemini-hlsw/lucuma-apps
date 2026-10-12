// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.events.CatalogMessage.*
import explore.model.AppContext
import explore.model.BlindOffset
import explore.model.ObservationTargets
import explore.model.TargetList
import explore.model.WorkerClients.CatalogClient
import explore.model.boopickle.CatalogPicklers.given
import explore.model.display.given
import explore.model.reusability.given
import explore.services.OdbObservationApi
import explore.services.OdbTargetApi
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.BlindOffsetLimits
import lucuma.core.enums.Instrument
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.syntax.display.*
import lucuma.core.util.NewBoolean
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.FieldSet
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.TooltipOptions
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.schemas.model.syntax.*
import lucuma.schemas.odb.input.toTargetPropertiesInput
import lucuma.schemas.odb.input.toWhereTarget
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import org.typelevel.log4cats.Logger

import java.time.Duration
import java.time.Instant

case class BlindOffsetControl(
  obsId:        Observation.Id,
  blindOffset:  View[BlindOffset],
  obsTime:      Instant,
  baseTracking: Tracking,
  instrument:   Option[Instrument],
  obsTargets:   ObservationTargets,
  allTargets:   View[TargetList],
  readonly:     Boolean
) extends ReactFnProps(BlindOffsetControl):
  val supportedInstrument: Option[Instrument] =
    instrument.filter(BlindOffsetLimits.forInstrument(_).isDefined)

  // Candidate targets carry estimated brightnesses and an SED, so compare by Gaia source id
  // rather than relying on the whole target surviving an ODB round-trip unchanged.
  def isSameStar(target: Target, candidate: BlindOffsetCandidate): Boolean =
    target.name.value === candidate.sourceId.value

// TODO: Set the blind offset to the selected target when it is changed.
object BlindOffsetControl
    extends ReactFnComponent[BlindOffsetControl](props =>
      object IsWorking extends NewBoolean
      type IsWorking = IsWorking.Type

      def updateTargetListAndBlindOffset(
        toDelete:        Option[Target.Id],
        toAdd:           Option[TargetWithId],
        blindOffsetType: BlindOffsetType,
        useBlindOffset:  Boolean = true
      ): Callback =
        props.allTargets.mod: tl =>
          val removed = toDelete.fold(tl)(tl.removed)
          toAdd.fold(removed)(a => removed.updated(a.id, a))
        >> props.blindOffset.set(BlindOffset(useBlindOffset, toAdd.map(_.id), blindOffsetType))

      def setBlindOffset(
        candidate:       BlindOffsetCandidate,
        blindOffsetType: BlindOffsetType,
        current:         Option[Target.Id],
        isWorking:       View[IsWorking]
      )(using
        odbApi:          OdbObservationApi[IO],
        targetApi:       OdbTargetApi[IO],
        logger:          Logger[IO]
      ): IO[Unit] =
        (current, Option.when(blindOffsetType === BlindOffsetType.Manual)(()))
          .mapN: (currentId, _) =>
            // If we already have a blind offset and we're updating to a manual blind offset,
            // we can just edit the target. The API will convert to manual if it isn't already.
            updateTargetListAndBlindOffset(
              none,
              TargetWithId(currentId,
                           candidate.catalogResult.target,
                           TargetDisposition.BlindOffset,
                           none
              ).some,
              blindOffsetType
            ).toAsync >>
              targetApi.updateTarget(
                UpdateTargetsInput(WHERE = currentId.toWhereTarget.assign,
                                   SET = candidate.catalogResult.target.toTargetPropertiesInput
                )
              )
          .getOrElse(
            odbApi
              .setBlindOffsetTarget(
                props.obsId,
                candidate.catalogResult.target,
                blindOffsetType
              )
              .flatMap: newId =>
                updateTargetListAndBlindOffset(
                  current,
                  TargetWithId(newId,
                               candidate.catalogResult.target,
                               TargetDisposition.BlindOffset,
                               none
                  ).some,
                  blindOffsetType
                ).toAsync
              .switching(isWorking.async, IsWorking(_))
          )

      def initializeAutomaticBlindOffset(
        current:   Option[Target.Id],
        isWorking: View[IsWorking]
      )(using
        api:       OdbObservationApi[IO],
        logger:    Logger[IO]
      ): IO[Unit] =
        (api.initializeAutomaticBlindOffset(props.obsId) >>
          updateTargetListAndBlindOffset(current, none, BlindOffsetType.Automatic)
            .when_(current.isDefined)
            .toAsync)
          .switching(isWorking.async, IsWorking(_))

      def deleteBlindOffsetTarget(using api: OdbObservationApi[IO], logger: Logger[IO]): Callback =
        val cb = api.deleteBlindOffsetTarget(props.obsId).runAsync >>
          updateTargetListAndBlindOffset(
            props.blindOffset.get.blindOffsetTargetId,
            none,
            BlindOffsetType.Automatic,
            useBlindOffset = false
          )
        ConfirmDialog.confirmDialog(
          message = <.div("Delete the blind offset? This action cannot be undone."),
          header = "Blind offset delete",
          acceptLabel = "Yes, delete",
          accept = cb,
          position = DialogPosition.Top,
          acceptClass = PrimeStyles.ButtonSmall,
          rejectClass = PrimeStyles.ButtonSmall,
          icon = Icons.SkullCrossBones(^.color.red)
        )

      for
        ctx              <- useContext(AppContext.ctx)
        time2Search      <- useState(props.obsTime)
        _                <- useEffectWithDeps(props.obsTime): obsTime =>
                              val diff = Duration.between(time2Search.value, obsTime).abs()
                              if (props.baseTracking.isNonsidereal && diff.toHours() > 1) || diff.toDays >= 30L
                              then time2Search.setState(obsTime)
                              else Callback.empty
        candidatesResult <-
          useEffectResultWithDeps(
            (props.blindOffset.get.useBlindOffset,
             time2Search.value,
             props.baseTracking,
             props.supportedInstrument
            )
          ): (useBlindOffset, obsTime, baseTracking, oInstrument) =>
            import ctx.given

            oInstrument
              .filter(_ => useBlindOffset)
              .fold(none.pure[IO]): instrument =>
                baseTracking
                  .coordinatesAt(obsTime)
                  .fold(
                    _.asLeft.some.pure[IO],
                    coordsAt =>
                      CatalogClient[IO]
                        .requestSingle(
                          BlindOffsetRequest(coordsAt, instrument)
                        )
                  )
        index            <- useStateView(-1)
        isWorking        <- useStateView(IsWorking(false))
        _                <-
          useEffectWithDeps((candidatesResult.value, props.blindOffset.reuseByValue)):
            (candidates, blindOffset) =>
              import ctx.given

              val oList = candidates.value.toOption.flatten.flatMap(_.toOption)

              // Skip while readonly
              (oList,
               Option.when(!props.readonly && blindOffset.value.get.isAutomatic)(
                 props.obsTargets.blindOffset
               )
              )
                .mapN: (list, oCurrent) =>
                  // if there is no usable candidate, delete the current blind offset, if any
                  list.headOption
                    .filter(_.isUsable)
                    .fold(
                      oCurrent.foldMap: twid =>
                        initializeAutomaticBlindOffset(twid.id.some, isWorking)
                    ): candidate =>
                      if oCurrent.forall(c => !props.isSameStar(c.target, candidate))
                      then
                        setBlindOffset(candidate,
                                       BlindOffsetType.Automatic,
                                       oCurrent.map(_.id),
                                       isWorking
                        )
                      else IO.unit
                .getOrElse(IO.unit)
        _                <-
          useEffectWithDeps((candidatesResult.value, props.obsTargets.blindOffset)):
            (candidates, oBlindOffset) =>
              candidates.value.toOption.flatten
                .flatMap(_.toOption)
                .foldMap: list =>
                  val newIndex = oBlindOffset.fold(-1): bo =>
                    list.indexWhere(props.isSameStar(bo.target, _))
                  index.set(newIndex)
      yield
        import ctx.given

        def selectIndex(candidates: List[BlindOffsetCandidate], index: Int): Callback =
          candidates
            .lift(index)
            .foldMap: candidate =>
              setBlindOffset(candidate,
                             BlindOffsetType.Manual,
                             props.blindOffset.get.blindOffsetTargetId,
                             isWorking
              ).runAsync

        def renderNavigation(candidates: List[BlindOffsetCandidate]): VdomNode =
          val text = props.obsTargets.blindOffset.fold(
            if candidates.isEmpty then "No candidates found"
            else s"${candidates.length} candidates found"
          ): current =>
            val rejection =
              candidates
                .lift(index.get)
                .flatMap(_.rejection)
                .foldMap(r => s", ${r.shortName}")
            s"${current.target.name} (${index.get + 1}/${candidates.length} candidates$rejection)"
          <.span(
            React
              .Fragment(
                Button(
                  icon = Icons.ChevronLeft,
                  tooltip = "Select Previous Candidate",
                  tooltipOptions = TooltipOptions.ShowOnDisabled,
                  severity = Button.Severity.Secondary,
                  disabled = index.get < 1 || isWorking.get.value || props.readonly,
                  onClick = selectIndex(candidates, index.get - 1)
                ).veryCompact,
                Button(
                  icon = Icons.ChevronRight,
                  tooltip = "Select Next Candidate",
                  tooltipOptions = TooltipOptions.ShowOnDisabled,
                  severity = Button.Severity.Secondary,
                  disabled =
                    (index.get + 1) >= candidates.length || index.get < 0 || isWorking.get.value || props.readonly,
                  onClick = selectIndex(candidates, index.get + 1)
                ).veryCompact
              )
              .when(candidates.nonEmpty),
            text
          )

        def renderUnavailable: VdomNode =
          val reason = props.instrument.fold("select an observing mode to search for candidates"):
            i => s"blind offsets are not supported for ${i.shortName}"
          <.span(
            props.obsTargets.blindOffset.fold(reason.capitalize)(c => s"${c.target.name} ($reason)")
          )

        // Kept outside the candidate list so a blind offset can always be reverted or deleted,
        // even when no candidates can be searched for.
        def renderActions(candidates: List[BlindOffsetCandidate]): VdomNode =
          val usable = candidates.headOption.filter(_.isUsable)
          <.span(
            Button(
              label = "Revert to Automatic",
              tooltip = usable.fold("No usable candidates, the blind offset will be removed")(_ =>
                "Use automatic blind offset selection"
              ),
              tooltipOptions = TooltipOptions.Left,
              disabled = props.readonly || isWorking.get.value,
              onClick = usable
                .fold(
                  initializeAutomaticBlindOffset(props.blindOffset.get.blindOffsetTargetId,
                                                 isWorking
                  )
                ): candidate =>
                  setBlindOffset(
                    candidate,
                    BlindOffsetType.Automatic,
                    props.blindOffset.get.blindOffsetTargetId,
                    isWorking
                  )
                .runAsync
            ).veryCompact.when(props.blindOffset.get.isManual),
            Button(
              icon = Icons.TrashUnstyled,
              severity = Button.Severity.Danger,
              tooltip = "Delete blind offset",
              tooltipOptions = TooltipOptions.Left,
              onClick = deleteBlindOffsetTarget,
              disabled = props.readonly
            ).veryCompact
          )

        val candidates: List[BlindOffsetCandidate] =
          candidatesResult.value.value.toOption.flatten.flatMap(_.toOption).orEmpty

        if props.blindOffset.get.useBlindOffset
        then
          FieldSet(
            legend = "Blind Offset",
            clazz = ExploreStyles.BlindOffsetControl
          )(
            props.supportedInstrument.fold(renderUnavailable): _ =>
              candidatesResult.value.value.renderPot(
                oel =>
                  oel.fold(EmptyVdom): el =>
                    el.fold(err => s"Error loading candidates: ${err}", renderNavigation(_)),
                pendingRender = "Loading candidates..."
              ),
            renderActions(candidates)
          )
        else EmptyVdom
    )
