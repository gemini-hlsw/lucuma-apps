// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.BlindOffset
import explore.model.EmptySiderealTarget
import explore.model.EmptySourceProfile
import explore.model.ErrorMsgOr
import explore.model.ExploreModelValidators
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObservationTargets
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.PopupState
import explore.model.RegionOrTrackingMap
import explore.model.TargetEditObsInfo
import explore.model.UserPreferences
import explore.model.display.given
import explore.model.enums.TargetType
import explore.model.reusability.given
import explore.services.OdbAsterismApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targeteditor.RVInput
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Ephemeris
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TargetResolution
import lucuma.core.model.TelluricType
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.model.enums.BlindOffsetType
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.FormInputText
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.UndoSetter
import monocle.Optional
import monocle.Prism
import org.typelevel.log4cats.Logger

import java.time.Instant

case class TargetEditor(
  programId:                   Program.Id,
  programType:                 ProgramType,
  userId:                      User.Id,
  targetWithId:                UndoSetter[TargetWithId],
  obsAndTargets:               UndoSetter[ObservationsAndTargets],
  // TODO, we may derive obsTargets from obsAndTargets
  obsTargets:                  ObservationTargets, // This is passed through to Aladin, to plot the entire ObservationTargets.
  obsTime:                     Option[Instant],
  obsConf:                     Option[ObsConfiguration],
  trackingMap:                 Pot[ErrorMsgOr[RegionOrTrackingMap]],
  ags:                         AgsData,
  searching:                   View[Set[Target.Id]],
  obsInfo:                     TargetEditObsInfo,
  onClone:                     OnCloneParameters => Callback,
  fullScreen:                  View[AladinFullScreen],
  userPreferences:             View[UserPreferences],
  guideStarSelection:          View[GuideStarSelection],
  attachments:                 View[AttachmentList],
  authToken:                   Option[NonEmptyString],
  readonly:                    Boolean,
  allowEditingOngoing:         Boolean,
  isStaffOrAdmin:              Boolean,
  telluricType:                Option[TelluricType] = None,
  invalidateSequence:          Callback = Callback.empty,
  blindOffsetInfo:             Option[(Observation.Id, View[BlindOffset])] = none,
  renderAladin:                Boolean = true,
  externalObsToCloneTo:        Option[View[Option[ObsIdSet]]] = None,
  externalReadonlyForStatuses: Option[View[Boolean]] = None
) extends ReactFnProps(TargetEditor.component):
  def toManualBlindOffset: Callback =
    if targetWithId.get.disposition === TargetDisposition.BlindOffset then
      blindOffsetInfo.foldMap: (_, bo) =>
        bo.zoom(BlindOffset.blindOffsetType).set(BlindOffsetType.Manual)
    else Callback.empty

object TargetEditor:
  private type Props = TargetEditor

  private def cloneTarget(
    programId:     Program.Id,
    targetId:      Target.Id,
    obsIds:        ObsIdSet,
    cloning:       View[Boolean],
    obsAndTargets: UndoSetter[ObservationsAndTargets],
    onClone:       OnCloneParameters => Callback
  )(
    input:         UpdateTargetsInput
  )(using
    odbApi:        OdbTargetApi[IO] & OdbAsterismApi[IO]
  )(using Logger[IO], ToastCtx[IO]): IO[Unit] =
    odbApi
      .cloneTarget(targetId, obsIds, input)
      .flatMap: clone =>
        (TargetCloneAction
          .cloneTarget(programId, targetId, clone, obsIds, onClone)
          .set(obsAndTargets)(clone.target.some) >>
          // If we do the first `onClone` here, the UI works correctly.
          onClone(OnCloneParameters(targetId, clone.id, obsIds, true))).toAsync
      .switching(cloning.async)
      .handleErrorWith: t => // TODO Move error handling to API layer
        val msg = s"Error cloning target [$targetId]"
        Logger[IO].error(t)(msg) >>
          ToastCtx[IO].showToast(msg, Message.Severity.Error)

  // An UndoSetter that doesn't really update any undo stacks
  private def noopUndoSetter[M](view: View[M]): UndoSetter[M] =
    new UndoSetter[M] {
      val model         = view
      def set[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(v: A): Callback =
        mod(getter, setter, onSet, onRestore)(_ => v)

      def mod[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(f: A => A): Callback =
        model.modCB(
          oldModel => setter(f(getter(oldModel)))(oldModel),
          (oldModel, newModel) => onSet(oldModel, getter(newModel)).runAsyncAndForget
        )
    }

  private def emptySourceProfileInput(sp: SourceProfile): SourceProfileInput =
    sp match
      case SourceProfile.Point(sd)      =>
        SourceProfileInput.Point(emptySpectralDefinitionIntegratedInput(sd))
      case SourceProfile.Uniform(sd)    =>
        SourceProfileInput.Uniform(emptySpectralDefinitionSurfaceInput(sd))
      // The spectral definition must be left unassigned here: this is the base for *all*
      // deltas under `gaussian`, so assigning it would make even a plain FWHM edit emit a
      // spurious spectral definition edit. `SourceProfileEditor` fills it in via `forceAssign`.
      case SourceProfile.Gaussian(_, _) =>
        SourceProfileInput.Gaussian:
          GaussianInput()

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                         <- useContext(AppContext.ctx)
        cloning                     <- useStateView(false)
        internalObsToCloneTo        <- useStateView(none[ObsIdSet])
        internalReadonlyForStatuses <- useStateView(false)
        resolvePopupState           <- useStateView(PopupState.Closed)
        // If obsTime is not set, change it to now at the start of the day in UTC.
        obsTime                     <- useEffectKeepResultWithDeps(props.obsTime): obsTime =>
                                         IO(obsTimeOrDefault(obsTime))
        // select the aligner to use based on whether a clone will be created or not.
        targetAligner               <-
          val obsToCloneTo = props.externalObsToCloneTo.getOrElse(internalObsToCloneTo)
          useMemo(
            (props.programId,
             props.targetWithId.get.target,
             props.targetWithId.get.id,
             obsToCloneTo.get
            )
          ): (pid, target, tid, toCloneTo) =>
            import ctx.given

            toCloneTo.fold(
              Aligner(
                props.targetWithId.zoom(TargetWithId.target),
                UpdateTargetsInput(
                  WHERE = tid.toWhereTarget.assign,
                  SET = TargetPropertiesInput()
                ),
                // Invalidate the sequence if the target changes, and if it is a blind offset
                // make it a manual blind offset
                u =>
                  props.invalidateSequence.to[IO] >>
                    props.toManualBlindOffset.to[IO] >> ctx.odbApi.updateTarget(u)
              )
            ): obsIds =>
              val view = View(target, (mod, cb) => cb(target, mod(target)))
              Aligner(
                noopUndoSetter(view),
                // noopUndoSetter(noUndoTargetView),
                UpdateTargetsInput(SET = TargetPropertiesInput()),
                u =>
                  props.invalidateSequence.to[IO] *>
                    cloneTarget(
                      pid,
                      tid,
                      obsIds,
                      cloning,
                      props.obsAndTargets,
                      props.onClone
                    )(u)
              )
      yield
        import ctx.given

        val obsToCloneTo        = props.externalObsToCloneTo.getOrElse(internalObsToCloneTo)
        val readonlyForStatuses =
          props.externalReadonlyForStatuses.getOrElse(internalReadonlyForStatuses)

        val disabled: Boolean =
          props.searching.get.exists(_ === props.obsTargets.focus.id) ||
            cloning.get || props.readonly || readonlyForStatuses.get ||
            props.targetWithId.get.isReadonlyForProgramType(props.programType)

        // Via the resolution, so a Target of Opportunity resolved to a catalog entry keeps it.
        val catalogInfo: Option[CatalogInfo] =
          props.targetWithId.get.target.asSidereal.flatMap(_.catalogInfo)

        val nameLens          = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.name)
        val siderealLens      = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sidereal)
        val nonsideralLens    = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.nonsidereal)
        val opportunityLens   = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.opportunity)
        val sourceProfileLens = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sourceProfile)

        extension [A, B](prism: Prism[A, B])
          def optReplace[I](a: A, f: B => (I => I)): I => I =
            i => prism.getOption(a).fold(i)(b => f(b)(i))

        val allView: View[Target] =
          targetAligner.viewMod(t =>
            nameLens.replace(t.name.assign) >>>
              Target.sidereal.optReplace(t, s => siderealLens.replace(s.toInput.assign)) >>>
              Target.nonsidereal.optReplace(t, ns => nonsideralLens.replace(ns.toInput.assign)) >>>
              Target.opportunity.optReplace(t, o => opportunityLens.replace(o.toInput.assign)) >>>
              sourceProfileLens.replace(t.sourceProfile.toInput.assign)
          )

        val siderealToTargetEndo: Endo[SiderealInput] => Endo[UpdateTargetsInput] =
          forceAssign(siderealLens.modify)(SiderealInput())

        // The nested delta path for the sidereal tracking of a *resolved* Target of Opportunity:
        // SET.opportunity.resolution.sidereal. Each level is forced into existence so an edit to a
        // single field still arrives as a well-formed nested input.
        val opportunityToTargetEndo: Endo[OpportunityInput] => Endo[UpdateTargetsInput] =
          forceAssign(opportunityLens.modify)(OpportunityInput())

        val resolutionToTargetEndo: Endo[TargetResolutionInput] => Endo[UpdateTargetsInput] =
          forceAssign((f: Endo[Input[TargetResolutionInput]]) =>
            opportunityToTargetEndo(OpportunityInput.resolution.modify(f))
          )(TargetResolutionInput())

        val resolvedSiderealToTargetEndo: Endo[SiderealInput] => Endo[UpdateTargetsInput] =
          forceAssign((f: Endo[Input[SiderealInput]]) =>
            resolutionToTargetEndo(TargetResolutionInput.sidereal.modify(f))
          )(SiderealInput())

        // A Target of Opportunity that has resolved to a sidereal target tracks exactly like one,
        // so the sidereal editors below are pointed at its resolution rather than at a
        // `Target.Sidereal` it will never be.
        val opportunitySiderealTracking: Optional[Target, SiderealTracking] =
          Optional[Target, SiderealTracking](t =>
            Target.opportunityResolution
              .getOption(t)
              .flatten
              .collect:
                case TargetResolution.Sidereal(tracking, _) => tracking
          )(tracking =>
            Target.opportunityResolution.modify(
              _.map:
                case TargetResolution.Sidereal(_, catalogInfo) =>
                  TargetResolution.Sidereal(tracking, catalogInfo)
                case other                                     => other
            )
          )

        // Sidereal tracking, however the target comes by it. Empty for a nonsidereal target and
        // for a Target of Opportunity that is either unresolved or resolved to a nonsidereal one.
        val optSiderealTrackingAligner: Option[Aligner[SiderealTracking, SiderealInput]] =
          targetAligner.value
            .zoomOpt(
              Target.sidereal.andThen(Target.Sidereal.tracking),
              siderealToTargetEndo
            )
            .orElse:
              targetAligner.value.zoomOpt(
                opportunitySiderealTracking,
                resolvedSiderealToTargetEndo
              )

        val optOpportunityAligner: Option[Aligner[Target.Opportunity, TargetPropertiesInput]] =
          targetAligner.value.zoomOpt(
            Target.opportunity,
            UpdateTargetsInput.SET.modify
          )

        val nameView: View[NonEmptyString] =
          targetAligner
            .zoom(Target.name, nameLens.modify)
            .view(_.assign)

        val sourceProfileAligner: Aligner[SourceProfile, SourceProfileInput] =
          targetAligner.zoom(
            Target.sourceProfile,
            forceAssign(sourceProfileLens.modify)(
              emptySourceProfileInput(targetAligner.get.sourceProfile)
            )
          )

        def siderealCoordinates(
          siderealTargetAligner: Aligner[SiderealTracking, SiderealInput]
        ): VdomElement = {

          val coordsRAView: View[RightAscension] =
            siderealTargetAligner
              .zoom(SiderealTracking.baseRa, SiderealInput.ra.modify)
              .view(_.toInput.assign)

          val coordsDecView: View[Declination] =
            siderealTargetAligner
              .zoom(SiderealTracking.baseDec, SiderealInput.dec.modify)
              .view(_.toInput.assign)

          React.Fragment(
            FormInputTextView(
              id = "ra".refined,
              value = coordsRAView,
              label = React.Fragment("RA", HelpIcon("target/main/coordinates.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.truncatedRA,
              changeAuditor = ChangeAuditor.accept,
              validateOnPaste = false
            ),
            FormInputTextView(
              id = "dec".refined,
              value = coordsDecView,
              label = React.Fragment("Dec", HelpIcon("target/main/coordinates.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.truncatedDec,
              changeAuditor = ChangeAuditor.accept,
              validateOnPaste = false
            )
          )
        }

        def opportunityRegion(
          opportunityAligner: Aligner[Target.Opportunity, TargetPropertiesInput]
        ): VdomElement =
          val regionView: View[Region] =
            opportunityAligner
              .zoom(Target.Opportunity.region, TargetPropertiesInput.opportunity.modify)
              // The region alone: omitting `resolution` leaves it alone.
              .view(r => OpportunityInput(region = r.toInput.assign).assign)
          RegionEditor(regionView, disabled)

        // Resolving and unresolving are the same edit seen from either side, so both go through
        // this one view. `None` is sent as an explicit null, which is how the ODB is told to
        // return the target to waiting; the region is left untouched either way.
        def opportunityResolution(
          opportunityAligner: Aligner[Target.Opportunity, TargetPropertiesInput]
        ): View[Option[TargetResolution]] =
          opportunityAligner
            .zoom(Target.Opportunity.resolution, TargetPropertiesInput.opportunity.modify)
            .view(r => OpportunityInput(resolution = r.map(_.toInput).orUnassign).assign)

        val ephemerisKey: Option[VdomNode] =
          targetAligner.get.asNonsidereal
            .map(_.ephemerisKey)
            .map: key =>
              val (label, value) = key match
                case Ephemeris.Key.UserSupplied(id) =>
                  ("User Supplied", id.toString)
                case h: Ephemeris.Key.Horizons      =>
                  ("Horizons", s"${key.keyType.simplifiedName} ${h.des}")
              FormInputText(
                id = "ephemeris-key".refined,
                value = value,
                label = React.Fragment(
                  label,
                  HelpIcon("target/main/ephemeris-key.md".refined)
                ),
                disabled = true
              )

        def siderealTracking(
          siderealTargetAligner: Aligner[SiderealTracking, SiderealInput]
        ): VdomElement = {
          val epochView: View[Epoch] =
            siderealTargetAligner
              .zoom(SiderealTracking.epoch, SiderealInput.epoch.modify)
              .view(Epoch.fromString.reverseGet.andThen(_.assign))

          val properMotionView: View[ProperMotion] =
            siderealTargetAligner
              .zoom(SiderealTracking.properMotion, SiderealInput.properMotion.modify)
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(ProperMotion.Zero)

          val properMotionRAView: View[ProperMotion.RA] =
            properMotionView.zoom(ProperMotion.ra)

          val properMotionDecView: View[ProperMotion.Dec] =
            properMotionView.zoom(ProperMotion.dec)

          val parallaxView: View[Parallax] =
            siderealTargetAligner
              .zoom(
                SiderealTracking.parallax,
                SiderealInput.parallax.modify
              )
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(Parallax.Zero)

          val radialVelocityView: View[RadialVelocity] =
            siderealTargetAligner
              .zoom(
                SiderealTracking.radialVelocity,
                SiderealInput.radialVelocity.modify
              )
              .view(_.map(_.toInput).orUnassign)
              .removeOptionality(RadialVelocity.Zero)

          <.div(
            LucumaPrimeStyles.FormColumnVeryCompact,
            ExploreStyles.TargetProperMotionForm,
            FormInputTextView(
              id = "epoch".refined,
              value = epochView,
              label = React.Fragment("Epoch", HelpIcon("target/main/epoch.md".refined)),
              disabled = disabled,
              validFormat = MathValidators.epochNoScheme,
              changeAuditor = ChangeAuditor.maxLength(8.refined).decimal(3.refined).denyNeg,
              units = "years"
            ),
            FormInputTextView(
              id = "raPM".refined,
              value = properMotionRAView,
              label = "µ RA",
              disabled = disabled,
              validFormat = ExploreModelValidators.pmRAValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas/y",
              groupClass = ExploreStyles.ZeroValue.when_(
                properMotionRAView.get === ProperMotion.Zero.ra
              )
            ),
            FormInputTextView(
              id = "raDec".refined,
              value = properMotionDecView,
              label = "µ Dec",
              disabled = disabled,
              validFormat = ExploreModelValidators.pmDecValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas/y",
              groupClass = ExploreStyles.ZeroValue.when_(
                properMotionDecView.get === ProperMotion.Zero.dec
              )
            ),
            FormInputTextView(
              id = "parallax".refined,
              value = parallaxView,
              label = "Parallax",
              disabled = disabled,
              validFormat = ExploreModelValidators.pxValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined),
              units = "mas",
              groupClass = ExploreStyles.ZeroValue.when_(
                parallaxView.get === Parallax.Zero
              )
            ),
            RVInput(
              radialVelocityView,
              disabled,
              props.obsConf.flatMap(_.calibrationRole),
              props.obsTargets.focus.id,
              props.userPreferences,
              props.userId
            )
          )
        }

        val targetSources: NonEmptyMap[TargetType, NonEmptyList[TargetSource[IO]]] =
          NonEmptyMap.of(
            TargetType.Sidereal    ->
              NonEmptyList.one(TargetSource.FromSimbad[IO](ctx.simbadClient)),
            TargetType.Nonsidereal ->
              NonEmptyList.one(TargetSource.FromHorizons[IO](ctx.horizonsClient))
          )

        // Resolving may point at a target the program already holds, but only one that has
        // tracking to offer: an unresolved Target of Opportunity has no resolution to copy, and
        // the target being edited is itself.
        val resolveTargetSources: NonEmptyMap[TargetType, NonEmptyList[TargetSource[IO]]] =
          val programSource: TargetSource[IO]    =
            TargetSource.FromProgram[IO](
              props.obsAndTargets.get._2,
              include = twid =>
                !twid.isUnresolvedTargetOfOpportunity && twid.id =!= props.targetWithId.get.id
            )
          NonEmptyMap.of(
            TargetType.Sidereal    ->
              NonEmptyList.of(programSource, TargetSource.FromSimbad[IO](ctx.simbadClient)),
            TargetType.Nonsidereal ->
              NonEmptyList.of(programSource, TargetSource.FromHorizons[IO](ctx.horizonsClient))
          )

        // Resets a slot's sky position
        val resetSky: Option[SlotId => IO[Unit]] =
          props.obsInfo.current
            .filterNot(_ => props.readonly)
            .map: obsIds =>
              slot =>
                slot match
                  case SlotId.GhostIfu2 =>
                    ctx.odbApi.updateGhostIfu2SkyPosition(obsIds.idSet.toList, none).toastErrors
                  case _                => IO.unit

        // Resolving a Target of Opportunity means recording what the alert identified, not
        // replacing the target: it keeps its name, its approved region and its identity as a ToO.
        val optResolutionView: Option[View[Option[TargetResolution]]] =
          optOpportunityAligner.map(opportunityResolution)

        // Applying a resolution is a single edit: the Target of Opportunity takes on the resolving
        // target's name, tracking and source profile, while keeping its id, its approved region
        // and its identity as a ToO. `region` is omitted from the delta, which is what leaves the
        // approved region alone.
        val optResolveView: Option[View[Target]] =
          optOpportunityAligner.map: _ =>
            targetAligner.viewMod: t =>
              nameLens.replace(t.name.assign) >>>
                Target.opportunity.optReplace(
                  t,
                  o =>
                    opportunityLens.replace(
                      OpportunityInput(resolution = o.resolution.map(_.toInput).orUnassign).assign
                    )
                ) >>>
                sourceProfileLens.replace(t.sourceProfile.toInput.assign)

        def resolveWith(resolveView: View[Target])(twoid: TargetWithOptId): Callback =
          resolveView.mod:
            Target.opportunity.modify: o =>
              o.copy(
                name = twoid.target.name,
                resolution = twoid.target.resolution,
                // Horizons hits -- and anything else with nothing to say about brightness -- must
                // not wipe a source profile the ToO already has.
                sourceProfile =
                  if twoid.target.sourceProfile === EmptySourceProfile then o.sourceProfile
                  else twoid.target.sourceProfile
              )

        // Once resolved, a Target of Opportunity presents itself as an ordinary target, so only
        // "Unresolve" is offered: replacing a resolution outright means unresolving first.
        val resolveButtons: Option[VdomNode] =
          optResolutionView.map: resolutionView =>
            <.div(ExploreStyles.TargetResolutionControls)(
              if resolutionView.get.isDefined then
                Button(
                  label = "Unresolve",
                  icon = Icons.HourglassClock,
                  onClick = resolutionView.set(none),
                  disabled = disabled
                ).tiny.compact
              else
                Button(
                  label = "Resolve",
                  icon = Icons.Search,
                  onClick = resolvePopupState.set(PopupState.Open),
                  disabled = disabled
                ).tiny.compact
            )

        // The popup is only the picker. It offers the catalogs plus the program's own targets --
        // whatever it returns, only its name, resolution and source profile are taken.
        val resolvePopup: Option[VdomNode] =
          (optResolutionView, optResolveView).mapN: (resolutionView, resolveView) =>
            TargetSelectionPopup(
              "Resolve Target of Opportunity",
              resolvePopupState,
              resolveTargetSources,
              // Of the buttons the add-target flow offers, only an empty sidereal target makes
              // sense here: the rest either create a target or need an observation. It is a blank
              // slate for typing coordinates into, so it sets only the resolution -- the ToO keeps
              // its own name and source profile.
              List(
                Button(
                  "Empty Sidereal Target",
                  icon = Icons.Star,
                  onClick = resolvePopupState.set(PopupState.Closed) >>
                    resolutionView.set(
                      TargetResolution.Sidereal(EmptySiderealTarget.tracking, none).some
                    )
                ).tiny.compact
              ),
              selectExistingLabel = "Resolve",
              selectExistingIcon = Icons.ArrowDownLeft,
              selectNewLabel = "Resolve",
              selectNewIcon = Icons.ArrowDownLeft,
              onSelected = resolveWith(resolveView),
              existingHeader = "Resolve to an existing target"
            )

        val formColumn =
          <.div(LucumaPrimeStyles.FormColumnVeryCompact, ExploreStyles.TargetForm)(
            // The telluric star type this calibration target was chosen for
            props.telluricType.map: tt =>
              React.Fragment(
                FormLabel(htmlFor = "telluric-type".refined)("Telluric"),
                <.span(^.id := "telluric-type", tt.shortName).withOptionalTooltip(
                  TelluricType.manual
                    .getOption(tt)
                    .map(m => s"Star types: ${m.starTypes.toList.mkString(", ")}")
                )
              ),
            // Keep the search field and the coords always together
            SearchForm(
              props.obsTargets.focus.id,
              // SearchForm doesn't edit the name directly. It will set it atomically, together
              // with coords & magnitudes from the catalog search, so that all 3 fields are
              // a single undo/redo operation.
              nameView,
              targetSources,
              allView.set,
              props.searching,
              disabled,
              cloning.get,
              // A Target of Opportunity must not be replaced wholesale by a catalog entry, even
              // once it has resolved -- that would discard its region. It resolves instead.
              disableSearch =
                props.targetWithId.get.disposition === TargetDisposition.BlindOffset ||
                  props.targetWithId.get.isTargetOfOpportunity
            ),
            optSiderealTrackingAligner.map(siderealCoordinates),
            // The arcs are what a Target of Opportunity has *instead* of a position. Once it has
            // one they are just noise, so a resolved ToO reads like an ordinary target. The region
            // is still kept -- unresolving brings the editor back.
            optOpportunityAligner.filterNot(_.get.isResolved).map(opportunityRegion),
            resolveButtons,
            ephemerisKey
          )

        val sourceProfileColumn =
          <.div(
            ExploreStyles.Grid,
            ExploreStyles.Compact,
            LucumaPrimeStyles.FormColumnVeryCompact,
            ExploreStyles.TargetSourceProfileEditor,
            ExploreStyles.WithGaussian
              .when(SourceProfile.gaussian.getOption(sourceProfileAligner.get).isDefined),
            ExploreStyles.WithCatalogInfo
              .when(catalogInfo.flatMap(_.objectType).isDefined)
          )(
            // The `withKey` is important because React wasn't updating the BrightnessesEditor
            // or the EmissionsLineEditor when the obsIdSubset changed, resulting in targets always
            // being cloned even when all targets should have been edited.
            SourceProfileEditor(
              props.programId,
              sourceProfileAligner,
              catalogInfo,
              props.attachments,
              props.authToken,
              props.obsConf.flatMap(_.calibrationRole),
              disabled,
              props.userPreferences.get.globalPreferences.wavelengthUnits
            ).withKey(obsToCloneTo.get.fold("none")(_.show))
          )

        if props.renderAladin then
          React.Fragment(
            TargetCloneSelector(
              props.obsInfo,
              obsToCloneTo,
              readonlyForStatuses,
              props.allowEditingOngoing
            ),
            <.div(ExploreStyles.TargetGrid)(
              // If there is an unresolved ToO in the obsTargets, we won't have a baseTracking and will skip visualization.
              obsTime.value.renderPot(ot =>
                AladinCell(
                  props.userId,
                  props.obsTargets,
                  ot,
                  props.obsConf,
                  props.trackingMap,
                  props.ags,
                  props.fullScreen,
                  props.userPreferences,
                  props.guideStarSelection,
                  props.blindOffsetInfo,
                  props.obsAndTargets.model.zoom(ObservationsAndTargets.targets),
                  none,
                  none,
                  resetSky,
                  props.isStaffOrAdmin,
                  props.readonly
                )
              ),
              formColumn,
              optSiderealTrackingAligner.map(siderealTracking),
              sourceProfileColumn
            ),
            resolvePopup
          )
        else
          // Form columns only — no grid wrapper, no AladinCell, no TargetCloneSelector.
          // The caller renders TargetCloneSelector before the grid at the tile level.
          React.Fragment(
            formColumn,
            optSiderealTrackingAligner.map(siderealTracking),
            sourceProfileColumn,
            resolvePopup
          )
