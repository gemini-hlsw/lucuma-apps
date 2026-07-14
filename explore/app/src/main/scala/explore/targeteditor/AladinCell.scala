// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.Order.given
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import crystal.react.syntax.pot.given
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.*
import explore.model.InteractiveRegion
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.given
import explore.optics.ModelOptics
import explore.targeteditor.UseAgsCalculation.*
import explore.utils.tracking.*
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.*
import lucuma.ags.syntax.*
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.model.User
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.syntax.minimizeEphemeris
import lucuma.ui.aladin.AladinFullScreen as UIFullScreen
import lucuma.ui.aladin.AladinFullScreenControl
import lucuma.ui.aladin.Fov
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.schemas.UserPreferencesDB

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

case class AladinCell(
  uid:                 User.Id,
  obsTargets:          ObservationTargets,
  obsTime:             Instant,
  obsConf:             Option[ObsConfiguration],
  fullScreen:          View[AladinFullScreen],
  userPreferences:     View[UserPreferences],
  guideStarSelection:  View[GuideStarSelection],
  blindOffsetInfo:     Option[(Observation.Id, View[BlindOffset])],
  allTargets:          View[TargetList], // for blind offset, no undo
  assignSky:           Option[(SlotId, Coordinates) => IO[Unit]],
  addSkySlot:          Option[SlotId],
  resetSky:            Option[SlotId => IO[Unit]],
  isStaffOrAdmin:      Boolean,
  blindOffsetReadonly: Boolean
) extends ReactFnProps(AladinCell.component):
  val needsAGS: Boolean =
    obsConf.exists(_.needGuideStar)

  // This matters for non-sidereals - not sure what to default to.
  // Probably doesn't matters, since we don't do much if there isn't a configuration.
  val site: Site = obsConf.flatMap(_.configuration).flatMap(_.siteFor).getOrElse(Site.GN)

  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, obsConf.flatMap(_.posAngleConstraint))

  val anglesToTest: Option[NonEmptyList[Angle]] =
    for
      conf         <- obsConf
      paConstraint <- conf.posAngleConstraint
      angles       <-
        // For visual mode we want to default to PA 0 if needed e.g. average parallactic not available
        paConstraint
          .anglesToTestAt(obsConf.flatMap(_.averagePA).map(_.averagePA))
          .orElse(NonEmptyList.one(Angle.Angle0).some)
    // We sort the angles or we could end up in a loop where the angles are tested back and forth
    // This is rare but can happen if each angle finds an equivalent guide star
    yield angles.sorted(using Angle.AngleOrder)

  def durationAvailable: Boolean =
    obsConf.flatMap(_.obsDuration).isDefined

  def modeSelected: Boolean =
    obsConf.exists(_.configuration.isDefined)

  def selectedGSName: Option[NonEmptyString] =
    obsConf.flatMap(_.remoteGSName)

end AladinCell

trait AladinCommon:
  given Reusability[AgsState] = Reusability.byEq

  def userPrefsSetter(
    uid:                User.Id,
    showCatalog:        Option[Visible] = None,
    agsOverlay:         Option[Visible] = None,
    fullScreen:         Option[AladinFullScreen] = None,
    scienceOffsets:     Option[Visible] = None,
    acquisitionOffsets: Option[Visible] = None
  )(using Logger[IO], FetchClient[IO, UserPreferencesDB]): Callback =
    GlobalUserPreferences
      .storeAladinPreferences[IO](
        uid,
        showCatalog = showCatalog,
        agsOverlay = agsOverlay,
        scienceOffsets = scienceOffsets,
        acquisitionOffsets = acquisitionOffsets,
        fullScreen = fullScreen
      )
      .runAsync
      .void

object AladinCell extends ModelOptics with AladinCommon:
  import GuideStarSelection.*

  private type Props = AladinCell

  // only compare candidates by id
  private given Reusability[GuideStarCandidate] = Reusability.by(_.id)

  private val fovLens: Lens[AsterismVisualOptions, Fov] =
    Lens[AsterismVisualOptions, Fov](t => Fov(t.fovRA, t.fovDec)): f =>
      t => t.copy(fovRA = f.x, fovDec = f.y)

  val fullScreenIso: Iso[AladinFullScreen, UIFullScreen] =
    Iso[AladinFullScreen, UIFullScreen](x => UIFullScreen(x.value))(x => AladinFullScreen(x.value))

  private def offsetViews(
    uid:       User.Id,
    targetIds: NonEmptyList[Target.Id],
    options:   View[Pot[AsterismVisualOptions]],
    storeId:   Option[Int] => Callback = _ => Callback.empty
  )(ctx: AppContext[IO]): (Offset => Callback, ViewOpt[Offset]) = {
    import ctx.given

    val offsetView: ViewOpt[Offset] =
      options.zoom:
        Pot.readyPrism.andThen(AsterismVisualOptions.viewOffset)

    val offsetChangeInAladin = (newOffset: Offset) => {
      val ignore = options.get.fold(
        true,
        _ => true,
        o =>
          val diffP = newOffset.p.toAngle.difference(o.viewOffset.p.toAngle)
          val diffQ = newOffset.q.toAngle.difference(o.viewOffset.q.toAngle)
          // Don't save if the change is less than 1 arcse
          diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
      )

      offsetView.set(newOffset) *>
        AsterismPreferences
          .updateAladinPreferences[IO](
            options.get.toOption.flatMap(_.id),
            uid,
            targetIds,
            offset = newOffset.some
          )
          .flatMap(id => storeId(id).to[IO])
          .unlessA(ignore)
          .runAsync
          .rateLimit(1.seconds, 1)
          .void
    }

    // Always store the offset when centering
    val offsetOnCenter = offsetView.withOnMod:
      case o @ Some(_) =>
        AsterismPreferences
          .updateAladinPreferences[IO](
            options.get.toOption.flatMap(_.id),
            uid,
            targetIds,
            offset = o
          )
          .flatMap(id => storeId(id).to[IO])
          .void
          .runAsync
      case _           => Callback.empty

    (offsetChangeInAladin, offsetOnCenter)
  }

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx                 <- useContext(AppContext.ctx)
      trackingMapResult   <-
        useEffectResultWithDeps((props.obsTargets, props.obsTime, props.site)): (targets, at, s) =>
          import ctx.given
          // if there is a TOO, don't bother getting tracking
          if (targets.hasTargetOfOpportunity)
            RegionOrTrackingMap.Empty.asRight.pure
          else
            // get it for the full semester for visualization purposes, with
            // high resolution around the obsTime.
            getMixedResolutionRegionOrTrackingMap(targets.allTargets.toList, s, at)
      obsTargetsCoordsPot <- useMemo(
                               (props.obsTargets,
                                props.obsTime,
                                trackingMapResult.value.value,
                                props.obsConf.map(_.targetViz),
                                props.obsConf.flatMap(_.explicitBase)
                               )
                             ): (targets, at, trPot, targetViz, explicitBase) =>
                               // Generic instrument slot layout, resolved to obs-time coords inside
                               // ObservationTargetsCoordinatesAt alongside base/blind-offset coords.
                               val slots = targetViz.foldMap(_.slots)
                               trPot.map: tr =>
                                 if (targets.hasTargetOfOpportunity)
                                   ObservationTargetsCoordinatesAt.emptyAt(at)
                                 else
                                   tr.flatMap: map =>
                                     ObservationTargetsCoordinatesAt(at,
                                                                     targets,
                                                                     map,
                                                                     slots,
                                                                     explicitBase
                                     )
      oBaseTracking       <-
        useMemo((props.obsTargets, trackingMapResult.value.toOption.flatMap(_.toOption))):
          (obsTargets, trackings) =>
            // We should have trackings for all the targets, so we'll ignore errors here.
            trackings.flatMap(obsTargets.asterismTracking).flatMap(_.toOption)
      // Pending sky-position changes for optimistic updates, keyed by slot:
      optimisticSky       <- useStateView(SortedMap.empty[SlotId, Option[Coordinates]])
      // set of slots we currently have a position for.
      realSlots            = obsTargetsCoordsPot.value.toOption
                               .flatMap(_.toOption)
                               .fold(SortedSet.empty[SlotId])(c => SortedSet.from(c.slotCoords.keys))
      // reconcile local state with the remote values for slot assignments
      _                   <- useEffectWithDeps((optimisticSky.get, realSlots)): (pending, real) =>
                               def settled(slot: SlotId, expected: Option[Coordinates]): Boolean =
                                 expected.fold(!real.contains(slot))(_ => real.contains(slot))
                               val reconciled                                                    = pending.toList.collect:
                                 case (slot, expected) if settled(slot, expected) => slot
                               optimisticSky.mod(_ -- reconciled).whenA(reconciled.nonEmpty)
      candidates          <-
        useEffectResultWithDeps(
          (props.siderealDiscretizedObsTime,
           oBaseTracking,
           props.obsConf.flatMap(_.explicitBase),
           props.obsConf.flatMap(_.obsModeType),
           props.obsConf.flatMap(_.guideProbe),
           props.needsAGS
          )
        ):
          (
            siderealDiscretizedObsTime,
            oTracking,
            explicitBase,
            obsModeType,
            guideProbe,
            needsAGS
          ) =>
            import ctx.given

            // Prefer the explicit base override as the catalog search center
            val searchTracking: Option[Tracking] =
              explicitBase.map(Tracking.constant).orElse(oTracking.value)

            (obsModeType, searchTracking)
              .mapN: (_, baseTracking) =>
                if (needsAGS)
                  (for
                    _          <- props.obsConf
                                    .flatMap(_.agsState)
                                    .foldMap(_.async.set(AgsState.LoadingCandidates))
                    candidates <-
                      guideProbe.foldMap: gp =>
                        CatalogClient[IO]
                          .requestSingle:
                            CatalogMessage.GSRequest(
                              baseTracking.minimizeEphemeris(siderealDiscretizedObsTime.obsTime),
                              siderealDiscretizedObsTime.obsTime,
                              gp
                            )
                  yield candidates)
                    .guarantee:
                      props.obsConf
                        .flatMap(_.agsState)
                        .foldMap(_.async.set(AgsState.Idle))
                else none.pure
              .getOrElse(List.empty.some.pure)
      agsCalcProps        <- useMemo(
                               (props.obsTargets.focus.id,
                                props.obsTime,
                                props.obsConf.flatMap(_.constraints),
                                props.obsConf.flatMap(_.agsWavelength),
                                props.obsConf.flatMap(_.configuration),
                                props.obsConf.flatMap(_.obsModeType),
                                props.obsConf.flatMap(_.guidedAcqOffsets),
                                props.obsConf.flatMap(_.guidedSciOffsets),
                                candidates.value.toOption.flatten,
                                props.obsConf.flatMap(_.trackType)
                               )
                             ):
                               case (focusedId,
                                     obsTime,
                                     Some(constraints),
                                     Some(agsWavelength),
                                     observingMode,
                                     Some(obsModeType),
                                     acqOffsets,
                                     sciOffsets,
                                     Some(cands),
                                     trackType
                                   ) =>
                                 AgsCalcProps(
                                   focusedId,
                                   obsTime,
                                   constraints,
                                   agsWavelength,
                                   observingMode,
                                   obsModeType,
                                   acqOffsets,
                                   sciOffsets,
                                   cands,
                                   trackType
                                 ).some
                               case _ => none
      // Reference to root
      root                <- useMemo(())(_ => domRoot)
      // target options, will be read from the user preferences cache
      options             <- useStateView(
                               props.userPreferences.get.asterismPreferences
                                 .get(UserPreferences.AsterismKey.fromTargetIds(props.obsTargets.ids))
                                 .fold(pending[AsterismVisualOptions])(_.ready)
                             )
      _                   <- useEffectWithDeps((props.uid, props.obsTargets.ids)): (uid, tids) =>
                               import ctx.given

                               val key = UserPreferences.AsterismKey.fromTargetIds(tids)

                               def applyOptions(o: AsterismVisualOptions): Callback =
                                 options.set(o.ready) *>
                                   setVariable(root, "saturation", o.saturation) *>
                                   setVariable(root, "brightness", o.brightness)

                               props.userPreferences.get.asterismPreferences.get(key) match
                                 case Some(o) =>
                                   applyOptions(o)
                                 case None    =>
                                   options.set(pending[AsterismVisualOptions]) *>
                                     AsterismPreferences
                                       .queryAsterism[IO](uid, tids)
                                       .runAsyncAndThen:
                                         case Right(Some(o)) =>
                                           // try to read it from the db and send to cache
                                           props.userPreferences
                                             .zoom(UserPreferences.asterismVisualOptions(key))
                                             .set(o.some) *> applyOptions(o)
                                         case _              =>
                                           // if not found in db, use default and send to cache
                                           applyOptions(AsterismVisualOptions.Default)
      // Hold the mouse position on a SignallingRef instead of react state to avoid a re-rending loop.
      mouseSignal         <- useEffectResultOnMount(SignallingRef.of[IO, Option[Coordinates]](none))
      setMouseCoords      <- useCallbackWithDeps(mouseSignal.value.value.toOption.isDefined): _ =>
                               import ctx.given
                               (coords: Option[Coordinates]) =>
                                 mouseSignal.value.value.toOption.foldMap(_.set(coords).runAsync)
      _                   <- useEffectWithDeps(
                               (obsTargetsCoordsPot.value.toOption
                                  .flatMap(_.toOption)
                                  .flatMap(_.baseOrBlindCoords),
                                mouseSignal.value.value.toOption.isDefined
                               )
                             ): (coords, _) =>
                               setMouseCoords.value(coords)
      // Reset offset and gs if asterism change
      _                   <- useEffectWithDeps(props.obsTargets): targets =>
                               val (_, offsetOnCenter) = offsetViews(props.uid, targets.ids, options)(ctx)
                               // if the coordinates change, reset ags && offset
                               for
                                 _ <- props.guideStarSelection.set(GuideStarSelection.Default)
                                 _ <- offsetOnCenter.set(Offset.Zero)
                               yield ()
      // Reset selection if the angles to test change
      _                   <- useEffectWithDeps(props.anglesToTest): _ =>
                               (props.obsConf
                                 .flatMap(_.agsState)
                                 .foldMap(
                                   _.set(AgsState.Calculating)
                                 ) *>
                                 props.guideStarSelection
                                   .set(GuideStarSelection.Default))
                                 .whenA(
                                   // should check that the candidates list option is definde AND non empty
                                   props.needsAGS && candidates.value.toOption.flatten.exists(_.nonEmpty)
                                 )
      // request AGS calculation
      agsResults          <- useAgsCalculation(
                               obsTargetsCoordsPot.toOption.flatMap(_.toOption),
                               agsCalcProps.value,
                               props.anglesToTest,
                               props.obsConf.flatMap(_.posAngleConstraint).isDefined,
                               props.obsConf.flatMap(_.agsState),
                               props.guideStarSelection,
                               props.needsAGS
                             )(ctx)
      // In case the selected name changes remotely
      _                   <- useEffectWithDeps((props.selectedGSName, agsResults.constrained)): (n, resultsPot) =>
                               resultsPot.toOption.foldMap: results =>
                                 val newGss =
                                   n.fold(AgsSelection(results.headOption.tupleLeft(0))):
                                     results.pick
                                 props.guideStarSelection.set(newGss)
      menuRef             <- usePopupMenuRef
    } yield
      import ctx.given

      val fovView =
        options.zoom(Pot.readyPrism.andThen(fovLens))

      val globalPreferences = props.userPreferences.zoom(UserPreferences.globalPreferences)

      val fullScreenView =
        globalPreferences
          .zoom(GlobalPreferences.fullScreen)
          .withOnMod: v =>
            props.fullScreen.set(v) *> userPrefsSetter(props.uid, fullScreen = v.some)

      val coordinatesSetter =
        setMouseCoords.map(set => (c: Coordinates) => set(c.some))

      val asterismKey = UserPreferences.AsterismKey.fromTargetIds(props.obsTargets.ids)

      // Update only the `id` field, on the current state
      def storePrefsId(newId: Option[Int]): Callback =
        options.mod(_.map(_.copy(id = newId))) *>
          props.userPreferences
            .zoom(UserPreferences.asterismVisualOptions(asterismKey))
            .mod(_.map(_.copy(id = newId)))

      val fovSetter = (newFov: Fov) => {
        val ignore = options.get.fold(
          true,
          _ => true,
          o =>
            // Don't save if the change is less than 1 arcse
            o.fov.isDifferentEnough(newFov)
        )
        if (newFov.x.toMicroarcseconds === 0L) Callback.empty
        else
          fovView.set(newFov) *>
            AsterismPreferences
              .updateAladinPreferences[IO](
                options.get.toOption.flatMap(_.id),
                props.uid,
                props.obsTargets.ids,
                newFov.x.some,
                newFov.y.some
              )
              .flatMap(id => storePrefsId(id).to[IO])
              .unlessA(ignore)
              .runAsync
              .rateLimit(1.seconds, 1)
              .void

      }

      val (offsetChangeInAladin, offsetOnCenter) =
        offsetViews(props.uid, props.obsTargets.ids, options, storePrefsId)(ctx)

      val guideStar = props.guideStarSelection.get.analysis

      val agsResultsList = agsResults.constrained.toOption.getOrElse(List.empty)

      // Apply the optimistic sky changes.
      def mergedCoords(
        obsCoords: ObservationTargetsCoordinatesAt
      ): ObservationTargetsCoordinatesAt =
        optimisticSky.get.foldLeft(obsCoords): (acc, entry) =>
          val (slot, oc) = entry
          oc match
            case Some(c) =>
              if acc.slotCoords.contains(slot) then acc
              else acc.copy(slots = acc.slots.updated(slot, SlotInfo(c, None)))
            case None    =>
              acc.copy(slots = acc.slots.removed(slot))

      def renderAladin(
        opts:        AsterismVisualOptions,
        trackingMap: RegionOrTrackingMap,
        realCoords:  ObservationTargetsCoordinatesAt // subscription coords
      ): VdomNode =
        val assignSkyOptimistic: Option[(SlotId, Coordinates) => IO[Unit]] =
          props.assignSky.map: assign =>
            (slot, c) =>
              optimisticSky.mod(_.updated(slot, c.some)).to[IO] *>
                assign(slot, c).onError { case _ => optimisticSky.mod(_.removed(slot)).to[IO] }

        // Slots whose assignment is still in flight
        val pendingSlots: Set[SlotId] = optimisticSky.get.keySet

        val mergedForMarker: ObservationTargetsCoordinatesAt = mergedCoords(realCoords)

        // Build clickable regions for the aladin component, in practice the only one so far is ghost ifu2 sky.
        val interactiveRegions: List[InteractiveRegion] =
          InteractiveRegion.forViz(
            props.obsConf.flatMap(ConfigurationForVisualization.fromObsConfiguration),
            mergedForMarker,
            props.allTargets.get.get(_).exists(_.isTargetOfOpportunity),
            guideStar,
            assignSkyOptimistic
          )
        AladinContainer(
          props.obsTargets,
          props.obsTime,
          props.obsConf.flatMap(_.obsDuration),
          trackingMap,
          mergedForMarker,
          props.obsConf.flatMap(ConfigurationForVisualization.fromObsConfiguration),
          globalPreferences.get,
          opts,
          coordinatesSetter,
          mouseSignal.value.value.toOption,
          interactiveRegions,
          pendingSlots,
          fovSetter,
          offsetChangeInAladin.reuseAlways,
          guideStar,
          agsResults,
          props.anglesToTest,
          props.obsConf.flatMap(_.agsState).map(_.get),
          props.isStaffOrAdmin
        )

      val renderToolbar: (AsterismVisualOptions) => VdomNode =
        (t: AsterismVisualOptions) =>
          val agsState = props.obsConf
            .flatMap(_.agsState.map(_.get))
            .getOrElse(AgsState.Idle)
          mouseSignal.value.value.toOption.map: signal =>
            AladinToolbar(
              Fov(t.fovRA, t.fovDec),
              signal,
              agsState,
              guideStar,
              globalPreferences.get.agsOverlay,
              offsetOnCenter
            )

      val renderAgsOverlay: AsterismVisualOptions => VdomNode =
        (_: AsterismVisualOptions) =>
          if (props.needsAGS && globalPreferences.get.agsOverlay)
            props.obsConf
              .flatMap(_.agsState)
              .map: agsState =>
                <.div(
                  ExploreStyles.AgsOverlay |+| ExploreStyles.VisualizationStale
                    .when_(agsState.get === AgsState.Calculating),
                  AgsOverlay(
                    props.guideStarSelection,
                    agsResultsList.filter(_.isUsable),
                    agsState.get,
                    props.modeSelected,
                    props.durationAvailable,
                    candidates.value.value.nonEmpty
                  )
                )
          else EmptyVdom

      val renderAddSkyModeOverlay: VdomNode =
        if (props.addSkySlot.isDefined && props.assignSky.isDefined)
          <.div(ExploreStyles.AddSkyModeOverlay,
                "Click in the shaded are to set the sky pos (Esc to cancel)"
          )
        else EmptyVdom

      val renderBlindOffsetControl =
        (oBaseTracking.value, props.blindOffsetInfo).mapN: (bt, boInfo) =>
          BlindOffsetControl(
            boInfo._1,
            boInfo._2,
            props.obsTime,
            bt,
            props.obsTargets,
            props.allTargets,
            props.blindOffsetReadonly
          )

      <.div(ExploreStyles.TargetAladinCell)(
        (trackingMapResult.value.value, obsTargetsCoordsPot.value).tupled.renderPot: (etr, eco) =>
          (etr, eco).tupled.fold(
            err => Message(severity = Message.Severity.Error, text = err),
            (tr, co) =>
              React.Fragment(
                <.div(
                  ExploreStyles.AladinContainerColumn,
                  AladinFullScreenControl(fullScreenView.zoom(fullScreenIso)),
                  <.div(
                    ExploreStyles.AladinToolbox,
                    Button(onClickE = menuRef.toggle).withMods(
                      ExploreStyles.ButtonOnAladin,
                      Icons.ThinSliders
                    )
                  ),
                  options.get.renderPot(opt =>
                    React.Fragment(renderAladin(opt, tr, co),
                                   renderToolbar(opt),
                                   renderAgsOverlay(opt),
                                   renderAddSkyModeOverlay
                    )
                  )
                ),
                renderBlindOffsetControl
              )
          ),
        options
          .zoom(Pot.readyPrism[AsterismVisualOptions])
          .mapValue: options =>
            AladinPreferencesMenu(
              props.uid,
              props.obsTargets.ids,
              globalPreferences,
              options,
              menuRef,
              props.isStaffOrAdmin
            )
      )
