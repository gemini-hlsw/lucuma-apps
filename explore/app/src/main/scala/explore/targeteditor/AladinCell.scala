// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

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
import explore.Icons
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.InteractiveRegion
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.given
import explore.optics.ModelOptics
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.SlotId
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
  trackingMap:         Pot[ErrorMsgOr[RegionOrTrackingMap]],
  ags:                 AgsData,
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

  val anglesToTest: Option[NonEmptyList[Angle]] =
    obsConf.flatMap(_.anglesToTest)

  def durationAvailable: Boolean =
    obsConf.flatMap(_.obsDuration).isDefined

  def modeSelected: Boolean =
    obsConf.exists(_.configuration.isDefined)

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
      obsTargetsCoordsPot <- useMemo(
                               (props.obsTargets,
                                props.obsTime,
                                props.trackingMap,
                                props.obsConf.map(_.targetViz),
                                props.obsConf.flatMap(_.explicitBase)
                               )
                             ): (targets, at, trPot, targetViz, explicitBase) =>
                               // Generic instrument slot layout, resolved to obs-time coords inside
                               // ObservationTargetsCoordinatesAt alongside base/blind-offset coords.
                               val slots = targetViz.foldMap(_.slots)
                               trPot.map: tr =>
                                 if (targets.hasUnresolvedTargetOfOpportunity)
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
        useMemo((props.obsTargets, props.trackingMap.toOption.flatMap(_.toOption))):
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
      // Reset the offset if the asterism changes. The guide star is reset by AGS itself.
      _                   <- useEffectWithDeps(props.obsTargets): targets =>
                               val (_, offsetOnCenter) = offsetViews(props.uid, targets.ids, options)(ctx)
                               offsetOnCenter.set(Offset.Zero)
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
            // Don't save if the change is less than 10 arcsec on both axes
            o.fov.isCloseTo(newFov)
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

      val agsResults     = props.ags.results
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
            props.allTargets.get.get(_).exists(_.isUnresolvedTargetOfOpportunity),
            guideStar,
            assignSkyOptimistic
          )

        // While the Base Position is armed, any click in the field sets it.
        val clickAnywhere: Option[Coordinates => IO[Unit]] =
          props.assignSky.flatMap: assign =>
            Option.when(props.addSkySlot.contains(SlotId.Base)): (c: Coordinates) =>
              assign(SlotId.Base, c)
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
          clickAnywhere,
          pendingSlots,
          fovSetter,
          offsetChangeInAladin.reuseAlways,
          guideStar,
          agsResults,
          props.anglesToTest,
          props.obsConf.flatMap(_.agsState).map(_.get),
          props.isStaffOrAdmin,
          baseExplicit = props.obsConf.flatMap(_.explicitBase).isDefined
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
                    props.ags.candidates.isReady
                  )
                )
          else EmptyVdom

      val renderAddPositionOverlay: VdomNode =
        // Banner for the shared "click to place" mode.
        props.addSkySlot match
          case Some(SlotId.Base)                    =>
            <.div(ExploreStyles.AddPositionOverlay,
                  "Click anywhere to set the Base Position (Esc to cancel)"
            )
          case Some(_) if props.assignSky.isDefined =>
            <.div(ExploreStyles.AddPositionOverlay,
                  "Click in the shaded area to set the sky position (Esc to cancel)"
            )
          case _                                    =>
            EmptyVdom

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
        (props.trackingMap, obsTargetsCoordsPot.value).tupled.renderPot: (etr, eco) =>
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
                                   renderAddPositionOverlay
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
