// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.PotOption
import crystal.implicits._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.UserPreferencesQueries._
import explore.components.ui.ExploreStyles
import explore.events._
import explore.implicits._
import explore.model.Constants
import explore.model.ObsConfiguration
import explore.model.TargetVisualOptions
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle._
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle._
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability._
import explore.optics.ModelOptics
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags._
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL._
import react.aladin.Fov
import react.common.ReactFnProps
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.sizes._

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration._

final case class AladinCell(
  uid:              User.Id,
  tid:              Target.Id,
  obsConf:          ObsConfiguration,
  target:           View[SiderealTracking],
  fullScreen:       View[Boolean]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AladinCell](AladinCell.component)

final case class AladinSettings(showMenu: Boolean, showCatalog: Boolean)

object AladinSettings {
  val Default = AladinSettings(false, false)
}

object AladinCell extends ModelOptics {
  type Props = AladinCell

  val params  = AgsParams.GmosAgsParams(none, PortDisposition.Side)
  val basePos = AgsPosition(Angle.Angle0, Offset.Zero)

  // We want to re render only when the vizTime changes at least a month
  // We keep the candidates data pm corrected for the viz time
  // If it changes over a month we'll request the data again and recalculate
  // This way we avoid recalculating pm for example if only pos angle or
  // conditions change
  given Reusability[Instant] = Reusability {
    Duration.between(_, _).toDays().abs < 30L
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // mouse coordinates, starts on the base
      .useStateBy(_.target.get.baseCoordinates)
      // target options, will be read from the user preferences
      .useStateView(Pot.pending[TargetVisualOptions])
      // flag to trigger centering. This is a bit brute force but
      // avoids us needing a ref to a Fn component
      .useStateView(false)
      // to get faster reusability use a serial state, rather than check every candidate
      .useSerialState(List.empty[GuideStarCandidate])
      // Analysis results
      .useSerialState(List.empty[AgsAnalysis])
      // Ags state
      .useState[AgsState](AgsState.Idle)
      // Request data again if vizTime changes more than a month
      .useEffectWithDepsBy((p, _, _, _, _, _, _) => p.obsConf.vizTime) {
        (props, _, _, _, gs, _, agsState) => vizTime =>
          implicit val ctx = props.ctx

          agsState.setStateAsync(AgsState.LoadingCandidates) >>
            CatalogClient[IO].requestSingle(CatalogMessage.GSRequest(props.target.get, vizTime)) >>=
            (_.map(candidates =>
              agsState.setState(AgsState.Idle).to[IO] >> gs.setStateAsync(candidates)
            ).orEmpty)
      }
      .useEffectWithDepsBy((p, _, _, _, _, _, _) => (p.uid, p.tid)) {
        (props, _, options, _, _, _, _) => _ =>
          implicit val ctx = props.ctx

          UserTargetPreferencesQuery
            .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
            .flatMap { (fov, viewOffset, agsCandidates, agsOverlay, fullScreen) =>
              options
                .set(
                  TargetVisualOptions.Default
                    .copy(fovAngle = fov,
                          viewOffset = viewOffset,
                          agsCandidates = agsCandidates,
                          agsOverlay = agsOverlay,
                          fullScreen = fullScreen
                    )
                    .ready
                )
                .to[IO] *> props.fullScreen.set(fullScreen).to[IO]
            }
      }
      // Selected GS index. Should be stored in the db
      .useStateView(none[Int])
      // Request ags calculation
      .useEffectWithDepsBy((p, _, _, _, candidates, _, _, _) =>
        (p.target.get,
         p.obsConf.posAngleConstraint,
         p.obsConf.constraints,
         p.obsConf.wavelength,
         p.obsConf.vizTime,
         candidates.value
        )
      ) { (props, _, _, _, _, ags, agsState, selectedIndex) =>
        {
          case (tracking,
                Some(posAngle),
                Some(constraints),
                Some(wavelength),
                vizTime,
                candidates
              ) =>
            implicit val ctx = props.ctx

            val pa = posAngle match {
              case PosAngleConstraint.Fixed(a)               => a.some
              case PosAngleConstraint.AllowFlip(a)           => a.some
              case PosAngleConstraint.ParallacticOverride(a) => a.some
              case _                                         => none
            }

            (tracking.at(vizTime), pa).mapN { (base, pa) =>
              val basePos = AgsPosition(pa, Offset.Zero)

              for {
                _ <- selectedIndex.async.set(none)
                _ <- agsState.setStateAsync(AgsState.Calculating)
                _ <- AgsClient[IO]
                       .requestSingle(
                         AgsMessage.Request(props.tid,
                                            constraints,
                                            wavelength,
                                            base,
                                            List(base),
                                            basePos,
                                            params,
                                            candidates
                         )
                       )
                       .flatMap(
                         _.map(r =>
                           agsState.setStateAsync(AgsState.Idle) *> ags.setStateAsync(r)
                         ).orEmpty
                       )
                       .unlessA(candidates.isEmpty)
                       .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
              } yield ()
            }.orEmpty
          case _ => IO.unit
        }
      }
      // open settings menu
      .useState(false)
      // Reset the selected gs if results chage
      .useEffectWithDepsBy((p, _, _, _, _, agsResults, _, _, _) => (agsResults, p.obsConf)) {
        (p, _, _, _, _, agsResults, agsState, selectedIndex, _) => _ =>
          selectedIndex
            .set(
              0.some.filter(_ => agsResults.value.nonEmpty && p.obsConf.canSelectGuideStar)
            )
            .unless_(agsState.value === AgsState.Calculating)
      }
      .render {
        (
          props,
          mouseCoords,
          options,
          center,
          _,
          agsResults,
          agsState,
          selectedGSIndex,
          openSettings
        ) =>
          implicit val ctx = props.ctx

          val agsCandidatesView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.agsCandidates))

          val agsOverlayView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.agsOverlay))

          val fovView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.fovAngle))

          val offsetView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.viewOffset))

          val fullScreenView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.fullScreen))

          val agsCandidatesShown: Boolean = agsCandidatesView.get.map(_.visible).getOrElse(false)

          val agsOverlayShown: Boolean = agsOverlayView.get.map(_.visible).getOrElse(false)

          val coordinatesSetter =
            ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

          val fovSetter = (newFov: Fov) => {
            val ignore = options.get.fold(
              true,
              _ => true,
              o =>
                // Don't save if the change is less than 1 arcse
                (o.fovAngle.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e6
            )
            if (newFov.x.toMicroarcseconds === 0L) Callback.empty
            else {
              fovView.set(newFov.x) *>
                (fovView.get, agsCandidatesView.get, agsOverlayView.get, fullScreenView.get).mapN {
                  (_, a, o, f) =>
                    UserTargetPreferencesUpsert
                      .updateAladinPreferences[IO](props.uid, props.tid, newFov.x, a, o, f)
                      .unlessA(ignore)
                      .runAsync
                      .rateLimit(1.seconds, 1)
                      .void
                }.orEmpty
            }
          }

          val offsetSetter = (newOffset: Offset) => {
            val ignore = options.get.fold(
              true,
              _ => true,
              o => {
                val diffP = newOffset.p.toAngle.difference(o.viewOffset.p.toAngle)
                val diffQ = newOffset.q.toAngle.difference(o.viewOffset.q.toAngle)
                // Don't save if the change is less than 1 arcse
                diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
              }
            )

            offsetView.set(newOffset) *>
              UserTargetPreferencesFovUpdate
                .updateViewOffset[IO](props.uid, props.tid, newOffset)
                .unlessA(ignore)
                .runAsync
                .rateLimit(1.seconds, 1)
                .void
          }

          def prefsSetter(
            candidates: Visible => Visible,
            overlay:    Visible => Visible,
            fullScreen: Boolean => Boolean
          ): Callback =
            (fovView.get, agsCandidatesView.get, agsOverlayView.get, fullScreenView.get).mapN {
              (f, a, o, s) =>
                UserTargetPreferencesUpsert
                  .updateAladinPreferences[IO](
                    props.uid,
                    props.tid,
                    f,
                    candidates(a),
                    overlay(o),
                    fullScreen(s)
                  )
                  .runAsync
                  .void
            }.orEmpty

          def agsOverlaySetter: Callback =
            agsOverlayView.mod(_.flip) *>
              prefsSetter(identity, _.flip, identity)

          def candidatesSetter: Callback =
            agsCandidatesView.mod(_.flip) *>
              prefsSetter(_.flip, identity, identity)

          def fullScreenSetter: Callback =
            props.fullScreen.mod(!_) *>
              fullScreenView.mod(!_) *>
              prefsSetter(identity, identity, !_)

          val aladinKey = s"${props.target.get}"

          val selectedGuideStar = selectedGSIndex.get.flatMap(agsResults.value.lift)
          val usableGuideStar   = selectedGuideStar.exists(_.isUsable)

          val renderCell: TargetVisualOptions => VdomNode = (t: TargetVisualOptions) =>
            AladinContainer(
              props.target,
              props.obsConf,
              t.copy(fullScreen = props.fullScreen.get),
              coordinatesSetter,
              fovSetter.reuseAlways,
              offsetSetter.reuseAlways,
              center,
              selectedGuideStar,
              agsResults.value
            ).withKey(aladinKey)

          val renderToolbar: TargetVisualOptions => VdomNode =
            (t: TargetVisualOptions) =>
              AladinToolbar(Fov.square(t.fovAngle),
                            mouseCoords.value,
                            agsState.value,
                            selectedGuideStar.map(_.target),
                            center,
                            t.agsOverlay
              ): VdomNode

          val renderAgsOverlay: TargetVisualOptions => VdomNode =
            (t: TargetVisualOptions) =>
              if (t.agsOverlay.visible && usableGuideStar) {
                <.div(
                  ExploreStyles.AgsOverlay,
                  AgsOverlay(
                    selectedGSIndex,
                    agsResults.value.count(_.isUsable),
                    selectedGuideStar
                  )
                )
              } else EmptyVdom

          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              Button(size = Small, icon = true, onClick = fullScreenSetter)(
                ExploreStyles.AladinFullScreenButton,
                Icons.ExpandDiagonal.unless(props.fullScreen.get),
                Icons.ContractDiagonal.when(props.fullScreen.get)
              ),
              <.div(
                ExploreStyles.AladinToolbox,
                Button(size = Small, icon = true, onClick = openSettings.modState(!_))(
                  ExploreStyles.ButtonOnAladin,
                  ^.onMouseEnter --> openSettings.setState(true),
                  Icons.ThinSliders
                ),
                Menu(vertical = true,
                     compact = true,
                     size = Mini,
                     clazz = ExploreStyles.AladinSettingsMenu
                )(
                  ^.onMouseLeave --> openSettings.setState(false),
                  MenuItem(
                    Checkbox(
                      label = "Show Catalog",
                      checked = agsCandidatesShown,
                      onChange = (_: Boolean) => openSettings.setState(false) *> candidatesSetter
                    )
                  ),
                  MenuItem(
                    Checkbox(
                      label = "AGS",
                      checked = agsOverlayShown,
                      onChange = (_: Boolean) => openSettings.setState(false) *> agsOverlaySetter
                    )
                  )
                ).when(openSettings.value)
              ),
              potRenderView[TargetVisualOptions](renderCell)(options),
              potRenderView[TargetVisualOptions](renderToolbar)(options),
              potRenderView[TargetVisualOptions](renderAgsOverlay)(options)
            )
          )
      }

}
