// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.Visible
import explore.optics.ModelOptics
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.*
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenuRef
import lucuma.react.primereact.PopupTieredMenu
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

import scala.scalajs.LinkingInfo

case class AladinPreferencesMenu(
  uid:               User.Id,
  tids:              NonEmptyList[Target.Id],
  globalPreferences: View[GlobalPreferences],
  targetPreferences: View[AsterismVisualOptions],
  menuRef:           PopupMenuRef
) extends ReactFnProps(AladinPreferencesMenu.component)

object AladinPreferencesMenu extends ModelOptics with AladinCommon:

  private type Props = AladinPreferencesMenu

  private val unsafeRangeLens: Lens[AsterismVisualOptions.ImageFilterRange, Double] =
    Lens[AsterismVisualOptions.ImageFilterRange, Double](_.value.toDouble)(x =>
      y =>
        refineV[AsterismVisualOptions.FilterRange](x.toInt).toOption
          .getOrElse(y) // Ignore invalid updates
    )

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx  <- useContext(AppContext.ctx)
        root <- useMemo(())(_ => domRoot)
      yield
        import ctx.given

        def prefsSetter(
          saturation: Option[Int] = None,
          brightness: Option[Int] = None
        ): Callback =
          AsterismPreferences
            .updateAladinPreferences[IO](
              props.targetPreferences.get.id,
              props.uid,
              props.tids,
              saturation = saturation,
              brightness = brightness
            )
            .flatMap(id => props.targetPreferences.zoom(AsterismVisualOptions.id).set(id).to[IO])
            .runAsync
            .void

        def visiblePropView(
          get:   Lens[GlobalPreferences, Visible],
          onMod: Visible => Callback
        ) =
          props.globalPreferences
            .zoom(get)
            .withOnMod(onMod)
            .as(Visible.Value)

        val agsCandidatesView =
          visiblePropView(
            GlobalPreferences.showCatalog,
            v => userPrefsSetter(props.uid, showCatalog = v.some)
          )

        val agsOverlayView =
          visiblePropView(
            GlobalPreferences.agsOverlay,
            v => userPrefsSetter(props.uid, agsOverlay = v.some)
          )

        val scienceOffsetsView =
          visiblePropView(
            GlobalPreferences.scienceOffsets,
            v => userPrefsSetter(props.uid, scienceOffsets = v.some)
          )

        val acquisitionOffsetsView =
          visiblePropView(
            GlobalPreferences.acquisitionOffsets,
            v => userPrefsSetter(props.uid, acquisitionOffsets = v.some)
          )

        def cssVarView(
          varLens:        Lens[AsterismVisualOptions, AsterismVisualOptions.ImageFilterRange],
          variableName:   String,
          updateCallback: Int => Callback
        ) =
          props.targetPreferences
            .zoom(varLens)
            .withOnMod(s => setVariable(root, variableName, s) *> updateCallback(s))

        val saturationView =
          cssVarView(AsterismVisualOptions.saturation,
                     "saturation",
                     s => prefsSetter(saturation = s.some)
          )
        val brightnessView =
          cssVarView(AsterismVisualOptions.brightness,
                     "brightness",
                     s => prefsSetter(brightness = s.some)
          )

        val allowMouseZoomView =
          props.globalPreferences
            .zoom(GlobalPreferences.aladinMouseScroll)
            .withOnMod(z =>
              GlobalUserPreferences.storeAladinPreferences[IO](props.uid, z.some).runAsync
            )

        val pfView = props.globalPreferences.zoom(GlobalPreferences.pfVisibility)

        val showBaseView            = pfView.zoom(AGSDebugVisibility.showBase).as(Visible.Value)
        val showBlindOffsetView     = pfView.zoom(AGSDebugVisibility.showBlindOffset).as(Visible.Value)
        val showSciOffsetView       = pfView.zoom(AGSDebugVisibility.showScienceOffset).as(Visible.Value)
        val showAcqOffsetView       = pfView.zoom(AGSDebugVisibility.showAcquisitionOffset).as(Visible.Value)
        val showIntersectionView    = pfView.zoom(AGSDebugVisibility.showIntersection).as(Visible.Value)
        val showAllAngles           = pfView.zoom(AGSDebugVisibility.showAllAngles).as(Visible.Value)
        val showAllCatalogStarsView = pfView.zoom(AGSDebugVisibility.showAllCatalogStars).as(Visible.Value)

        def menuItem(content: VdomNode): MenuItem =
          MenuItem.Custom(
            <.div(^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB))(content)
          )

        val menuItems = List(
          menuItem(
            CheckboxView(
              id = "ags-candidates".refined,
              value = agsCandidatesView,
              label = "Show Catalog"
            )
          ),
          menuItem(
            CheckboxView(
              id = "ags-overlay".refined,
              value = agsOverlayView,
              label = "AGS"
            )
          ),
          menuItem(
            CheckboxView(
              id = "science-offsets".refined,
              value = scienceOffsetsView,
              label = "Sci. Offsets"
            )
          ),
          menuItem(
            CheckboxView(
              id = "acq-offsets".refined,
              value = acquisitionOffsetsView,
              label = "Acq. Offsets"
            )
          ),
          MenuItem.Separator
        ) ++
          Option
            .when(LinkingInfo.developmentMode)(
              List(
                MenuItem.SubMenu(
                  label = "Patrol Fields",
                  expanded = false,
                  icon = Icons.ObjectIntersect
                )(
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-base".refined,
                      value = showBaseView,
                      label = <.span(ExploreStyles.PatrolFieldBase, "Base")
                    )
                  ),
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-blind".refined,
                      value = showBlindOffsetView,
                      label = <.span(ExploreStyles.PatrolFieldBlindOffset, "Blind Offset")
                    )
                  ),
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-acq".refined,
                      value = showAcqOffsetView,
                      label = <.span(ExploreStyles.PatrolFieldAcqOffset, "Acq. Offset")
                    )
                  ),
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-sci".refined,
                      value = showSciOffsetView,
                      label = <.span(ExploreStyles.PatrolFieldSciOffset, "Sci. Offset")
                    )
                  ),
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-intersection".refined,
                      value = showIntersectionView,
                      label = <.span(ExploreStyles.PatrolFieldIntersection, "Intersection")
                    )
                  ),
                  MenuItem.Separator,
                  menuItem(
                    CheckboxView(
                      id = "patrol-field-all-angles".refined,
                      value = showAllAngles,
                      label = "All PAs"
                    )
                  )
                ),
                MenuItem.SubMenu(
                  label = "Candidates",
                  expanded = false,
                  icon = Icons.Stars
                )(
                  menuItem(
                    CheckboxView(
                      id = "all-catalog-stars".refined,
                      value = showAllCatalogStarsView,
                      label = "All Catalog Stars"
                    )
                  )
                ),
                MenuItem.Separator
              )
            )
            .toList
            .flatten ++
          List(
            menuItem(
              SliderView(
                id = "saturation".refined,
                label = "Saturation",
                clazz = ExploreStyles.AladinRangeControl,
                value = saturationView
                  .zoom(unsafeRangeLens)
              )
            ),
            menuItem(
              SliderView(
                id = "brightness".refined,
                label = "Brightness",
                clazz = ExploreStyles.AladinRangeControl,
                value = brightnessView.zoom(unsafeRangeLens)
              )
            ),
            MenuItem.Separator,
            menuItem(
              CheckboxView(
                id = "allow-zoom".refined,
                value = allowMouseZoomView.as(AladinMouseScroll.Value),
                label = "Scroll to zoom"
              )
            )
          )

        PopupTieredMenu(model = menuItems, clazz = ExploreStyles.AladinSettingsMenu)
          .withRef(props.menuRef.ref)
