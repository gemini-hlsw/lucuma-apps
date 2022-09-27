// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ConstraintsQueries._
import explore.components.HelpIcon
import explore.components.Tile
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.Help
import explore.model.display._
import explore.undo.UndoContext
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.validation.ModelValidators
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Lens
import react.common.ReactFnProps
import react.primereact.PrimeStyles
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.LabelPointing

final case class ConstraintsPanel(
  obsIds:        List[Observation.Id],
  constraintSet: View[ConstraintSet],
  undoStacks:    View[UndoStacks[IO, ConstraintSet]],
  renderInTitle: Tile.RenderInTitle
)(using val ctx: AppContextIO)
    extends ReactFnProps[ConstraintsPanel](ConstraintsPanel.component)

object ConstraintsPanel {
  type Props = ConstraintsPanel

  private enum ElevationRangeType(val label: String):
    case AirMass   extends ElevationRangeType("Air Mass")
    case HourAngle extends ElevationRangeType("Hour Angle")

  private object ElevationRangeType:
    given Enumerated[ElevationRangeType] = Enumerated.from(AirMass, HourAngle).withTag(_.label)
    given Display[ElevationRangeType]    = Display.byShortName(_.label)

  import ElevationRangeType._

  private final case class ElevationRangeOptions(
    rangeType: ElevationRangeType,
    airMass:   ElevationRange.AirMass,
    hourAngle: ElevationRange.HourAngle
  ) {
    def toElevationRange(er: ElevationRange): ElevationRangeOptions =
      er match {
        case am @ ElevationRange.AirMass(_, _)   =>
          copy(rangeType = ElevationRangeType.AirMass, airMass = am)
        case ha @ ElevationRange.HourAngle(_, _) =>
          copy(rangeType = ElevationRangeType.HourAngle, hourAngle = ha)
      }
  }

  private object ElevationRangeOptions {
    def fromElevationRange(er: ElevationRange): ElevationRangeOptions =
      ElevationRangeOptions(
        ElevationRangeType.AirMass,
        ElevationRange.AirMass.Default,
        ElevationRange.HourAngle.Default
      ).toElevationRange(er)
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(props =>
        ElevationRangeOptions.fromElevationRange(props.constraintSet.get.elevationRange)
      )
      .useEffectWithDepsBy((props, _) => props.constraintSet.get.elevationRange)(
        (_, elevationRangeOptions) =>
          elevationRange => elevationRangeOptions.modState(_.toElevationRange(elevationRange))
      )
      .render { (props, elevationRangeOptions) =>
        import props.given

        val undoCtx: UndoContext[ConstraintSet] = UndoContext(props.undoStacks, props.constraintSet)

        val undoViewSet = UndoView(props.obsIds, undoCtx)

        val erView =
          undoViewSet(ConstraintSet.elevationRange, UpdateConstraintSet.elevationRange)

        def selectEnum[A: Enumerated: Display](
          label:     NonEmptyString,
          helpId:    Help.Id,
          lens:      Lens[ConstraintSet, A],
          remoteSet: A => ConstraintSetInput => ConstraintSetInput
        ) = {
          val id = NonEmptyString.unsafeFrom(label.value.toLowerCase().replaceAll(" ", "-"))
          React.Fragment(FormLabel(htmlFor = id)(label, HelpIcon(helpId)),
                         EnumDropdownView(id = id, value = undoViewSet(lens, remoteSet))
          )

        }

        val erTypeView: View[ElevationRangeType] =
          View[ElevationRangeType](
            elevationRangeOptions.value.rangeType,
            (mod, cb) =>
              erView
                .setCB(
                  mod(elevationRangeOptions.value.rangeType) match {
                    case AirMass   => elevationRangeOptions.value.airMass
                    case HourAngle => elevationRangeOptions.value.hourAngle
                  },
                  _ match {
                    case ElevationRange.AirMass(_, _)   => cb(AirMass)
                    case ElevationRange.HourAngle(_, _) => cb(HourAngle)
                  }
                )
          )

        val airMassView: View[ElevationRange.AirMass] =
          View[ElevationRange.AirMass](
            elevationRangeOptions.value.airMass,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.airMass)
                .modCB(mod, _.map(cb).orEmpty)
          )

        val hourAngleView: View[ElevationRange.HourAngle] =
          View[ElevationRange.HourAngle](
            elevationRangeOptions.value.hourAngle,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.hourAngle)
                .modCB(mod, _.map(cb).orEmpty)
          )

        React.Fragment(
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx))
          ),
          <.div(ExploreStyles.ConstraintsGrid)(
            selectEnum(
              "Image Quality".refined,
              "constraints/main/iq.md".refined,
              ConstraintSet.imageQuality,
              UpdateConstraintSet.imageQuality
            ),
            selectEnum(
              "Cloud Extinction".refined,
              "constraints/main/ce.md".refined,
              ConstraintSet.cloudExtinction,
              UpdateConstraintSet.cloudExtinction
            ),
            selectEnum(
              "Water Vapor".refined,
              "constraints/main/wv.md".refined,
              ConstraintSet.waterVapor,
              UpdateConstraintSet.waterVapor
            ),
            selectEnum(
              "Sky Background".refined,
              "constraints/main/sb.md".refined,
              ConstraintSet.skyBackground,
              UpdateConstraintSet.skyBackground
            ),
            FormLabel("ertype".refined)(
              "Elevation Range",
              HelpIcon("constraints/main/er.md".refined)
            ),
            <.div(
              ExploreStyles.ConstraintsElevationRangeGroup,
              EnumDropdownView(
                id = "ertype".refined,
                value = erTypeView,
                clazz = ExploreStyles.ElevationRangePicker
              ),
              React
                .Fragment(
                  <.label("Min"),
                  FormInputTextView(
                    id = "minam".refined,
                    value = airMassView.zoom(ElevationRange.AirMass.min),
                    validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.lte(elevationRangeOptions.value.airMass.max,
                                           "Must be <= Max".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry
                  ),
                  <.label("Max"),
                  FormInputTextView(
                    id = "maxam".refined,
                    value = airMassView.zoom(ElevationRange.AirMass.max),
                    validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.gte(elevationRangeOptions.value.airMass.min,
                                           "Must be >= Min".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry
                  )
                )
                .when(elevationRangeOptions.value.rangeType === AirMass),
              React
                .Fragment(
                  <.label("Min"),
                  FormInputTextView(
                    id = "minha".refined,
                    value = hourAngleView.zoom(ElevationRange.HourAngle.minHours),
                    validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.lte(
                        elevationRangeOptions.value.hourAngle.maxHours,
                        "Must be <= Max".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry
                  ),
                  <.label("Max"),
                  FormInputTextView(
                    id = "maxha".refined,
                    value = hourAngleView.zoom(ElevationRange.HourAngle.maxHours),
                    validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.gte(
                        elevationRangeOptions.value.hourAngle.minHours,
                        "Must be >= Min".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry
                  ),
                  <.label("Hours")
                )
                .when(elevationRangeOptions.value.rangeType === HourAngle)
            )
          )
        )
      }
}
