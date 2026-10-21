// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.TimeChargeRow
import explore.model.VisitTimeCharge
import explore.model.inputCodecs
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Visit
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Divider
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.ui.format.DurationSpacedFormatter
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.effect.*

import java.util.concurrent.TimeUnit
import scala.scalajs.js.JSConverters.*

case class TimeCorrectionsForm(
  visits:    List[VisitTimeCharge],
  onApplied: Callback
) extends ReactFnProps(TimeCorrectionsForm.component):
  // Only visits listed in the time charges table can be corrected, newest first.
  val rows: List[TimeChargeRow] =
    visits.flatMap(_.nightRow).sortBy(_.interval.map(_.start)).reverse

  val visitIds: List[Visit.Id] = rows.map(_.visitId)

object TimeCorrectionsForm:
  private given Display[TimeChargeCorrection.Op] = Display.byShortName(_.name)

  private val AmountUnits: NonEmptyList[TimeUnit] =
    NonEmptyList.of(TimeUnit.HOURS, TimeUnit.MINUTES, TimeUnit.SECONDS)

  private val component = ScalaFnComponent[TimeCorrectionsForm]: props =>
    for
      ctx      <- useContext(AppContext.ctx)
      selected <- useStateView(none[Visit.Id])
      op       <- useStateView(TimeChargeCorrection.Op.Subtract)
      amount   <- useStateView(TimeSpan.Zero)
      comment  <- useStateView(none[NonEmptyString])
      applying <- useStateView(false)
    yield
      import ctx.given

      val visit: Option[Visit.Id] =
        selected.get.filter(props.visitIds.contains).orElse(props.visitIds.headOption)

      // The ODB stops a visit's charge at zero but records the full amount, so a subtraction is
      // capped at what the visit is charged now.
      val maxAmount: Option[TimeSpan] =
        Option
          .when(op.get === TimeChargeCorrection.Op.Subtract)(visit)
          .flatten
          .flatMap(v => props.rows.find(_.visitId === v))
          .map(_.charged)

      val cappedAmount: View[TimeSpan] =
        amount.zoom(a => maxAmount.fold(a)(a.min))(f => a => f(maxAmount.fold(a)(a.min)))

      def apply(visitId: Visit.Id): Callback =
        (ctx.odbApi
          .addTimeChargeCorrection(visitId, op.get, cappedAmount.get, comment.get.map(_.value))
          .toastErrors
          .switching(applying.async) >>
          (amount.set(TimeSpan.Zero) >> comment.set(none) >> props.onApplied).toAsync).runAsync

      <.div(ExploreStyles.TimeChargesSection)(
        Divider(),
        <.div(ExploreStyles.ObservationDetailsSection)("Time Corrections"),
        <.div(ExploreStyles.TimeCorrectionsForm)(
          FormDropdownOptional(
            id = "time-correction-visit".refined,
            value = visit,
            options = props.visitIds.map(v => SelectItem(v, v.show)),
            label = "Visit",
            disabled = applying.get,
            onChange = selected.set
          ),
          FormLabel(htmlFor = "time-correction-amount".refined)("Amount"),
          <.div(ExploreStyles.TimeCorrectionsAmount)(
            SelectButtonEnumView(
              id = "time-correction-op".refined,
              view = op,
              disabled = applying.get
            ),
            FormTimeSpanInput(
              id = "time-correction-amount".refined,
              value = cappedAmount,
              units = AmountUnits,
              max = maxAmount.orUndefined,
              disabled = applying.get,
              tooltip = maxAmount
                .map(m => s"At most ${DurationSpacedFormatter(m.toDuration)} can be subtracted")
                .orUndefined
            )
          ),
          FormInputTextView(
            id = "time-correction-comment".refined,
            value = comment,
            codec = inputCodecs.NonEmptyTextOptional,
            label = "Comment",
            disabled = applying.get
          ),
          Button(
            label = "Apply",
            severity = Button.Severity.Secondary,
            loading = applying.get,
            disabled = applying.get || cappedAmount.get.isZero || visit.isEmpty,
            onClick = visit.fold(Callback.empty)(apply),
            clazz = ExploreStyles.TimeCorrectionsApply
          ).mini.compact
        )
      )
