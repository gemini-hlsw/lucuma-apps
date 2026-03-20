// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.datepicker.hooks.UseDatepickerRef.useDatepickerRef
import lucuma.react.primereact.Button
import explore.components.ui.ExploreStyles

import java.time.LocalDate

case class DatePickerLocalDate(
  dateView:        View[LocalDate],
  withTodayButton: Option[Callback] = None,
  dateFormat:      String = "yyyy-MM-dd",
  className:       Css = Css.Empty
) extends ReactFnProps(DatePickerLocalDate)

object DatePickerLocalDate
    extends ReactFnComponent[DatePickerLocalDate](props =>
      useDatepickerRef.map: datepickerRef =>
        Datepicker(
          onChange = _.map(_.fromJsDate).foldMap(props.dateView.set),
          selected = props.dateView.get.toJsDate.some,
          dateFormat = props.dateFormat,
          className = props.className,
          calendarClassName = ExploreStyles.DatePicker
        )(
          props.withTodayButton.map: cb =>
            Button(
              onClick = cb >> datepickerRef.setOpen(false),
            )("Today")
        ).withRef(datepickerRef.ref)
    )
