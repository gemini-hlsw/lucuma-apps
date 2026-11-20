// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.queue

import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.services.DaytimeQueueApi

case class DaytimeQueueLamps(lampsOffIn: Option[TimeSpan], lampsOffError: Option[String])
    extends ReactFnProps(DaytimeQueueLamps)

object DaytimeQueueLamps
    extends ReactFnComponent[DaytimeQueueLamps](props =>
      for
        ctx      <- useContext(AppContext.ctx)
        queueApi <- useContext(DaytimeQueueApi.ctx)
      yield
        import ctx.given

        React.Fragment(
          props.lampsOffIn.map: lampsOffIn =>
            val seconds: Long =
              lampsOffIn.toSeconds.setScale(0, BigDecimal.RoundingMode.CEILING).toLong
            <.div(ObserveStyles.DaytimeQueueLamps)(
              Icons.Clock,
              s"GCAL lamps turn off in $seconds s",
              Button(
                label = "Cancel",
                icon = Icons.Ban,
                severity = Button.Severity.Secondary,
                onClick = queueApi.cancelLampsOff.runAsync
              ).small.compact
            )
          ,
          props.lampsOffError.map: error =>
            <.div(ObserveStyles.DaytimeQueueLamps |+| ObserveStyles.DaytimeQueueLampsError)(
              Icons.TriangleExclamation,
              s"Could not turn off the GCAL lamps: $error"
            )
        )
    )
