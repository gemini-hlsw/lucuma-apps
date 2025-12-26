// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import lucuma.react.common.*
import crystal.react.View
import lucuma.core.math.Offset
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

final case class OffsetInput(offset: View[Offset], readonly: Boolean)
    extends ReactFnProps(OffsetInput)

object OffsetInput
    extends ReactFnComponent[OffsetInput]({ props =>
      <.div(props.offset.get.toString)
    })
