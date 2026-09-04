// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import explore.model.MaskDesignSlit
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps

case class MaskSlitTooltip(slit: MaskDesignSlit) extends ReactFnProps(MaskSlitTooltip)

object MaskSlitTooltip
    extends ReactFnComponent[MaskSlitTooltip](p =>
      val role   = if (p.slit.isAcquisition) "Acquisition" else "Science"
      val raStr  = MathValidators.truncatedRA.reverseGet(p.slit.coordinates.ra)
      val decStr = MathValidators.truncatedDec.reverseGet(p.slit.coordinates.dec)

      <.div(
        <.div(s"Slit ${p.slit.id} · $role"),
        <.div(s"$raStr $decStr")
      )
    )
