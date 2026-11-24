// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.schemas.model.ImagingVariant
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

final case class MosPreImagingCheckbox(variant: View[ImagingVariant], readonly: Boolean)
    extends ReactFnProps(MosPreImagingCheckbox)

object MosPreImagingCheckbox
    extends ReactFnComponent[MosPreImagingCheckbox](props =>
      val preImaging: View[Boolean] =
        props.variant.zoom(_.isPreImaging)(f => v => v.withPreImaging(f(v.isPreImaging)))

      <.div(ExploreStyles.ImagingPreImagingRow)(
        CheckboxView(
          id = "mos-pre-imaging".refined,
          value = preImaging,
          label = React.Fragment(
            "MOS Pre-imaging",
            HelpIcon("configuration/imaging/mos-pre-imaging.md".refined)
          ),
          disabled = props.readonly
        )
      )
    )
