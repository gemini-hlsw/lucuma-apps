// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.formats.*
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

// Component on the toolbar showing the coordinates of the mouse
case class AladinMousePosition(
  coords: SignallingRef[IO, Option[Coordinates]]
) extends ReactFnProps(AladinMousePosition)

object AladinMousePosition
    extends ReactFnComponent[AladinMousePosition](props =>
      useStreamOnMount(props.coords.discrete).map: current =>
        <.label(
          Icons.MousePointer.withClass(ExploreStyles.Accented),
          ExploreStyles.AladinCurrentCoords,
          <.span(
            ExploreStyles.AladinDetailText,
            current.toOption.flatten.map(formatCoordinates)
          )
        )
    )
