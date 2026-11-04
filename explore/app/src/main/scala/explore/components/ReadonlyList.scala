// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps

/**
 * A bordered, striped list of configuration values that can't be edited, such as the filters or
 * wavelengths a generated daytime observation copies from its science observation.
 */
final case class ReadonlyList(items: List[String]) extends ReactFnProps(ReadonlyList)

object ReadonlyList
    extends ReactFnComponent[ReadonlyList](props =>
      <.div(ExploreStyles.ReadonlyList)(
        props.items.toTagMod(using item => <.span(ExploreStyles.ReadonlyListRow, item))
      )
    )
