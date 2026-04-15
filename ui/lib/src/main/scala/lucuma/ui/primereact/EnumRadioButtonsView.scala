// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps

import scalajs.js

final case class EnumRadioButtonsView[V[_], A](
  idBase:   NonEmptyString, // base id for the 2 radio buttons. `-{tag}` is appended to this.
  value:    V[A],
  name:     NonEmptyString, // name of "radio button group"
  clazz:    Css = Css.Empty,
  disabled: js.UndefOr[Boolean] = js.undefined,
  required: js.UndefOr[Boolean] = js.undefined
)(using val ev: ViewLike[V], val enumerated: Enumerated[A], val display: Display[A])
    extends ReactFnProps(EnumRadioButtonsView.component)

object EnumRadioButtonsView:
  type AnyF[_] = Any

  private def buildComponent[V[_], A] = ScalaFnComponent[EnumRadioButtonsView[V, A]]: props =>
    import props.given

    React.Fragment(
      Enumerated[A].all
        .map(a =>
          RadioButtonView[V, A](
            id = NonEmptyString.unsafeFrom(s"${props.idBase.value}-${Enumerated[A].tag(a)}"),
            value = a,
            view = props.value,
            label = NonEmptyString.unsafeFrom(a.shortName),
            name = props.name,
            clazz = props.clazz,
            disabled = props.disabled,
            required = props.required
          )
        )*
    )

  private val component = buildComponent[AnyF, Any]
