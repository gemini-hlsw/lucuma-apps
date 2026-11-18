// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.model.Help
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.ui.primereact.EnumDropdownOptionalView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given

/**
 * An enum select for a value that defaults to `defaultValue` but can be overridden. `overrideView`
 * holds the override. While there is none, the dropdown shows `defaultValue`, and choosing
 * `defaultValue` removes the override. While there is an override, an addon icon is shown that
 * removes it, with a tooltip naming the default it reverts to, or `noDefaultText` if there is none.
 */
final case class OverridableEnumSelect[A](
  id:            NonEmptyString,
  overrideView:  View[Option[A]],
  defaultValue:  Option[A],
  noDefaultText: String,
  label:         Option[String] = None,
  helpId:        Option[Help.Id] = None,
  disabled:      Boolean = false,
  clazz:         Css = Css.Empty
)(using val display: Display[A], val enumerated: Enumerated[A])
    extends ReactFnProps(OverridableEnumSelect.component)

object OverridableEnumSelect:
  private def buildComponent[A] = ScalaFnComponent[OverridableEnumSelect[A]]: props =>
    import props.given

    val displayView: View[Option[A]] =
      props.defaultValue.fold(props.overrideView)(props.overrideView.withDefault)

    val defaultText: String = props.defaultValue.fold(props.noDefaultText)(_.shortName)

    val resetAddon: TagMod =
      <.span(
        PrimeStyles.InputGroupAddon,
        <.span(
          Icons.SquareXMark,
          ^.onClick --> props.overrideView.set(none).unless_(props.disabled)
        ).withTooltip(
          content = s"Reset to default: $defaultText",
          position = Tooltip.Position.Left
        )
      ).when(props.overrideView.get.isDefined)

    React.Fragment(
      props.label.map(label => FormLabel(htmlFor = props.id)(label, props.helpId.map(HelpIcon(_)))),
      <.span(
        LucumaPrimeStyles.FormField,
        PrimeStyles.InputGroup,
        props.clazz,
        EnumDropdownOptionalView(
          id = props.id,
          value = displayView,
          disabled = props.disabled,
          showClear = false
        ),
        resetAddon
      )
    )

  private val component = buildComponent[Any]
