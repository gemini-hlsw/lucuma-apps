// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import cats.syntax.monoid.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.*
import lucuma.react.primereact.SelectButtonMultiple
import lucuma.react.primereact.SelectItem

import scalajs.js

case class SelectButtonMultipleEnumView[V[_], A](
  id:             NonEmptyString,
  view:           V[List[A]],
  label:          js.UndefOr[TagMod] = js.undefined,
  groupClass:     js.UndefOr[Css] = js.undefined,
  buttonClass:    js.UndefOr[Css] = js.undefined,
  size:           js.UndefOr[PlSize] = js.undefined,
  itemTemplate:   js.UndefOr[SelectItem[A] => VdomNode] = js.undefined,
  disabled:       js.UndefOr[Boolean] = js.undefined,
  onChange:       List[A] => Callback = (_: List[A]) => Callback.empty,
  filterPred:     A => Boolean = (_: A) => true,
  itemClass:      A => js.UndefOr[Css] = (_: A) => js.undefined,
  modifiers:      Seq[TagMod] = Seq.empty
)(using
  val enumerated: Enumerated[A],
  val display:    Display[A],
  val vl:         ViewLike[V]
) extends ReactFnProps(SelectButtonMultipleEnumView.component):
  def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
  def withMods(mods:          TagMod*)     = addModifiers(mods)
  def apply(mods:             TagMod*)     = addModifiers(mods)

object SelectButtonMultipleEnumView:
  private type AnyF[_] = Any

  private def buildComponent[V[_], A] = ScalaFnComponent[SelectButtonMultipleEnumView[V, A]]:
    props =>
      import props.given

      React.Fragment(
        props.label.map(l => FormLabel(htmlFor = props.id, size = props.size)(l)),
        props.view.get.map: value =>
          SelectButtonMultiple[A](
            value = value,
            Enumerated[A].all
              .filter(props.filterPred)
              .map: s =>
                val itemCss = (props.itemClass(s).toOption, props.buttonClass.toOption) match
                  case (Some(i), Some(b)) => i |+| b
                  case (Some(i), None)    => i
                  case (None, Some(b))    => b
                  case _                  => Css.Empty
                SelectItem(s, label = props.display.shortName(s), clazz = itemCss)
            ,
            id = props.id.value,
            clazz = props.groupClass,
            itemTemplate = props.itemTemplate,
            onChange = (a: List[A]) => props.view.set(a) *> props.onChange(a),
            disabled = props.disabled
          )
      )

  private val component = buildComponent[AnyF, Any]
