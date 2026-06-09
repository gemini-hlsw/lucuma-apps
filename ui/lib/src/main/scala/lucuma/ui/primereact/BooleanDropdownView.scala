// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem

import scalajs.js

case class BooleanDropdownView[V[_]](
  id:         NonEmptyString,
  value:      V[Boolean],
  trueLabel:  NonEmptyString,
  falseLabel: NonEmptyString,
  clazz:      js.UndefOr[Css] = js.undefined,
  panelClass: js.UndefOr[Css] = js.undefined,
  disabled:   js.UndefOr[Boolean] = js.undefined,
  size:       js.UndefOr[PlSize] = js.undefined,
  onChangeE:  js.UndefOr[(Option[Boolean], ReactEvent) => Callback] =
    js.undefined, // called after the view is set
  modifiers:  Seq[TagMod] = Seq.empty
)(using
  val vl:     ViewLike[V]
) extends ReactFnProps(BooleanDropdownView.component):
  def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
  def withMods(mods:          TagMod*)     = addModifiers(mods)
  def apply(mods:             TagMod*)     = addModifiers(mods)

object BooleanDropdownView:
  private type AnyF[_] = Any

  private def buildComponent[V[_]] = ScalaFnComponent[BooleanDropdownView[V]] { props =>
    import props.given

    val sizeCls = props.size.toOption.map(_.cls).orEmpty

    DropdownOptional(
      id = props.id.value,
      value = props.value.get,
      options = List(
        SelectItem(false, props.falseLabel.value),
        SelectItem(true, props.trueLabel.value)
      ),
      clazz = sizeCls |+| props.clazz.toOption.orEmpty,
      panelClass = sizeCls |+| props.panelClass.toOption.orEmpty,
      disabled = props.disabled.getOrElse(false),
      onChange = v => props.value.set(v.get),
      onChangeE = props.onChangeE.getOrElse((_, _) => Callback.empty),
      modifiers = props.modifiers
    )
  }

  private val component = buildComponent[AnyF]
