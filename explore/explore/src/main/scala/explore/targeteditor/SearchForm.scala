// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.validation.InputValidSplitEpi
import lucuma.refined.*
import lucuma.ui.forms.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.abbreviate
import org.scalajs.dom
import react.common.ReactFnProps
import react.semanticui.collections.form.Form.FormProps
import react.semanticui.collections.form.*
import react.semanticui.elements.label.LabelPointing
import react.semanticui.shorthand.*

import scalajs.js.timers
import scalajs.js.JSConverters.*

case class SearchForm(
  id:          Target.Id,
  targetView:  View[NonEmptyString],
  name:        NonEmptyString,
  searching:   View[Set[Target.Id]],
  searchAndGo: SearchCallback => Callback
) extends ReactFnProps[SearchForm](SearchForm.component) {
  def submit(
    searchTerm: String,
    before:     Callback,
    onComplete: Option[Target] => Callback,
    onError:    Throwable => Callback
  ): Callback =
    NonEmptyString
      .from(searchTerm)
      .toOption
      .map(s => before >> searchAndGo(SearchCallback(s, onComplete, onError)))
      .getOrEmpty
}

object SearchForm {
  type Props = SearchForm

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(props => props.name) // term
      .useState(true)                      // enabled
      .useState(none[NonEmptyString])      // error
      .useEffectWithDepsBy((props, _, _, _) => props.name)((_, term, enabled, _) =>
        name => term.set(name) >> enabled.setState(true)
      )
      .useEffectWithDepsBy((props, _, _, _) => props.id)((props, _, _, _) =>
        _ => // Auto-select name field on new targets.
          // Accessing dom elements by id is not ideal in React, but passing a ref to the <input> element of a
          // Form.Input is a bit convoluted. We should reevaluate when and if we switch to another component library.
          CallbackTo(
            Option(dom.document.getElementById("search"))
              .foldMap(node =>
                Callback(timers.setTimeout(0)(node.asInstanceOf[dom.HTMLInputElement].select()))
              )
          ).flatten
            .when(props.name === NewTargetName)
            .void
      )
      .render { (props, term, enabled, error) =>
        val searchComplete: Callback = props.searching.mod(_ - props.id)

        val search: Callback =
          props
            .submit(
              term.get.value,
              error.setState(none) >> props.searching.mod(_ + props.id),
              t =>
                searchComplete >>
                  error
                    .setState(
                      NonEmptyString
                        .unsafeFrom(s"'${abbreviate(term.get.value, 10)}' not found")
                        .some
                    )
                    .when_(t.isEmpty),
              _ => searchComplete >> error.setState("Search error...".refined[NonEmpty].some)
            )

        def iconKeyPress(e: ReactKeyboardEvent): Callback =
          search >> e.stopPropagationCB *> e.preventDefaultCB

        def submitForm: Form.OnSubmitE =
          (e: Form.ReactFormEvent, _: FormProps) =>
            e.preventDefaultCB >> search.when(enabled.value).void

        val searchIcon =
          (if (enabled.value)
             if (props.searching.get.nonEmpty)
               Icons.Spinner.withSpin(true)
             else
               Icons.Search.addModifiers(
                 Seq(^.onKeyPress ==> iconKeyPress, ^.onClick --> search)
               )
           else
             Icons.Ban).withClass(ExploreStyles.AladinSearchIcon)(^.tabIndex := -1)

        val disabled = props.searching.get.exists(_ === props.id)

        Form(clazz = ExploreStyles.SearchForm, onSubmitE = submitForm)(
          <.label("Name",
                  HelpIcon("target/main/search-target.md".refined),
                  ExploreStyles.SkipToNext
          ),
          FormInputEV(
            id = "search".refined,
            value = term.withOnMod(props.targetView.set),
            validFormat = InputValidSplitEpi.nonEmptyString,
            error = error.value.orUndefined,
            loading = disabled,
            disabled = disabled,
            errorClazz = ExploreStyles.InputErrorTooltipBelow,
            errorPointing = LabelPointing.Above,
            onTextChange = _ => error.setState(none),
            onValidChange = valid => enabled.setState(valid),
            icon = searchIcon
          ).withMods(^.placeholder := "Name"),
          // We need this hidden control to submit when pressing enter
          <.input(^.`type`         := "submit", ^.hidden := true)
        )

      }

}
