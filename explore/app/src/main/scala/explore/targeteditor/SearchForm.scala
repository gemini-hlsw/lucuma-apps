// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.HelpIcon
import explore.model.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.scalajs.dom

import scala.concurrent.duration.*

import scalajs.js.JSConverters.*

case class SearchForm(
  id:            Target.Id,
  targetName:    View[NonEmptyString],
  targetSet:     Target => Callback,
  searching:     View[Set[Target.Id]],
  readonly:      Boolean,
  cloningTarget: Boolean,
  disableSearch: Boolean = false
) extends ReactFnProps[SearchForm](SearchForm)

object SearchForm
    extends ReactFnComponent[SearchForm](props =>
      for
        ctx              <- useContext(AppContext.ctx)
        term             <- useStateView(props.targetName.get)
        enabled          <- useState(true)
        error            <- useState(none[NonEmptyString])
        _                <- useEffectWithDeps(props.targetName.get): name =>
                              term.set(name) >> enabled.setState(true)
        buttonRef        <- useRefToVdom[dom.HTMLButtonElement]
        inputRef         <- useRefToVdom[dom.HTMLInputElement]
        _                <- useEffectWithDeps((props.id, inputRef)): (_, inputRef) =>
                              // Auto select name field on new targets
                              inputRef.get
                                .flatMap(
                                  _.foldMap(i => Callback(i.select()).delayMs(50).toCallback)
                                )
                                .when_(props.targetName.get === NewTargetName)
        singleEffect     <- useSingleEffect
        searchPopupState <- useStateView(PopupState.Closed)
      yield
        import ctx.given

        val searchComplete: Callback = props.searching.mod(_ - props.id)

        def onKeyPress = (e: ReactKeyboardEvent) =>
          if (Option(e.key).exists(_ === dom.KeyValue.Enter) && !props.readonly)
            // we just click the button, which deals with the name update/cloning/delay
            // as details in comments below.
            buttonRef.get >>= (_.map(button => Callback(button.click())).orEmpty)
          else
            Callback.empty

        def onButtonClick: Callback =
          // This will cancel the name update, as described in a comment below.
          singleEffect
            .submit(
              (error.setState(none) >> props.searching.mod(_ + props.id)).toAsync
            )
            .runAsync

        // if the user cancels the search, the term (in the text input) might not
        // match the target name, so we need to update the target name to match.
        // This is the behavior requested by Andy.
        def updateNameIfNeeded: Callback =
          if (term.get =!= props.targetName.get)
            props.targetName.set(term.get)
          else Callback.empty

        val searchIcon: VdomNode =
          if (enabled.value && !props.readonly && !props.disableSearch)
            Button(
              severity = Button.Severity.Success,
              disabled = props.cloningTarget || props.searching.get.nonEmpty,
              icon = Icons.Search,
              loading = props.searching.get.nonEmpty,
              onClick = onButtonClick >> searchPopupState.set(PopupState.Open),
              modifiers = List(^.untypedRef := buttonRef)
            ).tiny.compact
          else Icons.Ban

        val disabled =
          props.searching.get.exists(_ === props.id) || props.readonly || props.cloningTarget

        React.Fragment(
          FormInputTextView(
            id = "search".refined,
            value = term.withOnMod(nes =>
              // We submit to the singleEffect with a delay. If the user hit Enter or they
              // click on the button before leaving the input field, this will get cancelled
              // so that the name doesn't get updated and, more importantly, no target cloning
              // occurs, which messed everything up.
              singleEffect
                .submit(IO.sleep(200.milliseconds) >> props.targetName.set(nes).toAsync)
                .runAsync
            ),
            label = React.Fragment("Name", HelpIcon("target/main/search-target.md".refined)),
            validFormat = InputValidSplitEpi.nonEmptyString,
            error = error.value.orUndefined,
            disabled = disabled,
            postAddons = List(searchIcon).filterNot(_ => props.readonly || props.disableSearch),
            onTextChange = (_: String) => error.setState(none),
            onValidChange = valid => enabled.setState(valid),
            placeholder = "Name"
          ).withMods(^.onKeyPress ==> onKeyPress, ^.untypedRef := inputRef),
          TargetSelectionPopup(
            "Replace Target Data",
            searchPopupState,
            NonEmptyList.one(TargetSource.FromSimbad[IO](ctx.simbadClient)),
            "",
            Icons.Ban,
            "Fetch",
            Icons.ArrowDownLeft,
            onSelected = targetWithId => props.targetSet(targetWithId.target) >> searchComplete,
            onCancel = updateNameIfNeeded >> searchComplete,
            initialSearch = term.get.some
          )
        )
    )
