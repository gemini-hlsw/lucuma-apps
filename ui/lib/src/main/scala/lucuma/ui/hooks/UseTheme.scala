// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.hooks

import crystal.react.View
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.CustomHook
import lucuma.ui.enums.Theme

// The stored theme is mounted in a layout effect so the first paint already uses it.
def useTheme(initial: => Theme = Theme.System): HookResult[View[Theme]] =
  for
    theme <- useStateView(initial)
    _     <- useLayoutEffectOnMount:
               Theme.stored.map(_.getOrElse(initial)).flatMap(Theme.init) >>= theme.set
  yield theme.withOnMod(t => t.mount >> Theme.store(t))

private object UseTheme:

  private val hook: CustomHook[Theme, View[Theme]] =
    CustomHook.fromHookResult(useTheme(_))

  object HooksApiExt:
    sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]):
      /**
       * Applies theming to the whole page and provides a `View` to change the theme.
       *
       * @param initial
       *   Initial theme. Defaults to `Theme.System`.
       */
      final def useTheme(initial: Theme = Theme.System)(using step: Step): step.Next[View[Theme]] =
        useThemeBy(_ => initial)

      /**
       * Applies theming to the whole page and provides a `View` to change the theme.
       *
       * @param initial
       *   Initial theme. Defaults to `Theme.System`.
       */
      final def useThemeBy(initial: Ctx => Theme)(using step: Step): step.Next[View[Theme]] =
        api.customBy(ctx => hook(initial(ctx)))

    final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ) extends Primary[Ctx, Step](api):
      /**
       * Applies theming to the whole page and provides a `View` to change the theme.
       *
       * @param initial
       *   Initial theme. Defaults to `Theme.System`.
       */
      def useThemeBy(initial: CtxFn[Theme])(using step: Step): step.Next[View[Theme]] =
        super.useThemeBy(step.squash(initial)(_))

  trait HooksApiExt:
    import HooksApiExt.*

    implicit def hooksExtTheme1[Ctx, Step <: HooksApi.AbstractStep](
      api: HooksApi.Primary[Ctx, Step]
    ): Primary[Ctx, Step] =
      new Primary(api)

    implicit def hooksExtTheme2[
      Ctx,
      CtxFn[_],
      Step <: HooksApi.SubsequentStep[Ctx, CtxFn]
    ](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ): Secondary[Ctx, CtxFn, Step] =
      new Secondary(api)

  object syntax extends HooksApiExt
