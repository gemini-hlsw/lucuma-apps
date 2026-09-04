// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.CustomHook
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import lucuma.core.util.NewType
import lucuma.react.table.*
import lucuma.ui.reusability.given

case class TableOptionsWithStateStore[F[_], T, TM, CM, TF](
  tableOptions:            TableOptions[T, TM, CM, TF],
  stateStore:              TableStateStore[F, TF],
  appControlledColumns:    Set[ColumnId] = Set.empty,
  defaultColumnVisibility: Option[ColumnVisibility] = none
)

private object UseReactTableWithStateStore:
  private object Loaded extends NewType[Boolean]

  def useReactTableWithStateStore[T, TM, CM, TF](
    options: TableOptionsWithStateStore[DefaultA, T, TM, CM, TF]
  ): HookResult[Table[T, TM, CM, TF]] =
    def preferencesOf(state: TableState[TF]): TablePreferences =
      TablePreferences.fromState(state).withoutColumns(options.appControlledColumns)

    for
      table         <- useReactTable(options.tableOptions)
      defaults      <- useMemo(()): _ =>
                         options.defaultColumnVisibility.getOrElse:
                           options.tableOptions.state
                             .flatMap(_.columnVisibility)
                             .getOrElse(table.initialState.columnVisibility)
      loaded        <- useRef(Loaded(false))
      lastPersisted <- useRef(none[TablePreferences])
      _             <- useEffectOnMount:
                         options.stateStore
                           .load()
                           .flatMap:
                             _.map(preferencesOf)
                               .map(_.withDefaultVisibility(defaults.value))
                               .traverse: prefs =>
                                 lastPersisted.setAsync(prefs.some) >>
                                   prefs.applyTo(table, options.appControlledColumns).to[IO]
                           .void
                           // On failure `lastPersisted` stays empty, so the next change saves.
                           .guarantee(loaded.setAsync(Loaded(true)))
      _             <- useEffectWithDeps(table.getState()): state =>
                         val current = preferencesOf(state)
                         (options.stateStore.save(current.toTableState) >>
                           lastPersisted.setAsync(current.some))
                           .whenA(loaded.value.value && !lastPersisted.value.contains_(current))
    yield table

  private def hook[T, TM, CM, TF]
    : CustomHook[TableOptionsWithStateStore[DefaultA, T, TM, CM, TF], Table[T, TM, CM, TF]] =
    CustomHook.fromHookResult(useReactTableWithStateStore(_))

  object HooksApiExt:
    sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]):
      final def useReactTableWithStateStore[T, TM, CM, TF](
        options: TableOptionsWithStateStore[DefaultA, T, TM, CM, TF]
      )(using
        step:    Step
      ): step.Next[Table[T, TM, CM, TF]] =
        useReactTableWithStateStoreBy(_ => options)

      final def useReactTableWithStateStoreBy[T, TM, CM, TF](
        options: Ctx => TableOptionsWithStateStore[DefaultA, T, TM, CM, TF]
      )(using
        step:    Step
      ): step.Next[Table[T, TM, CM, TF]] =
        api.customBy(ctx => hook(options(ctx)))

    final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ) extends Primary[Ctx, Step](api):
      def useReactTableWithStateStoreBy[T, TM, CM, TF](
        tableDefWithOptions: CtxFn[TableOptionsWithStateStore[DefaultA, T, TM, CM, TF]]
      )(using
        step:                Step
      ): step.Next[Table[T, TM, CM, TF]] =
        super.useReactTableWithStateStoreBy(step.squash(tableDefWithOptions)(_))

  trait HooksApiExt:
    import HooksApiExt.*

    implicit def hooksExtReactTableWithStateStore1[Ctx, Step <: HooksApi.AbstractStep](
      api: HooksApi.Primary[Ctx, Step]
    ): Primary[Ctx, Step] =
      new Primary(api)

    implicit def hooksExtReactTableWithStateStore2[
      Ctx,
      CtxFn[_],
      Step <: HooksApi.SubsequentStep[Ctx, CtxFn]
    ](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ): Secondary[Ctx, CtxFn, Step] =
      new Secondary(api)

  object syntax extends HooksApiExt
