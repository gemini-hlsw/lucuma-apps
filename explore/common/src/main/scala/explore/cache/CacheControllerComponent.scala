// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Resource
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks.*
import crystal.react.syntax.effect.*
import fs2.Pipe
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewBoolean
import lucuma.ui.syntax.effect.*

object ResetType extends NewBoolean { val Wipe = True; val Keep = False }
type ResetType = ResetType.Type

trait CacheControllerComponent[S, P <: CacheControllerComponent.Props[S]]:
  private type F[T] = DefaultA[T]

  // Initial model and a stream of delayed updates.
  protected val initial: P => F[(S, fs2.Stream[F, S => S])]

  // Stream of updates to the cache. Updates are collected as soon as the
  // app starts, but they processed once all initial delayed updates complete.
  protected val updateStream: P => Resource[F, fs2.Stream[F, S => S]]

  private def impactModel(
    modState: (Pot[S] => Pot[S]) => F[Unit]
  ): Pipe[F, Either[Throwable, S => S], Unit] =
    _.evalMap: modElem =>
      modState: oldValue =>
        modElem.fold(
          t => Pot.Error(new RuntimeException(s"Update Error: ${t.getMessage}", t)),
          f => oldValue.map(f)
        )

  val component = ScalaFnComponent[P]: props =>
    for
      applyStreamUpdates <-
        useCallback: // Apply updates from an update stream.
          (stream: fs2.Stream[F, Either[Throwable, S => S]]) =>
            stream.through(impactModel(props.modState)).compile.drain
      version            <- useState(0)
      _                  <-
        useResource(version.value): _ =>
          for
            // Start the update fiber. We want subscriptions to start before initial query.
            // This way we don't miss updates.
            // The update fiber will only apply updates once the cache is initialized (via latch).
            latch           <- Resource.eval(Deferred[F, Unit])
            // Next is the update fiber. It subscribes immediately, but holds the updates (clue
            // queues them) until the snapshot and the delayed inits are applied, which would
            // otherwise overwrite them. Held updates are applied in order; they are idempotent.
            // Will run until the component is unmounted.
            updateStreamPot <- updateStream(props).map(_.attempt)
            _               <- applyStreamUpdates(fs2.Stream.exec(latch.get) ++ updateStreamPot).background
            // initResult is (initValue, delayedInitsStream)
            initResult      <- Resource.eval:
                                 initial(props).attemptPot.map:
                                   _.adaptError: t =>
                                     new RuntimeException(s"Initialization Error: ${t.getMessage}", t)
            // Apply initial value.
            _               <- Resource.eval(props.modState(_ => initResult.map(_._1)) >> props.onLoad)
            // Apply delayed inits, then release the held updates.
            _               <- (initResult.toOption
                                 .map(_._2.attempt)
                                 .map(applyStreamUpdates)
                                 .orEmpty >> latch.complete(()).void).background
          yield ()
      _                  <- useStreamOnMount:
                              props.resetSignal.evalMap: resetType =>
                                props.modState(_ => Pot.pending).whenA(resetType === ResetType.Wipe) >>
                                  version.modState(_ + 1).toAsync
    yield EmptyVdom

object CacheControllerComponent:
  trait Props[S]:
    def modState: (Pot[S] => Pot[S]) => DefaultA[Unit]
    def onLoad: DefaultA[Unit]
    def resetSignal: fs2.Stream[DefaultA, ResetType] = fs2.Stream.empty
