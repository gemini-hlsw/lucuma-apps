// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.Applicative
import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.testkit.TestControl
import crystal.react.View
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import lucuma.ui.optics.GetAdjust
import lucuma.ui.undo.UndoContext

import scala.concurrent.duration.*

class TestUndoable[M](
  val valueRef:   VarRef[DefaultS, M],
  stacksRef:      VarRef[DefaultS, UndoStacks[DefaultA, M]],
  effectRunner:   UndoEffectRunner[DefaultA]
)(implicit
  val DefaultA:   Applicative[DefaultA],
  val dispatcher: Dispatcher[DefaultA]
) {
  def get: DefaultS[M] = valueRef.get

  def context: DefaultS[UndoContext[M]] =
    for
      valueView  <- varRefView(valueRef)
      stacksView <- varRefView(stacksRef)
    yield UndoContext(
      stacksView,
      valueView,
      effectRunner,
      // Runs the effects on the runtime of the test, so that TestControl drives them.
      fa => DefaultS.delay(dispatcher.unsafeRunAndForget(fa))
    )

  def set[A](getAdjust: GetAdjust[M, A], v: A): DefaultS[Unit] =
    context >>= (_.set(getAdjust.get, getAdjust.set, (_: A) => DefaultA.unit)(v))

  def mod[A](getAdjust: GetAdjust[M, A], f: A => A): DefaultS[Unit] =
    context >>= (_.mod(getAdjust.get, getAdjust.set, (_: A) => DefaultA.unit)(f))

  def setWith[A](
    getAdjust: GetAdjust[M, A],
    v:         A,
    onSet:     DefaultA[Unit],
    onRestore: DefaultA[Unit]
  ): DefaultS[Unit] =
    context >>= (_.set(
      getAdjust.get,
      getAdjust.set,
      (_: A) => onSet,
      (_: A) => onRestore
    )(v))

  def stacks: DefaultS[UndoStacks[DefaultA, M]] = stacksRef.get

  def withContext(f: UndoContext[M] => DefaultS[Unit]): DefaultS[Unit] = context >>= f

  /** Runs `action` without waiting for the effects it starts. */
  def fire(action: TestUndoable[M] => DefaultS[Unit]): IO[Unit] =
    IO(action(this).runNow()) >> IO.sleep(1.milli)

  /** Runs `action` and lets the effects it starts finish. */
  def exec(action: TestUndoable[M] => DefaultS[Unit]): IO[Unit] =
    fire(action) >> IO.sleep(1.minute)

  /** Changes the model behind the undo context, as a server update would. */
  def external(f: M => M): IO[Unit] = IO(valueRef.update(f)).void

  def value: IO[M] = IO(get.runNow())

  def currentStacks: IO[UndoStacks[DefaultA, M]] = IO(stacks.runNow())

  def undo: DefaultS[Unit] = context >>= (_.undo)

  def redo: DefaultS[Unit] = context >>= (_.redo)

  private def varRefModCB[A](
    ref: VarRef[DefaultS, A]
  ): (A => A, (A, A) => DefaultS[Unit]) => DefaultS[Unit] =
    // `VarRef.update` mutates when called, so it is deferred until the effect runs.
    (f, cb) => DefaultS.delay(()).flatMap(_ => ref.update(f)).flatMap((p, c) => cb(p, c))

  private def varRefView[A](ref: VarRef[DefaultS, A]): DefaultS[View[A]] =
    ref.get.map(a => View(a, varRefModCB(ref)))
}

object TestUndoable {
  def apply[M](
    initValue:    M,
    effectRunner: UndoEffectRunner[DefaultA] = UndoEffectRunner.local[DefaultA]
  )(implicit
    DefaultA:     Applicative[DefaultA],
    dispatcher:   Dispatcher[DefaultA]
  ): DefaultS[TestUndoable[M]] =
    DefaultS.pure(
      new TestUndoable(
        VarRef[DefaultS].of(initValue),
        VarRef[DefaultS].of(UndoStacks.empty[DefaultA, M]),
        effectRunner
      )
    )

  /** Runs `body` on a fresh undoable under simulated time. */
  def simulated[M, A](
    initValue: M,
    runner:    IO[UndoEffectRunner[IO]] = IO.pure(UndoEffectRunner.local[IO])
  )(body: TestUndoable[M] => IO[A]): IO[A] =
    TestControl.executeEmbed(
      Dispatcher
        .parallel[IO]
        .use: dispatcher =>
          given Dispatcher[IO] = dispatcher
          runner.flatMap(r => IO(TestUndoable[M](initValue, r).runNow())).flatMap(body)
    )
}
