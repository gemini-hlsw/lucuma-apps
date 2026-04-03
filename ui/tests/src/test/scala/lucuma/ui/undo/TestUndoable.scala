// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.Applicative
import cats.effect.std.Dispatcher
import crystal.react.View
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import lucuma.ui.optics.GetAdjust
import lucuma.ui.undo.UndoContext

class TestUndoable[M](
  val valueRef:   VarRef[DefaultS, M],
  stacksRef:      VarRef[DefaultS, UndoStacks[DefaultA, M]]
)(implicit
  val DefaultA:   Applicative[DefaultA],
  val dispatcher: Dispatcher[DefaultA]
) {
  def get: DefaultS[M] = valueRef.get

  def context: DefaultS[UndoContext[M]] =
    for
      valueView  <- varRefView(valueRef)
      stacksView <- varRefView(stacksRef)
    yield UndoContext(stacksView, valueView)

  def set[A](getAdjust: GetAdjust[M, A], v: A): DefaultS[Unit] =
    context >>= (_.set(getAdjust.get, getAdjust.set, (_: A) => DefaultA.unit)(v))

  def mod[A](getAdjust: GetAdjust[M, A], f: A => A): DefaultS[Unit] =
    context >>= (_.mod(getAdjust.get, getAdjust.set, (_: A) => DefaultA.unit)(f))

  def undo: DefaultS[Unit] = context >>= (_.undo)

  def redo: DefaultS[Unit] = context >>= (_.redo)

  private def varRefModCB[A](
    ref: VarRef[DefaultS, A]
  ): (A => A, (A, A) => DefaultS[Unit]) => DefaultS[Unit] =
    (f, cb) => ref.update(f) >>= ((p, c) => cb(p, c))

  private def varRefView[A](ref: VarRef[DefaultS, A]): DefaultS[View[A]] =
    ref.get.map(a => View(a, varRefModCB(ref)))
}

object TestUndoable {
  def apply[M](
    initValue:  M
  )(implicit
    DefaultA:   Applicative[DefaultA],
    dispatcher: Dispatcher[DefaultA]
  ): DefaultS[TestUndoable[M]] =
    DefaultS.pure(
      new TestUndoable(
        VarRef[DefaultS].of(initValue),
        VarRef[DefaultS].of(UndoStacks.empty[DefaultA, M])
      )
    )
}
