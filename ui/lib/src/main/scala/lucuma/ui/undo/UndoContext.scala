// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.Applicative
import cats.Endo
import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS

/*
 * Combines a view of a model `M` and a view of `UndoStacks` over `M`.
 *
 * `launch` starts an effect in the background. It exists only for tests, which replace it so that
 * effects run on their simulated clock; application code should keep the default.
 */
case class UndoContext[M](
  stacks:       View[UndoStacks[DefaultA, M]],
  model:        View[M],
  effectRunner: UndoEffectRunner[DefaultA] = UndoEffectRunner.local[DefaultA],
  launch:       DefaultA[Unit] => DefaultS[Unit] = _.runAsyncAndForget
) extends UndoSetter[M]
    with Undoer {

  private lazy val undoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.undo)
  private lazy val redoStack: View[UndoStack[DefaultA, M]] = stacks.zoom(UndoStacks.redo)
  private lazy val workingView: View[Boolean]              = stacks.zoom(UndoStacks.working)

  lazy val isUndoEmpty: Boolean = stacks.get.undo.isEmpty
  lazy val isRedoEmpty: Boolean = stacks.get.redo.isEmpty

  // Unset "working" on callback passed to react to be executed after setState completion...

  lazy val working: Boolean = stacks.get.working

  private def push(stack: View[UndoStack[DefaultA, M]]): Restorer[DefaultA, M] => DefaultS[Unit] =
    restorer => stack.mod(s => restorer +: s)

  // A popped entry, its inverse, and how to put the stacks back if the entry is rejected.
  private type Popped =
    (Restorer[DefaultA, M], Restorer[DefaultA, M], Endo[UndoStacks[DefaultA, M]])

  // Moves the top of the undo stack to the redo stack as its inverse.
  private def popUndoStack: DefaultS[Option[Popped]] =
    DefaultS
      .delay(stacks.get.undo)
      .flatMap:
        case head :: tail =>
          val inverse: Restorer[DefaultA, M]       = head.onModel(model.get)
          // Puts `head` back under the entries that edits pushed since.
          val unpop: Endo[UndoStacks[DefaultA, M]] = s =>
            val (newer: UndoStack[DefaultA, M], older: UndoStack[DefaultA, M]) =
              s.undo.splitAt(s.undo.length - tail.length)
            s.copy(undo = newer ++ (head +: older), redo = s.redo.filterNot(_ === inverse))
          stacks
            .set(UndoStacks(tail, inverse +: stacks.get.redo, true))
            .as((head, inverse, unpop).some)
        case Nil          => Applicative[DefaultS].pure(none)

  // Moves the top of the redo stack to the undo stack as its inverse.
  private def popRedoStack: DefaultS[Option[Popped]] =
    DefaultS
      .delay(stacks.get.redo)
      .flatMap:
        case head :: tail =>
          val inverse: Restorer[DefaultA, M]       = head.onModel(model.get)
          // Drops the inverse; `head` returns to the redo stack unless an edit cleared it.
          val unpop: Endo[UndoStacks[DefaultA, M]] = s =>
            if s.undo.headOption.exists(_ === inverse) then
              s.copy(undo = s.undo.drop(1), redo = head +: s.redo)
            else s.copy(undo = s.undo.filterNot(_ === inverse))
          stacks
            .set(UndoStacks(inverse +: stacks.get.undo, tail, true))
            .as((head, inverse, unpop).some)
        case Nil          => Applicative[DefaultS].pure(none)

  private def reset(stack: View[UndoStack[DefaultA, M]]): DefaultS[Unit] =
    stack.set(List.empty)

  private def setModel(restorer: Restorer[DefaultA, M]): DefaultS[Unit] =
    model.mod(restorer.setter(restorer.value))

  // `rollback` runs if `fa` is rejected; `onFailure` runs on any failure.
  private def submitEffect(
    fa:        DefaultA[Unit],
    rollback:  DefaultS[Unit],
    onFailure: DefaultS[Unit]
  ): DefaultS[Unit] =
    launch(effectRunner.submit(fa, rollback.to[DefaultA], onFailure.to[DefaultA]))

  // Applies the popped entry; a rejection puts the model and the stacks back.
  private def restore(popped: Option[Popped]): DefaultS[Unit] =
    popped
      .map: (restorer, inverse, unpop) =>
        for
          _ <- setModel(restorer)
          _ <- submitEffect(
                 fa = restorer.onRestore(model.get, restorer.value) >>
                   workingView.set(false).to[DefaultA],
                 rollback = setModel(inverse) >> stacks.mod(unpop),
                 onFailure = workingView.set(false)
               )
        yield ()
      .getOrEmpty

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit], // M is the old model, A is the new value
    onRestore: (M, A) => DefaultA[Unit]  // M is the old model, A is the new value
  )(v: A): DefaultS[Unit] =
    mod(getter, setter, onSet, onRestore)(_ => v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => DefaultA[Unit], // M is the old model, A is the new value
    onRestore: (M, A) => DefaultA[Unit]  // M is the old model, A is the new value
  )(f: A => A): DefaultS[Unit] =
    model.modCB(
      oldModel => setter(f(getter(oldModel)))(oldModel),
      (oldModel, newModel) =>
        val restorer: Restorer[DefaultA, M] =
          Restorer[DefaultA, M, A](oldModel, getter, setter, onRestore)
        push(undoStack)(restorer) >>
          reset(redoStack) >>
          submitEffect(
            fa = onSet(oldModel, getter(newModel)),
            rollback = setModel(restorer) >> undoStack.mod(_.filterNot(_ === restorer)),
            onFailure = Applicative[DefaultS].unit
          )
    )

  val undo: DefaultS[Unit] = popUndoStack.flatMap(restore)

  val redo: DefaultS[Unit] = popRedoStack.flatMap(restore)
}
