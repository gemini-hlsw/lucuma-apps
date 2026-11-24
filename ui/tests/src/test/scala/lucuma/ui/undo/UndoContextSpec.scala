// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.Dispatcher
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import lucuma.ui.optics.GetAdjust
import monocle.Iso

import scala.concurrent.duration.*

class UndoContextSpec extends munit.CatsEffectSuite {
  def idLens[A] = Iso.id[A]

  def id[A] = GetAdjust(idLens[A])

  val dispatcher = ResourceFunFixture(Dispatcher.parallel[IO])

  dispatcher.test("UndoRedo") { implicit dispatcher =>
    (for {
      undoable <- TestUndoable[Int](0)
      _        <- undoable.set(id[Int], 1)
      _        <- undoable.set(id[Int], 2)
      _        <- undoable.set(id[Int], 3)
      _        <- undoable.get.map(v => assertEquals(v, 3))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, 2))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, 1))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, 2))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, 3))
    } yield ()).runNow()
  }

  private val Failure: IO[Unit] = IO.raiseError(new RuntimeException("remote failed"))

  private val RollingBack: UndoEffectRunner[IO] =
    UndoEffectRunner.unordered[IO](_ => true, _ => IO.unit)

  // Runs `actions` in order on a fresh undoable, returning its final value and stacks.
  private def run(runner: Option[UndoEffectRunner[IO]])(
    actions: (TestUndoable[Int] => DefaultS[Unit])*
  ): IO[(Int, UndoStacks[IO, Int])] =
    TestControl.executeEmbed(
      Dispatcher
        .parallel[IO]
        .use: dispatcher =>
          given Dispatcher[IO] = dispatcher
          for
            undoable <- IO(runner.fold(TestUndoable[Int](0))(TestUndoable[Int](0, _)).runNow())
            _        <- actions.toList.traverse_(action => IO(action(undoable).runNow()))
            _        <- IO.sleep(1.second)
            value    <- IO(undoable.get.runNow())
            stacks   <- IO(undoable.stacks.runNow())
          yield (value, stacks)
    )

  test("a failing onSet of unknown outcome keeps the value and the history") {
    run(none)(_.set(id[Int], 1), _.setWith(id[Int], 2, Failure, IO.unit))
      .map: (value, stacks) =>
        assertEquals(value, 2)
        assertEquals(stacks.undo.size, 2)
        assertEquals(stacks.working, false)
  }

  test("a rejected onSet puts the model back and drops only its own entry") {
    run(RollingBack.some)(_.set(id[Int], 1), _.setWith(id[Int], 2, Failure, IO.unit))
      .map: (value, stacks) =>
        assertEquals(value, 1)
        assertEquals(stacks.undo.size, 1)
        assertEquals(stacks.redo.size, 0)
        assertEquals(stacks.working, false)
  }

  test("a failing onRestore of unknown outcome keeps the undo and resets working") {
    run(none)(_.setWith(id[Int], 1, IO.unit, Failure), _.undo)
      .map: (value, stacks) =>
        assertEquals(value, 0)
        assertEquals(stacks.undo.size, 0)
        assertEquals(stacks.redo.size, 1)
        assertEquals(stacks.working, false)
  }

  test("a rejected onRestore puts the model and the stacks back") {
    run(RollingBack.some)(_.setWith(id[Int], 1, IO.unit, Failure), _.undo)
      .map: (value, stacks) =>
        assertEquals(value, 1)
        assertEquals(stacks.undo.size, 1)
        assertEquals(stacks.redo.size, 0)
        assertEquals(stacks.working, false)
  }

  test("a rejected redo puts the model and the stacks back") {
    TestUndoable.simulated(0, IO.pure(RollingBack)): u =>
      for
        calls <- Ref[IO].of(0)
        // The first restore is the undo, the second is the redo.
        _     <- u.exec:
                   _.setWith(
                     id[Int],
                     1,
                     IO.unit,
                     calls.updateAndGet(_ + 1).flatMap(n => Failure.whenA(n > 1))
                   )
        _     <- u.exec(_.undo)
        _     <- u.exec(_.redo)
        v     <- u.value
        s     <- u.currentStacks
      yield
        assertEquals(v, 0)
        assertEquals(s.undo.size, 0)
        assertEquals(s.redo.size, 1)
        assertEquals(s.working, false)
  }

  test("undo and redo on empty stacks do nothing") {
    TestUndoable.simulated(0): u =>
      for
        _ <- u.exec(_.undo)
        _ <- u.exec(_.redo)
        v <- u.value
        s <- u.currentStacks
      yield
        assertEquals(v, 0)
        assertEquals(s, UndoStacks.empty[IO, Int])
  }

  test("a new edit clears the redo stack") {
    TestUndoable.simulated(0): u =>
      for
        _ <- u.exec(_.set(id[Int], 1))
        _ <- u.exec(_.set(id[Int], 2))
        _ <- u.exec(_.undo)
        _ <- u.exec(_.set(id[Int], 5))
        _ <- u.exec(_.redo)
        v <- u.value
        s <- u.currentStacks
      yield
        assertEquals(v, 5)
        assertEquals(s.undo.size, 2)
        assertEquals(s.redo.size, 0)
  }

  test("onSet and onRestore get the model before the change and the value after it") {
    TestUndoable.simulated(0): u =>
      for
        calls <- Ref[IO].of(List.empty[(String, Int, Int)])
        _     <- u.exec:
                   _.withContext:
                     _.set(
                       identity[Int],
                       (a: Int) => (_: Int) => a,
                       (m: Int, a: Int) => calls.update(_ :+ ("set", m, a)),
                       (m: Int, a: Int) => calls.update(_ :+ ("restore", m, a))
                     )(1)
        _     <- u.exec(_.undo)
        _     <- u.exec(_.redo)
        c     <- calls.get
      yield assertEquals(c, List(("set", 0, 1), ("restore", 1, 0), ("restore", 0, 1)))
  }

  test("working is set only while a restore effect runs") {
    TestUndoable.simulated(0): u =>
      for
        _      <- u.exec(_.setWith(id[Int], 1, IO.unit, IO.sleep(5.seconds)))
        before <- u.currentStacks.map(_.working)
        _      <- u.fire(_.undo)
        during <- u.currentStacks.map(_.working)
        _      <- IO.sleep(10.seconds)
        after  <- u.currentStacks.map(_.working)
      yield
        assertEquals(before, false)
        assertEquals(during, true)
        assertEquals(after, false)
  }

  private case object Rejected extends RuntimeException("rejected")

  private val Serial: IO[UndoEffectRunner[IO]] =
    UndoEffectRunner.serial[IO](
      10.seconds,
      {
        case Rejected => true
        case _        => false
      },
      _ => IO.unit,
      IO.unit
    )

  // Starts two edits whose effects finish in the opposite order, returning the order they ran in.
  private def effectOrder(runner: IO[UndoEffectRunner[IO]]): IO[List[Int]] =
    TestUndoable.simulated(0, runner): u =>
      for
        order <- Ref[IO].of(List.empty[Int])
        _     <- u.fire(_.setWith(id[Int], 1, IO.sleep(3.seconds) >> order.update(_ :+ 1), IO.unit))
        _     <- u.fire(_.setWith(id[Int], 2, order.update(_ :+ 2), IO.unit))
        _     <- IO.sleep(1.minute)
        o     <- order.get
      yield o

  test("a serial runner runs the effects in edit order") {
    effectOrder(Serial).map(assertEquals(_, List(1, 2)))
  }

  test("the local runner does not order the effects") {
    effectOrder(IO.pure(UndoEffectRunner.local[IO])).map(assertEquals(_, List(2, 1)))
  }

  test("a rejection reverts only its own edit and keeps the rest of the history") {
    TestUndoable.simulated(0, Serial): u =>
      for
        _      <- u.exec(_.set(id[Int], 1))
        _      <- u.exec:
                    _.setWith(id[Int], 2, IO.sleep(2.seconds) >> IO.raiseError(Rejected), IO.unit)
        v      <- u.value
        s      <- u.currentStacks
        _      <- u.exec(_.undo)
        undone <- u.value
      yield
        assertEquals(v, 1)
        assertEquals(s.undo.size, 1)
        assertEquals(s.redo.size, 0)
        assertEquals(undone, 0)
  }

  test("a timeout keeps the timed out edit and still runs the ones waiting behind it") {
    TestUndoable.simulated(0, Serial): u =>
      for
        ran <- Ref[IO].of(false)
        _   <- u.fire(_.setWith(id[Int], 1, IO.never, IO.unit))
        _   <- u.fire(_.setWith(id[Int], 2, ran.set(true), IO.unit))
        _   <- IO.sleep(1.minute)
        v   <- u.value
        s   <- u.currentStacks
        r   <- ran.get
      yield
        assertEquals(v, 2)
        assertEquals(s.undo.size, 2)
        assertEquals(r, true)
  }
}
