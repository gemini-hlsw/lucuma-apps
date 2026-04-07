// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.effect.IO
import cats.effect.std.Dispatcher
import lucuma.ui.optics.GetAdjust
import monocle.Iso

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
}
