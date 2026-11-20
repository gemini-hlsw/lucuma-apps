// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.Dispatcher
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.odb.data.AltairConfiguration
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class AltairControllerSuite extends CatsEffectSuite {
  private given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("navigate-engine")

  private def config(mode: AltairMode): AltairConfiguration =
    AltairConfiguration(mode, FieldLens.In.some, CassRotator.Following, AltairNdFilter.In)

  // Runs the test on a virtual clock, so the waits don't take real time
  private def altairTest(name: String)(
    body: (
      AltairController[IO],
      Ref[IO, TestTcsEpicsSystem.State],
      Ref[IO, TestAltairEpicsSystem.State]
    ) => IO[Any]
  )(using munit.Location): Unit =
    test(name)(
      TestControl.executeEmbed(
        Dispatcher.parallel[IO](await = true).use { d =>
          given Dispatcher[IO] = d
          for {
            tcs  <- Ref.of[IO, TestTcsEpicsSystem.State](TestTcsEpicsSystem.defaultState)
            ao   <- Ref.of[IO, TestAltairEpicsSystem.State](TestAltairEpicsSystem.defaultState)
            ctrl <- AltairController.fromEpics[IO](TestAltairEpicsSystem.build(ao),
                                                   TestTcsEpicsSystem.build(tcs),
                                                   5.seconds
                    )
            _    <- body(ctrl, tcs, ao)
          } yield ()
        }
      )
    )

  private def assertRejected(r: IO[?]): IO[Unit] =
    r.attempt.map(e => assert(e.left.exists(_.isInstanceOf[AltairController.AltairException]), e))

  altairTest("Follow is refused if Altair has not been configured") { (ctrl, tcs, _) =>
    for {
      _ <- assertRejected(ctrl.follow(true))
      s <- tcs.get
    } yield assertEquals(s.aoFollow.value, none)
  }

  altairTest("Follow and unfollow in NGS") { (ctrl, tcs, _) =>
    for {
      _  <- ctrl.configure(config(AltairMode.Ngs).some)
      _  <- ctrl.follow(true)
      s1 <- tcs.get
      _  <- ctrl.follow(false)
      s2 <- tcs.get
    } yield {
      assert(s1.aoFollow.connected)
      assertEquals(s1.aoFollow.value, "On".some)
      assertEquals(s2.aoFollow.value, "Off".some)
    }
  }

  altairTest("Follow in LGS") { (ctrl, tcs, _) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Lgs).some)
      _ <- ctrl.follow(true)
      s <- tcs.get
    } yield assertEquals(s.aoFollow.value, "On".some)
  }

  // REL-4159: the probe must never be commanded in LGS with P1, in either direction
  altairTest("Follow is refused in LGS with P1, in both directions") { (ctrl, tcs, _) =>
    for {
      _ <- ctrl.configure(config(AltairMode.LgsP1).some)
      _ <- assertRejected(ctrl.follow(true))
      _ <- assertRejected(ctrl.follow(false))
      s <- tcs.get
    } yield assertEquals(s.aoFollow.value, none)
  }

  altairTest("Follow is refused after Altair stops being used") { (ctrl, tcs, _) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Ngs).some)
      _ <- ctrl.configure(none)
      _ <- assertRejected(ctrl.follow(true))
      s <- tcs.get
    } yield assertEquals(s.aoFollow.value, none)
  }

  altairTest("Park in NGS stops following and parks") { (ctrl, tcs, _) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Ngs).some)
      _ <- ctrl.park
      s <- tcs.get
    } yield {
      assertEquals(s.aoFollow.value, "Off".some)
      assert(s.aoPark.connected)
      assertEquals(s.aoPark.value, "1".some)
    }
  }

  altairTest("Park waits for the probe to stop") { (ctrl, tcs, ao) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Ngs).some)
      _ <- ao.update(_.copy(wfsPositionBusy = true))
      _ <- (IO.sleep(3.seconds) *> ao.update(_.copy(wfsPositionBusy = false))).start
      _ <- ctrl.park
      s <- tcs.get
    } yield assertEquals(s.aoPark.value, "1".some)
  }

  altairTest("Park fails, without parking, if the probe does not stop") { (ctrl, tcs, ao) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Ngs).some)
      _ <- ao.update(_.copy(wfsPositionBusy = true))
      _ <- assertRejected(ctrl.park)
      s <- tcs.get
    } yield assertEquals(s.aoPark.value, none)
  }

  altairTest("Park fails, without parking, if Altair does not go idle") { (ctrl, tcs, ao) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Ngs).some)
      _ <- ao.update(_.copy(busy = true))
      _ <- assertRejected(ctrl.park)
      s <- tcs.get
    } yield assertEquals(s.aoPark.value, none)
  }

  altairTest("Park is not supported yet in LGS") { (ctrl, tcs, _) =>
    for {
      _ <- ctrl.configure(config(AltairMode.Lgs).some)
      _ <- assertRejected(ctrl.park)
      s <- tcs.get
    } yield {
      assertEquals(s.aoFollow.value, none)
      assertEquals(s.aoPark.value, none)
    }
  }

  altairTest("Park is refused if Altair has not been configured") { (ctrl, tcs, _) =>
    for {
      _ <- assertRejected(ctrl.park)
      s <- tcs.get
    } yield assertEquals(s.aoPark.value, none)
  }
}
