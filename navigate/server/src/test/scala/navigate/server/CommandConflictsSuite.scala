// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import lucuma.core.enums.GuideProbe
import lucuma.core.util.TimeSpan
import munit.FunSuite
import navigate.model.NavigateCommand
import navigate.model.NavigateCommand.*
import navigate.model.enums.QlMode

class CommandConflictsSuite extends FunSuite {

  private val period = TimeSpan.unsafeFromMicroseconds(1000000)

  private val pwfs1: List[NavigateCommand] = List(
    Pwfs1Observe(period),
    Pwfs1StopObserve,
    Pwfs1CircularBuffer(true),
    Pwfs1QlMode(QlMode.On),
    WfsSky(GuideProbe.PWFS1, period)
  )

  private val pwfs2: List[NavigateCommand] = List(
    Pwfs2Observe(period),
    Pwfs2StopObserve,
    Pwfs2CircularBuffer(true),
    Pwfs2QlMode(QlMode.On),
    WfsSky(GuideProbe.PWFS2, period)
  )

  private val oiwfs: List[NavigateCommand] = List(
    OiwfsObserve(period),
    OiwfsStopObserve,
    OiwfsCircularBuffer(true),
    OiwfsQlMode(QlMode.On),
    WfsSky(GuideProbe.GmosOIWFS, period)
  )

  private val ac: List[NavigateCommand] = List(AcObserve(period), AcStopObserve)

  private val unrelated: List[NavigateCommand] = List(
    McsFollow(true),
    AgAllPark,
    DisableGuide,
    Pwfs1Follow(true),
    Pwfs1Park,
    Pwfs2Follow(true),
    WfsSky(GuideProbe.AltairAOWFS, period)
  )

  private def conflict(a: NavigateCommand, b: NavigateCommand): Boolean =
    CommandConflicts.between(a, b).isDefined

  private def pairs(cmds: List[NavigateCommand]) = for {
    a <- cmds
    b <- cmds
  } yield (a, b)

  List(("PWFS1", pwfs1), ("PWFS2", pwfs2), ("OIWFS", oiwfs), ("AC", ac)).foreach { (name, cmds) =>
    test(s"$name commands block themselves and each other") {
      pairs(cmds).foreach { (running, incoming) =>
        assert(conflict(running, incoming), s"$incoming should be blocked by $running")
      }
    }

    test(s"$name conflict message names the WFS") {
      assert(CommandConflicts.between(cmds.head, cmds.last).exists(_.contains(name)))
    }
  }

  test("There is no blocking between different WFS, or between a WFS and the AC") {
    val groups = List(pwfs1, pwfs2, oiwfs, ac)
    for {
      g1 <- groups
      g2 <- groups if g1 ne g2
      a  <- g1
      b  <- g2
    } assert(!conflict(a, b), s"$b should not be blocked by $a")
  }

  test("The second OIWFS probe maps to the OIWFS too") {
    assert(conflict(OiwfsObserve(period), WfsSky(GuideProbe.Flamingos2OIWFS, period)))
    assert(conflict(WfsSky(GuideProbe.Flamingos2OIWFS, period), OiwfsStopObserve))
  }

  test("WfsSky depends on the probe") {
    assert(conflict(WfsSky(GuideProbe.PWFS1, period), WfsSky(GuideProbe.PWFS1, period)))
    assert(!conflict(WfsSky(GuideProbe.PWFS1, period), WfsSky(GuideProbe.PWFS2, period)))
    assert(!conflict(WfsSky(GuideProbe.PWFS1, period), Pwfs2Observe(period)))
  }

  test("Other commands are not blocked, and do not block") {
    (pwfs1 ++ pwfs2 ++ oiwfs ++ ac).foreach { w =>
      unrelated.foreach { u =>
        assert(!conflict(w, u), s"$u should not be blocked by $w")
        assert(!conflict(u, w), s"$w should not be blocked by $u")
      }
    }
    pairs(unrelated).foreach((a, b) => assert(!conflict(a, b), s"$b should not be blocked by $a"))
  }

}
