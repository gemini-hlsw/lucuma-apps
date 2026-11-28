// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.epics

import cats.Applicative
import cats.Eq
import cats.Id
import cats.data.Writer
import cats.laws.discipline.ApplicativeTests
import cats.syntax.all.*
import munit.DisciplineSuite
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics.*
import org.epics.ca.AccessRights
import org.epics.ca.ConnectionState
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import scala.concurrent.duration.FiniteDuration

class VerifiedEpicsLawsSuite extends DisciplineSuite {
  import VerifiedEpicsLawsSuite.{*, given}

  checkAll(
    "Applicative[ChannelTracker[Id, *]]", {
      given Applicative[[A] =>> ChannelTracker[Id, A]] = applicativeChannelTracker[Id]
      ApplicativeTests[[A] =>> ChannelTracker[Id, A]].applicative[Int, Int, String]
    }
  )

  checkAll(
    "Applicative[VerifiedEpics[Id, List, *]]",
    ApplicativeTests[[A] =>> VerifiedEpics[Id, List, A]].applicative[Int, Int, String]
  )

  test("mapN merges the channels and runs the effects from left to right") {
    type Logged[A] = Writer[List[String], A]

    def read(ch: StubChannel): VerifiedEpics[Id, Logged, String] =
      tracker(Map(telltales.head -> Set(ch)), Writer(List(ch.name), ch.name))

    val (a, b, c) = (channels(0), channels(1), channels(2))
    val r         = (read(a), read(b), read(c)).mapN((x, y, z) => s"$x$y$z")

    assertEquals(r.systems, Map(telltales.head -> Set[RemoteChannel[Id]](a, b, c)))
    assertEquals(r.run.run, (List(a.name, b.name, c.name), s"${a.name}${b.name}${c.name}"))
  }
}

object VerifiedEpicsLawsSuite {

  case class StubChannel(name: String) extends RemoteChannel[Id] {
    override def connect: Unit                          = ()
    override def connect(timeout: FiniteDuration): Unit = ()
    override def disconnect: Unit                       = ()
    override def getName: String                        = name
    override def getConnectionState: ConnectionState    = ConnectionState.CONNECTED
    override def getAccessRights: AccessRights          = AccessRights.READ_WRITE
  }

  val telltales: List[TelltaleChannel[Id]] =
    List("A", "B").map(s => TelltaleChannel[Id](s, StubChannel(s"$s:health")))

  val channels: List[StubChannel] = List("a", "b", "c", "d").map(StubChannel(_))

  def tracker[A](s: Map[TelltaleChannel[Id], Set[RemoteChannel[Id]]], a: A): ChannelTracker[Id, A] =
    new ChannelTracker[Id, A] {
      override val systems: Map[TelltaleChannel[Id], Set[RemoteChannel[Id]]] = s
      override val run: A                                                    = a
    }

  val genSystems: Gen[Map[TelltaleChannel[Id], Set[RemoteChannel[Id]]]] =
    Gen.mapOf(
      for {
        tt  <- Gen.oneOf(telltales)
        chs <- Gen.someOf(channels)
      } yield tt -> chs.toSet[RemoteChannel[Id]]
    )

  given [A: Arbitrary]: Arbitrary[ChannelTracker[Id, A]] =
    Arbitrary(
      for {
        s <- genSystems
        a <- Arbitrary.arbitrary[A]
      } yield tracker(s, a)
    )

  given [A: Eq]: Eq[ChannelTracker[Id, A]] = Eq.by(c => (c.systems, c.run))

}
