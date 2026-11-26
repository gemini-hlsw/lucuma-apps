// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.given
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.arb.ArbObservation
import lucuma.core.enums.CloneSequenceMode
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import scala.collection.immutable.SortedMap

class ObservationsAndGroupsSuite extends ScalaCheckSuite:
  import ArbObservation.given

  private val Zero: NonNegShort = NonNegShort.unsafeFrom(0)

  private def groupId(n: Long): Group.Id = Group.Id.fromLong(n).get

  private def group(n: Long, parent: Option[Long]): Group =
    Group(
      groupId(n),
      none,
      none,
      none,
      none,
      false,
      false,
      false,
      Nil,
      parent.map(groupId),
      Zero,
      none
    )

  private val obsGen: Gen[Observation] =
    org.scalacheck.Arbitrary.arbitrary[Observation]

  private def obsIn(obs: Observation, parent: Option[Long]): Observation =
    Observation.groupId.replace(parent.map(groupId))(obs)

  // Group 1 holds group 2, which holds group 3. Group 4 is a sibling of 1.
  private val groupList: GroupList =
    SortedMap.from(
      List(group(1, none), group(2, 1L.some), group(3, 2L.some), group(4, none)).map(g => g.id -> g)
    )

  private def state(obss: List[Observation]): ObservationsAndGroups =
    ObservationsAndGroups(SortedMap.from(obss.map(o => o.id -> o)), groupList)

  property("observations under a nested group are found at any depth, not the siblings'"):
    forAll(Gen.listOfN(5, obsGen)): obss =>
      val distinct = obss.distinctBy(_.id)
      if distinct.length < 5 then ()
      else
        val List(a, b, c, d, e) = distinct
        val oag                 = state(
          List(obsIn(a, 1L.some), obsIn(b, 2L.some), obsIn(c, 3L.some), obsIn(d, 4L.some), obsIn(e, none))
        )
        assertEquals(oag.observationsUnder(groupId(1)).map(_.id).toSet, Set(a.id, b.id, c.id))
        assertEquals(oag.observationsUnder(groupId(2)).map(_.id).toSet, Set(b.id, c.id))
        assertEquals(oag.observationsUnder(groupId(4)).map(_.id), List(d.id))

  test("a group with no observations has none under it"):
    assertEquals(state(Nil).observationsUnder(groupId(1)), Nil)
    assertEquals(state(Nil).descendantGroupIds(groupId(1)), Set(groupId(2), groupId(3)))

  property("adding then removing a clone returns the original state"):
    forAll(Gen.listOfN(3, obsGen)): obss =>
      val distinct = obss.distinctBy(_.id)
      val original = state(distinct.take(1).map(obsIn(_, 1L.some)))
      val clone    = GroupClone(
        group(10, none),
        List(group(11, 10L.some)),
        distinct.drop(1).map(obsIn(_, 11L.some))
      ).copy(observations = distinct.drop(1).filterNot(o => original.observations.contains(o.id)))
      assertEquals(original.addClone(clone).removeClone(clone.ids), original)

  property("a clone is read back by its ids"):
    forAll(Gen.listOfN(2, obsGen)): obss =>
      val clone = GroupClone(
        group(10, none),
        List(group(11, 10L.some)),
        obss.distinctBy(_.id).map(obsIn(_, 11L.some))
      )
      val oag   = state(Nil).addClone(clone)
      assertEquals(oag.cloneWithIds(clone.ids), clone.some)
      assertEquals(oag.removeClone(clone.ids).cloneWithIds(clone.ids), none)

  test("sequence choices are the distinct union ordered None, PendingSteps, AllSteps"):
    import CloneSequenceMode.*
    assertEquals(
      ObservationsAndGroups.mergeSequenceCopyChoices(
        List(List(AllSteps, None), List(PendingSteps, AllSteps), List(None))
      ),
      List(None, PendingSteps, AllSteps)
    )
    assertEquals(
      ObservationsAndGroups.mergeSequenceCopyChoices(List(List(None, AllSteps), List(AllSteps))),
      List(None, AllSteps)
    )

  test("sequence choices are empty when no observation has any"):
    assertEquals(ObservationsAndGroups.mergeSequenceCopyChoices(Nil), Nil)
    assertEquals(ObservationsAndGroups.mergeSequenceCopyChoices(List(Nil, Nil)), Nil)
    assertEquals(state(Nil).sequenceCopyChoices(groupId(1)), Nil)
