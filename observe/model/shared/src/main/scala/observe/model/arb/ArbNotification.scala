// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import cats.data.NonEmptyList
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.Notification
import observe.model.Notification.*
import observe.model.Observation
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbNotification {

  given rcArb: Arbitrary[SubsystemConflict] = Arbitrary[SubsystemConflict] {
    for {
      obsId <- arbitrary[Observation.Id]
    } yield SubsystemConflict(obsId)
  }

  given rcCogen: Cogen[SubsystemConflict] =
    Cogen[Observation.Id].contramap(_.obsId)

  given rfArb: Arbitrary[LoadingFailed] = Arbitrary[LoadingFailed] {
    for
      oid  <- arbitrary[Observation.Id]
      strs <- arbitrary[List[String]]
    yield LoadingFailed(oid, strs)
  }

  given rfCogen: Cogen[LoadingFailed] =
    Cogen[(Observation.Id, List[String])].contramap(x => (x.obsId, x.msgs))

  given inArb: Arbitrary[InstrumentInUse] = Arbitrary[InstrumentInUse] {
    for {
      id <- arbitrary[Observation.Id]
      i  <- arbitrary[Instrument]
    } yield InstrumentInUse(id, i)
  }

  given inCogen: Cogen[InstrumentInUse] =
    Cogen[(Observation.Id, Instrument)].contramap(x => (x.obsId, x.ins))

  given subsArb: Arbitrary[SubsystemBusy] = Arbitrary[SubsystemBusy] {
    for {
      id <- arbitrary[Observation.Id]
      i  <- arbitrary[Step.Id]
      r  <- arbitrary[ObservatorySubsystem]
    } yield SubsystemBusy(id, i, r)
  }

  given subsCogen: Cogen[SubsystemBusy] =
    Cogen[(Observation.Id, Step.Id, Subsystem)].contramap(x => (x.obsId, x.stepId, x.subsystem))

  given snArb: Arbitrary[SequenceNotIdle] = Arbitrary[SequenceNotIdle] {
    for
      id <- arbitrary[Observation.Id]
      a  <- arbitrary[String]
    yield SequenceNotIdle(id, a)
  }

  given snCogen: Cogen[SequenceNotIdle] =
    Cogen[(Observation.Id, String)].contramap(x => (x.obsId, x.action))

  given afArb: Arbitrary[ActionFailed] = Arbitrary[ActionFailed] {
    for
      id <- arbitrary[Observation.Id]
      a  <- arbitrary[String]
      m  <- arbitrary[String]
    yield ActionFailed(id, a, m)
  }

  given afCogen: Cogen[ActionFailed] =
    Cogen[(Observation.Id, String, String)].contramap(x => (x.obsId, x.action, x.msg))

  given rrArb: Arbitrary[RestartRefused] = Arbitrary[RestartRefused] {
    for
      head <- arbitrary[String]
      tail <- arbitrary[List[String]]
    yield RestartRefused(NonEmptyList(head, tail))
  }

  given rrCogen: Cogen[RestartRefused] =
    Cogen[List[String]].contramap(_.reasons.toList)

  given notArb: Arbitrary[Notification] = Arbitrary[Notification] {
    for {
      r <- arbitrary[SubsystemConflict]
      a <- arbitrary[InstrumentInUse]
      f <- arbitrary[LoadingFailed]
      b <- arbitrary[SubsystemBusy]
      n <- arbitrary[SequenceNotIdle]
      x <- arbitrary[ActionFailed]
      y <- arbitrary[RestartRefused]
      s <- Gen.oneOf(r, a, f, b, n, x, y)
    } yield s
  }

  given notCogen: Cogen[Notification] =
    Cogen[
      Either[
        SubsystemConflict,
        Either[
          InstrumentInUse,
          Either[LoadingFailed, Either[SubsystemBusy, Either[SequenceNotIdle, Either[ActionFailed,
                                                                                     RestartRefused
          ]]]]
        ]
      ]
    ]
      .contramap {
        case r: SubsystemConflict => Left(r)
        case i: InstrumentInUse   => Right(Left(i))
        case f: LoadingFailed     => Right(Right(Left(f)))
        case b: SubsystemBusy     => Right(Right(Right(Left(b))))
        case n: SequenceNotIdle   => Right(Right(Right(Right(Left(n)))))
        case x: ActionFailed      => Right(Right(Right(Right(Right(Left(x))))))
        case y: RestartRefused    => Right(Right(Right(Right(Right(Right(y))))))
      }

}

object ArbNotification extends ArbNotification
