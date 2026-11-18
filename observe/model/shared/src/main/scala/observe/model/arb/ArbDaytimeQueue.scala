// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.enums.Instrument
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.core.util.arb.ArbTimestamp.given
import observe.model.*
import observe.model.enums.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.util.UUID

trait ArbDaytimeQueue extends ArbSubsystem:
  given Arbitrary[QueuePauseId] = Arbitrary(arbitrary[UUID].map(QueuePauseId(_)))
  given Cogen[QueuePauseId]     = Cogen[UUID].contramap(_.value)

  given Arbitrary[QueueItemId] = Arbitrary:
    Gen.oneOf(
      arbitrary[Observation.Id].map(QueueItemId.Obs(_)),
      arbitrary[QueuePauseId].map(QueueItemId.Pause(_))
    )

  given Cogen[QueueItemId] = Cogen[Either[Observation.Id, QueuePauseId]].contramap:
    case QueueItemId.Obs(id)   => Left(id)
    case QueueItemId.Pause(id) => Right(id)

  val genSubsystems: Gen[Set[Subsystem]] =
    Gen.choose(0, 4).flatMap(Gen.listOfN(_, arbitrary[Subsystem])).map(_.toSet)

  def genObs(obsId: Observation.Id): Gen[DaytimeQueueItem.Obs] =
    for
      ins <- arbitrary[Instrument]
      st  <- arbitrary[QueueItemStatus]
      ss  <- genSubsystems
      est <- Gen.choose(0L, 3600L).map(s => TimeSpan.unsafeFromMicroseconds(s * 1000000L))
      err <- arbitrary[Option[String]]
      ab  <- arbitrary[Boolean]
      at  <- arbitrary[Option[Timestamp]]
    yield DaytimeQueueItem.Obs(obsId, ins, st, ss + ins, est, err, ab, at, est)

  given Arbitrary[DaytimeQueueItem.Obs] = Arbitrary:
    arbitrary[Observation.Id].flatMap(genObs)

  given Arbitrary[DaytimeQueueItem] = Arbitrary:
    Gen.oneOf(
      arbitrary[DaytimeQueueItem.Obs],
      arbitrary[QueuePauseId].map(DaytimeQueueItem.Pause(_))
    )

  given Cogen[DaytimeQueueItem] =
    Cogen[(QueueItemId, Option[(Instrument, QueueItemStatus, List[Subsystem], TimeSpan)])]
      .contramap:
        case DaytimeQueueItem.Obs(id, i, st, ss, est, _, _, _, _) =>
          (QueueItemId.Obs(id), Some((i, st, ss.toList, est)))
        case DaytimeQueueItem.Pause(id)                           => (QueueItemId.Pause(id), None)

  given Cogen[DaytimeQueueItem.Obs] =
    Cogen[DaytimeQueueItem].contramap(identity)

  given Arbitrary[DaytimeQueue] = Arbitrary:
    for
      n      <- Gen.choose(0, 6)
      ids    <- Gen.listOfN(n, arbitrary[Observation.Id]).map(_.distinct)
      obs    <- Gen.sequence[List[DaytimeQueueItem.Obs], DaytimeQueueItem.Obs](ids.map(genObs))
      np     <- Gen.choose(0, 2)
      pauses <- Gen.listOfN(np, arbitrary[QueuePauseId]).map(_.distinct)
      items  <- Gen
                  .pick(obs.size + pauses.size, obs ++ pauses.map(DaytimeQueueItem.Pause(_)))
                  .map(_.toList)
      mode   <- arbitrary[QueueOrderingMode]
      state  <- arbitrary[QueueDispatchState]
      err    <- arbitrary[Option[String]]
      at     <- arbitrary[Option[Timestamp]]
    yield DaytimeQueue(items, mode, state, err, at)

  given Cogen[DaytimeQueue] =
    Cogen[
      (List[DaytimeQueueItem],
       QueueOrderingMode,
       QueueDispatchState,
       Option[String],
       Option[Timestamp]
      )
    ].contramap(q => (q.items, q.orderingMode, q.dispatchState, q.lampsOffError, q.lampsOffAt))

  given Arbitrary[DaytimeQueueView] = Arbitrary:
    for
      q <- arbitrary[DaytimeQueue]
      b <- genSubsystems
    yield DaytimeQueueView(q, b)

  given Cogen[DaytimeQueueView] =
    Cogen[(DaytimeQueue, List[Subsystem])].contramap(v => (v.queue, v.busySubsystems.toList))

object ArbDaytimeQueue extends ArbDaytimeQueue
