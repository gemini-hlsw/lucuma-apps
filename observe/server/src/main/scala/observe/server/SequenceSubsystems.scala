// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepDigest
import lucuma.core.model.sequence.StepDigests
import lucuma.core.util.CalculationState
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.server.odb.OdbProxy
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/** The subsystems a running sequence holds, derived from what is left to execute of it. */
object SequenceSubsystems:

  /** How often a digest that is not up to date is read again. */
  val DigestPollInterval: FiniteDuration = 1.second

  /** How long to try for an up to date digest, reads included, before claiming conservatively. */
  val DigestMaxWait: FiniteDuration = 10.seconds

  private val CouldNotCalculate: String =
    "the ODB could not work out which subsystems it needs"

  private def didNotAnswer(e: Throwable): String =
    s"the ODB didn't answer which subsystems it needs (${e.getMessage})"

  private val NoAnswerInTime: String =
    s"the ODB didn't answer which subsystems it needs within ${DigestMaxWait.toSeconds} seconds"

  private val StillCalculating: String =
    s"the ODB was still working out which subsystems it needs after ${DigestMaxWait.toSeconds} seconds"

  private val TcsAndGcal: Set[Subsystem] =
    Set[Subsystem](ObservatorySubsystem.Tcs, ObservatorySubsystem.Gcal)

  private def isUpToDate(digest: OdbProxy.Digest): Boolean =
    digest.state === CalculationState.Ready

  private def any(digests: StepDigest*): Boolean = digests.exists(_.count.value > 0)

  private def hasObservingFlatOrArc(steps: StepDigests): Boolean =
    any(steps.observing, steps.flats, steps.arcs)

  private def hasBiasOrDark(steps: StepDigests): Boolean = any(steps.biases, steps.darks)

  /** What a start claims when the digest is missing, unreadable or not up to date. */
  def conservative(instrument: Instrument): Set[Subsystem] =
    Set[Subsystem](instrument) ++ TcsAndGcal

  /** The instrument plus what the remaining steps need; TCS and GCAL too if the digest is stale. */
  def fromDigest(instrument: Instrument, digest: Option[OdbProxy.Digest]): Set[Subsystem] =
    digest
      .filter(isUpToDate)
      .flatMap(_.value)
      .fold(conservative(instrument)): d =>
        val steps: StepDigests = d.steps
        // N&S info (its darks and biases need GCAL) will be added to the digest when N&S is supported.
        if hasObservingFlatOrArc(steps) then conservative(instrument)
        else if instrument === Instrument.Ghost && hasBiasOrDark(steps) then
          Set[Subsystem](instrument, ObservatorySubsystem.Gcal)
        else Set[Subsystem](instrument)

  /** The digest read for a claim and, if the claim must fall back to the conservative one, why. */
  case class DigestRead(digest: Option[OdbProxy.Digest], fallback: Option[String])

  /** Reads the digest, trying again while it fails or is not up to date; never fails. */
  def readDigest[F[_]: {Temporal, Logger}](
    odb:   OdbProxy[F],
    obsId: Observation.Id
  ): F[DigestRead] =
    // `reason` keeps why the last try was not good enough, for when the time runs out.
    def untilUpToDate(reason: Ref[F, String]): F[DigestRead] =
      odb
        .readDigest(obsId)
        .attempt
        .flatMap:
          case Right(digest) if isUpToDate(digest) && digest.value.isEmpty =>
            DigestRead(digest.some, CouldNotCalculate.some).pure[F]
          case Right(digest) if isUpToDate(digest)                         =>
            DigestRead(digest.some, none).pure[F]
          case result                                                      =>
            reason.set(result.fold(didNotAnswer, _ => StillCalculating)) >>
              Temporal[F].sleep(DigestPollInterval) >>
              untilUpToDate(reason)
    Ref
      .of[F, String](NoAnswerInTime)
      .flatMap: reason =>
        // The limit covers the reads too. A read still in flight is abandoned, not awaited, so
        // that one whose cancellation hangs cannot keep the start pending.
        untilUpToDate(reason)
          .timeoutAndForget(DigestMaxWait)
          .handleErrorWith(_ => reason.get.map(r => DigestRead(none, r.some)))
      .flatTap: cd =>
        cd.fallback.traverse_ : reason =>
          Logger[F].warn(
            s"Observation $obsId will keep TCS and GCAL to itself while it runs, because $reason."
          )
