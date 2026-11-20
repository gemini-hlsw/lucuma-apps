// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Sync
import cats.effect.syntax.all.*
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB
import observe.common.ObsQueriesGql.*
import observe.common.RemainingSequenceSubquery
import observe.common.RemainingStepSubquery
import observe.model.dhs.*
import observe.server.ObserveFailure

trait OdbProxy[F[_]] private[odb] () extends OdbCommands[F] {

  /** Reads the observation with up to `futureLimit` atoms beyond the next one of each sequence. */
  def read(
    oid:         Observation.Id,
    futureLimit: NonNegInt = OdbProxy.FullFuture
  ): F[OdbObservationData]

  /** Reads the role and mode of an observation, which say whether the daytime queue takes it. */
  def readQueueObservation(oid: Observation.Id): F[OdbQueueObservation]

  def resetAcquisition(obsId: Observation.Id): F[Unit]

  /**
   * Reads `instrument`'s execution config with up to `futureLimit` atoms beyond the next one in
   * `possibleFuture`. Both narrow the response, which crosses the network on every step.
   */
  def readExecutionConfig(
    oid:         Observation.Id,
    instrument:  Instrument,
    futureLimit: NonNegInt
  ): F[InstrumentExecutionConfig]

  /** Reads the steps left in both sequences, with only what classifying each of them needs. */
  def readRemainingSteps(oid: Observation.Id, instrument: Instrument): F[OdbProxy.RemainingSteps]
}

object OdbProxy {

  /** Enough to run the next step: the engine only needs `nextAtom` after a step completes. */
  val NextAtomOnly: NonNegInt = NonNegInt.unsafeFrom(0)

  /** The whole sequence, for loading it or jumping to a step the user picked. */
  val FullFuture: NonNegInt = NonNegInt.unsafeFrom(100)

  /**
   * The steps left to execute, and whether the sequence is Nod&Shuffle, which changes their kind.
   */
  case class RemainingSteps(isNodAndShuffle: Boolean, steps: List[RemainingStepSubquery.Data]):
    /** The time the steps are expected to take. */
    def estimate: TimeSpan = steps.foldMap(_.estimate.total)

  private def stepsOf(
    sequence: Option[RemainingSequenceSubquery.Data]
  ): List[RemainingStepSubquery.Data] =
    sequence.foldMap(s => s.nextAtom.steps ++ s.possibleFuture.flatMap(_.steps))

  private def remainingSteps(
    instrument: Instrument,
    config:     RemainingStepsQuery.Data.ExecutionConfig
  ): Option[RemainingSteps] =
    instrument match
      case Instrument.GmosNorth  =>
        config.gmosNorth.map: c =>
          RemainingSteps(c.static.nodAndShuffle.isDefined,
                         stepsOf(c.acquisition) ++ stepsOf(c.science)
          )
      case Instrument.GmosSouth  =>
        config.gmosSouth.map: c =>
          RemainingSteps(c.static.nodAndShuffle.isDefined,
                         stepsOf(c.acquisition) ++ stepsOf(c.science)
          )
      case Instrument.Flamingos2 =>
        config.flamingos2.map(c =>
          RemainingSteps(false, stepsOf(c.acquisition) ++ stepsOf(c.science))
        )
      case Instrument.Igrins2    =>
        config.igrins2.map(c => RemainingSteps(false, stepsOf(c.acquisition) ++ stepsOf(c.science)))
      case Instrument.Gnirs      =>
        config.gnirs.map(c => RemainingSteps(false, stepsOf(c.acquisition) ++ stepsOf(c.science)))
      case Instrument.Ghost      =>
        config.ghost.map(c => RemainingSteps(false, stepsOf(c.science)))
      case _                     => none

  def apply[F[_]](
    evCmds:    OdbCommands[F],
    stepSpans: StepSpans[F]
  )(using FetchClient[F, ObservationDB])(using F: Sync[F]): OdbProxy[F] =
    new OdbProxy[F] {
      override def read(oid: Observation.Id, futureLimit: NonNegInt): F[OdbObservationData] =
        evCmds.flushEvents(oid) >>
          ObsCalibrationRoleQuery[F]
            .query(oid)
            .raiseGraphQLErrors
            .map(_.observation.flatMap(_.calibrationRole))
            .flatMap: calibrationRole =>
              val skipTargets: Boolean = calibrationRole.exists(_.isOffSky)
              ObsQuery[F]
                .query(oid, skipTargets, futureLimit)
                .raiseGraphQLErrors
                .flatMap: data =>
                  (data.observation, data.executionConfig).tupled
                    .fold(
                      F.raiseError[OdbObservationData]:
                        ObserveFailure.Unexpected(s"OdbProxy: Unable to read observation $oid")
                    )((obs, ec) => OdbObservationData(obs, ec).pure[F])

      override def readQueueObservation(oid: Observation.Id): F[OdbQueueObservation] =
        evCmds.flushEvents(oid) >>
          ObsQueueQuery[F]
            .query(oid)
            .raiseGraphQLErrors
            .flatMap:
              _.observation
                .map(o => OdbQueueObservation(o.calibrationRole, o.observingMode))
                .liftTo[F](ObserveFailure.Unexpected(s"OdbProxy: Unable to read observation $oid"))

      override def readExecutionConfig(
        oid:         Observation.Id,
        instrument:  Instrument,
        futureLimit: NonNegInt
      ): F[InstrumentExecutionConfig] =
        instrument match
          case Instrument.GmosNorth  =>
            readingConfig(oid):
              GmosNorthExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.gmosNorth))
          case Instrument.GmosSouth  =>
            readingConfig(oid):
              GmosSouthExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.gmosSouth))
          case Instrument.Flamingos2 =>
            readingConfig(oid):
              Flamingos2ExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.flamingos2))
          case Instrument.Igrins2    =>
            readingConfig(oid):
              Igrins2ExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.igrins2))
          case Instrument.Gnirs      =>
            readingConfig(oid):
              GnirsExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.gnirs))
          case Instrument.Ghost      =>
            readingConfig(oid):
              GhostExecutionQuery[F]
                .query(oid, futureLimit)
                .raiseGraphQLErrors
                .map(_.executionConfig.flatMap(_.ghost))
          case other                 =>
            F.raiseError:
              ObserveFailure.Unexpected(s"OdbProxy: $other cannot execute sequences")

      // The sequence the ODB generates depends on the events we sent, so they have to be in first.
      // The engine calls this after each step to load the next one, so it is the last ODB wait of
      // a step and closes the step's span.
      private def readingConfig[A <: InstrumentExecutionConfig](oid: Observation.Id)(
        read: F[Option[A]]
      ): F[InstrumentExecutionConfig] =
        stepSpans
          .wait(StepSpans.ReadExecutionConfig, oid):
            evCmds.flushEvents(oid) >>
              read.flatMap:
                _.fold(
                  F.raiseError[InstrumentExecutionConfig]:
                    ObserveFailure.Unexpected(s"OdbProxy: Unable to read observation $oid")
                )(_.pure[F])
          .guarantee(stepSpans.endCurrent(oid))

      override def readRemainingSteps(
        oid:        Observation.Id,
        instrument: Instrument
      ): F[RemainingSteps] =
        evCmds.flushEvents(oid) >>
          RemainingStepsQuery[F]
            .query(
              oid,
              FullFuture,
              includeGmosNorth = instrument === Instrument.GmosNorth,
              includeGmosSouth = instrument === Instrument.GmosSouth,
              includeFlamingos2 = instrument === Instrument.Flamingos2,
              includeIgrins2 = instrument === Instrument.Igrins2,
              includeGnirs = instrument === Instrument.Gnirs,
              includeGhost = instrument === Instrument.Ghost
            )
            .raiseGraphQLErrors
            .flatMap:
              _.executionConfig
                .flatMap(remainingSteps(instrument, _))
                .liftTo[F]:
                  ObserveFailure.Unexpected(s"OdbProxy: Unable to read the steps left in $oid")

      override def resetAcquisition(obsId: Observation.Id): F[Unit] =
        evCmds.flushEvents(obsId) >>
          ResetAcquisitionMutation[F].execute(obsId = obsId).void

      export evCmds.*
    }

}
