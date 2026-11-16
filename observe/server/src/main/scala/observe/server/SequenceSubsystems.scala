// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Functor
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import observe.common.RemainingStepSubquery
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.server.odb.OdbProxy

/** The subsystems a running sequence holds: its instrument and what its remaining steps use. */
object SequenceSubsystems:

  // A step whose kind cannot be told might need either.
  private val Unclassified: Set[ObservatorySubsystem] =
    Set(ObservatorySubsystem.Tcs, ObservatorySubsystem.Gcal)

  /** The observatory subsystems one remaining step uses, by the translator's own rule. */
  def ofStep(instrument: Instrument, isNodAndShuffle: Boolean)(
    step: RemainingStepSubquery.Data
  ): Set[ObservatorySubsystem] =
    StepKind
      .classify(instrument, step.stepConfig, step.observeClass, isNodAndShuffle)
      .fold(_ => Unclassified, StepKind.observatorySubsystems)

  /** The instrument plus what any of the remaining steps uses. */
  def fromRemainingSteps(
    instrument: Instrument,
    remaining:  OdbProxy.RemainingSteps
  ): Set[Subsystem] =
    Set[Subsystem](instrument) ++
      remaining.steps.foldMap(ofStep(instrument, remaining.isNodAndShuffle))

  /** Reads the remaining steps of `obsId` and works out what it must hold while it runs. */
  def read[F[_]: Functor](
    odb:        OdbProxy[F],
    obsId:      Observation.Id,
    instrument: Instrument
  ): F[Set[Subsystem]] =
    odb.readRemainingSteps(obsId, instrument).map(fromRemainingSteps(instrument, _))
