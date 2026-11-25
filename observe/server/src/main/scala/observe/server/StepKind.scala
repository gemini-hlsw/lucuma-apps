// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.*
import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.StepConfig
import observe.model.enums.ObservatorySubsystem
import observe.model.extensions.*
import observe.server.ObserveFailure.Unexpected

sealed trait StepKind extends Product with Serializable {
  def instrument: Instrument
}

object StepKind {
  final case class CelestialObject(override val instrument: Instrument)     extends StepKind
  final case class NodAndShuffle(override val instrument: Instrument)       extends StepKind
  // Science steps in a daytime calibration: the light goes from the sky to the instrument, without
  // the telescope pointing at a target, and GCAL is off
  final case class DayCalObject(override val instrument: Instrument)        extends StepKind
  final case class Gems(override val instrument: Instrument)                extends StepKind
  final case class AltairObs(override val instrument: Instrument)           extends StepKind
  // Flats or Arcs that can be taken without caring about OI guiding
  final case class FlatOrArc(override val instrument: Instrument, gcalCfg: StepConfig.Gcal)
      extends StepKind
  // Flats or Arcs that must care about OI guiding
  final case class NightFlatOrArc(override val instrument: Instrument, gcalCfg: StepConfig.Gcal)
      extends StepKind
  final case class DarkOrBias(override val instrument: Instrument)          extends StepKind
  final case class DarkOrBiasNS(override val instrument: Instrument)        extends StepKind
  final case class ExclusiveDarkOrBias(override val instrument: Instrument) extends StepKind
//  case object AlignAndCalib                                                 extends StepType {
//    override val instrument: Instrument = Instrument.Gpi
//  }

  given Eq[StepKind] = Eq.instance {
    case (CelestialObject(i), CelestialObject(j))         => i === j
    case (NodAndShuffle(i), NodAndShuffle(j))             => i === j
    case (DayCalObject(i), DayCalObject(j))               => i === j
    case (Gems(i), Gems(j))                               => i === j
    case (AltairObs(i), AltairObs(j))                     => i === j
    case (FlatOrArc(i, a), FlatOrArc(j, b))               => i === j && a === b
    case (NightFlatOrArc(i, a), NightFlatOrArc(j, b))     => i === j && a === b
    case (DarkOrBias(i), DarkOrBias(j))                   => i === j
    case (DarkOrBiasNS(i), DarkOrBiasNS(j))               => i === j
    case (ExclusiveDarkOrBias(i), ExclusiveDarkOrBias(j)) => i === j
//    case (AlignAndCalib, AlignAndCalib)                   => true
    case _                                                => false
  }

  private def standard(
    inst:            Instrument,
    stepConfig:      StepConfig,
    obsClass:        ObserveClass,
    calibrationRole: Option[CalibrationRole]
  ): Either[ObserveFailure, StepKind] = stepConfig match {
    case StepConfig.Bias | StepConfig.Dark => DarkOrBias(inst).asRight
    case c: StepConfig.Gcal                =>
      if (obsClass =!= ObserveClass.DayCal && inst.hasOI) NightFlatOrArc(inst, c).asRight
      else FlatOrArc(inst, c).asRight
    case StepConfig.Science                =>
      // TODO: Here goes the logic to differentiate between a non GAOS, GeMS ot Altair observation.
      // Twilights are DayCal too, but they are taken on sky.
      if (calibrationRole.exists(_.isOffSky)) DayCalObject(inst).asRight
      else CelestialObject(inst).asRight
    case StepConfig.SmartGcal(_)           => Unexpected("Smart GCAL is not supported").asLeft
  }

  /** The kind of a step: the one rule for the translator and for what a sequence holds. */
  def classify(
    inst:            Instrument,
    stepConfig:      StepConfig,
    obsClass:        ObserveClass,
    calibrationRole: Option[CalibrationRole],
    isNodAndShuffle: Boolean
  ): Either[ObserveFailure, StepKind] =
    val stdType: Either[ObserveFailure, StepKind] =
      standard(inst, stepConfig, obsClass, calibrationRole)
    val withNs: Either[ObserveFailure, StepKind]  =
      if (isNodAndShuffle)
        stdType.flatMap {
          case DarkOrBias(_)      => DarkOrBiasNS(inst).asRight
          case CelestialObject(_) => NodAndShuffle(inst).asRight
          case st                 => Unexpected(s"N&S is not supported for steps of type $st").asLeft
        }
      else stdType
    // GHOST darks and biases must run with the GCAL shutter closed.
    if (inst === Instrument.Ghost)
      withNs.map {
        case DarkOrBias(i) => ExclusiveDarkOrBias(i)
        case other         => other
      }
    else withNs

  /** The observatory subsystems a step of `kind` uses. */
  def observatorySubsystems(kind: StepKind): Set[ObservatorySubsystem] = kind match {
    case DarkOrBias(_)                            => Set.empty
    case ExclusiveDarkOrBias(_) | DarkOrBiasNS(_) => Set(ObservatorySubsystem.Gcal)
    case _                                        => Set(ObservatorySubsystem.Tcs, ObservatorySubsystem.Gcal)
  }
}
