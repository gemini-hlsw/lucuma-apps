// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig as CoreTelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import observe.model.SystemOverrides
import observe.model.dhs.DataId
import observe.model.enums.Resource
import observe.server.engine.EngineStep

sealed trait StepGen[F[_]]:
  type D
  def atomId: Atom.Id
  def sequenceType: SequenceType
  def id: Step.Id
  def dataId: DataId
  def resources: Set[Resource | Instrument]
  def obsControl: SystemOverrides => InstrumentSystem.ObserveControl[F]
  def generator: StepActionsGen[F]
  def instConfig: D
  def config: StepConfig
  def telescopeConfig: CoreTelescopeConfig
  def signalToNoise: Option[SignalToNoise]
  def breakpoint: Breakpoint
  def statusGen: StepStatusGen = StepStatusGen.Null

object StepGen:
  type Aux[F[_], D0] = StepGen[F] { type D = D0 }

  type Factory[F[_], D] = (
    atomId: Atom.Id,
    sequenceType: SequenceType,
    id: Step.Id,
    dataId: DataId,
    resources: Set[Resource | Instrument],
    obsControl: SystemOverrides => InstrumentSystem.ObserveControl[F],
    generator: StepActionsGen[F],
    instConfig: D,
    config: StepConfig,
    telescopeConfig: CoreTelescopeConfig,
    signalToNoise: Option[SignalToNoise],
    breakpoint: Breakpoint
  ) => Aux[F, D]

  def generate[F[_]](
    step:            StepGen[F],
    systemOverrides: SystemOverrides,
    ctx:             HeaderExtraData
  ): (EngineStep[F], Breakpoint) =
    (EngineStep[F](step.id, step.generator.generate(ctx, systemOverrides)), step.breakpoint)

  case class GmosNorth[F[_]](
    atomId:          Atom.Id,
    sequenceType:    SequenceType,
    id:              Step.Id,
    dataId:          DataId,
    resources:       Set[Resource | Instrument],
    obsControl:      SystemOverrides => InstrumentSystem.ObserveControl[F],
    generator:       StepActionsGen[F],
    instConfig:      gmos.DynamicConfig.GmosNorth,
    config:          StepConfig,
    telescopeConfig: CoreTelescopeConfig,
    signalToNoise:   Option[SignalToNoise],
    breakpoint:      Breakpoint
  ) extends StepGen[F]:
    type D = gmos.DynamicConfig.GmosNorth

  case class GmosSouth[F[_]](
    atomId:          Atom.Id,
    sequenceType:    SequenceType,
    id:              Step.Id,
    dataId:          DataId,
    resources:       Set[Resource | Instrument],
    obsControl:      SystemOverrides => InstrumentSystem.ObserveControl[F],
    generator:       StepActionsGen[F],
    instConfig:      gmos.DynamicConfig.GmosSouth,
    config:          StepConfig,
    telescopeConfig: CoreTelescopeConfig,
    signalToNoise:   Option[SignalToNoise],
    breakpoint:      Breakpoint
  ) extends StepGen[F]:
    type D = gmos.DynamicConfig.GmosSouth

  case class Flamingos2[F[_]](
    atomId:          Atom.Id,
    sequenceType:    SequenceType,
    id:              Step.Id,
    dataId:          DataId,
    resources:       Set[Resource | Instrument],
    obsControl:      SystemOverrides => InstrumentSystem.ObserveControl[F],
    generator:       StepActionsGen[F],
    instConfig:      Flamingos2DynamicConfig,
    config:          StepConfig,
    telescopeConfig: CoreTelescopeConfig,
    signalToNoise:   Option[SignalToNoise],
    breakpoint:      Breakpoint
  ) extends StepGen[F]:
    type D = Flamingos2DynamicConfig

  case class Igrins2[F[_]](
    atomId:          Atom.Id,
    sequenceType:    SequenceType,
    id:              Step.Id,
    dataId:          DataId,
    resources:       Set[Resource | Instrument],
    obsControl:      SystemOverrides => InstrumentSystem.ObserveControl[F],
    generator:       StepActionsGen[F],
    instConfig:      Igrins2DynamicConfig,
    config:          StepConfig,
    telescopeConfig: CoreTelescopeConfig,
    signalToNoise:   Option[SignalToNoise],
    breakpoint:      Breakpoint
  ) extends StepGen[F]:
    type D = Igrins2DynamicConfig
