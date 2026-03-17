// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.syntax.eq.*
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import observe.model.Observer
import observe.model.SystemOverrides
import observe.model.enums.PendingObserveCmd
import observe.model.enums.Resource
import observe.server.engine.ActionCoordsInSeq
import observe.server.engine.Sequence
import lucuma.core.enums.Instrument
import observe.common.ObsQueriesGQL.ObsQuery.Data.Observation as OdbObservation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig

sealed trait SequenceData[F[_]]:
  type S
  type D
  def instrument: Instrument

  def observer: Option[Observer]
  def overrides: SystemOverrides
  def obsData: OdbObservation
  def staticCfg: S
  def currentStep: Option[StepGen.Aux[F, D]]
  def seq: Sequence.State[F]
  def pendingObsCmd: Option[PendingObserveCmd]
  def visitStartDone: Boolean
  def cleanup: F[Unit]

  def withCompleteVisitStart: SequenceData[F] = SequenceData.visitStartDone.replace(true)(this)

  val resources: Set[Resource | Instrument] =
    currentStep.map(_.resources).getOrElse(Set.empty)

  def configActionCoord(
    stepId: Step.Id,
    r:      Resource | Instrument
  ): Option[ActionCoordsInSeq] =
    currentStep
      .filter(_.id === stepId)
      .flatMap(_.generator.configActionCoord(r))
      .map { case (ex, ac) => ActionCoordsInSeq(stepId, ex, ac) }

  def resourceAtCoords(c: ActionCoordsInSeq): Option[Resource | Instrument] =
    currentStep
      .filter(_.id === c.stepId)
      .flatMap(_.generator.resourceAtCoords(c.execIdx, c.actIdx))

object SequenceData:
  def gmosNorth[F[_]]: Prism[SequenceData[F], SequenceData.GmosNorth[F]]   =
    GenPrism[SequenceData[F], SequenceData.GmosNorth[F]]
  def gmosSouth[F[_]]: Prism[SequenceData[F], SequenceData.GmosSouth[F]]   =
    GenPrism[SequenceData[F], SequenceData.GmosSouth[F]]
  def flamingos2[F[_]]: Prism[SequenceData[F], SequenceData.Flamingos2[F]] =
    GenPrism[SequenceData[F], SequenceData.Flamingos2[F]]

  def seq[F[_]]: Lens[SequenceData[F], Sequence.State[F]] =
    Lens[SequenceData[F], Sequence.State[F]](_.seq)(seq => {
      case gmosNorth(s)  => s.copy(seq = seq)
      case gmosSouth(s)  => s.copy(seq = seq)
      case flamingos2(s) => s.copy(seq = seq)
    })

  def overrides[F[_]]: Lens[SequenceData[F], SystemOverrides] =
    Lens[SequenceData[F], SystemOverrides](_.overrides)(overrides => {
      case gmosNorth(s)  => s.copy(overrides = overrides)
      case gmosSouth(s)  => s.copy(overrides = overrides)
      case flamingos2(s) => s.copy(overrides = overrides)
    })

  def pendingObsCmd[F[_]]: Lens[SequenceData[F], Option[PendingObserveCmd]] =
    Lens[SequenceData[F], Option[PendingObserveCmd]](_.pendingObsCmd)(pendingObsCmd => {
      case gmosNorth(s)  => s.copy(pendingObsCmd = pendingObsCmd)
      case gmosSouth(s)  => s.copy(pendingObsCmd = pendingObsCmd)
      case flamingos2(s) => s.copy(pendingObsCmd = pendingObsCmd)
    })

  def observer[F[_]]: Lens[SequenceData[F], Option[Observer]] =
    Lens[SequenceData[F], Option[Observer]](_.observer)(observer => {
      case gmosNorth(s)  => s.copy(observer = observer)
      case gmosSouth(s)  => s.copy(observer = observer)
      case flamingos2(s) => s.copy(observer = observer)
    })

  def visitStartDone[F[_]]: Lens[SequenceData[F], Boolean] =
    Lens[SequenceData[F], Boolean](_.visitStartDone)(visitStartDone => {
      case gmosNorth(s)  => s.copy(visitStartDone = visitStartDone)
      case gmosSouth(s)  => s.copy(visitStartDone = visitStartDone)
      case flamingos2(s) => s.copy(visitStartDone = visitStartDone)
    })

  case class GmosNorth[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      gmos.StaticConfig.GmosNorth,
    currentStep:    Option[StepGen.Aux[F, gmos.DynamicConfig.GmosNorth]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F]:
    type S = gmos.StaticConfig.GmosNorth
    type D = gmos.DynamicConfig.GmosNorth
    val instrument: Instrument = Instrument.GmosNorth

  case class GmosSouth[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      gmos.StaticConfig.GmosSouth,
    currentStep:    Option[StepGen.Aux[F, gmos.DynamicConfig.GmosSouth]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F]:
    type S = gmos.StaticConfig.GmosSouth
    type D = gmos.DynamicConfig.GmosSouth
    val instrument: Instrument = Instrument.GmosSouth

  case class Flamingos2[F[_]](
    observer:       Option[Observer],
    overrides:      SystemOverrides,
    obsData:        OdbObservation,
    staticCfg:      Flamingos2StaticConfig,
    currentStep:    Option[StepGen.Aux[F, Flamingos2DynamicConfig]],
    seq:            Sequence.State[F],
    pendingObsCmd:  Option[PendingObserveCmd],
    visitStartDone: Boolean,
    cleanup:        F[Unit]
  ) extends SequenceData[F]:
    type S = Flamingos2StaticConfig
    type D = Flamingos2DynamicConfig
    val instrument: Instrument = Instrument.Flamingos2
