// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation as OdbObservation
import observe.model.Observer
import observe.model.Subsystem
import observe.model.SystemOverrides
import observe.model.enums.PendingObserveCmd
import observe.server.engine.ActionCoordsInSeq
import observe.server.engine.LoadedStep
import observe.server.engine.SequenceState

import OdbObservation.TargetEnvironment

sealed trait SequenceData[F[_]]:
  type D
  def instrument: Instrument

  def observer: Option[Observer]
  def overrides: SystemOverrides
  def targetEnvironment: TargetEnvironment
  def constraintSet: ConstraintSet
  def seq: SequenceState[F]
  def pendingObsCmd: Option[PendingObserveCmd]
  def visitStartDone: Boolean

  lazy val obsId: Observation.Id             = seq.obsId
  lazy val currentSequenceType: SequenceType = seq.currentSequenceType
  lazy val loadedStep: Option[LoadedStep[F]] = seq.loadedStep

  def withCompleteVisitStart: SequenceData[F] = SequenceData.visitStartDone.replace(true)(this)

  lazy val resources: Set[Subsystem] =
    loadedStep.map(_.resources).getOrElse(Set.empty)

  def configActionCoord(
    stepId: Step.Id,
    r:      Subsystem
  ): Option[ActionCoordsInSeq] =
    loadedStep
      .filter(_.id === stepId)
      .flatMap(_.generator.configActionCoord(r))
      .map { case (ex, ac) => ActionCoordsInSeq(stepId, ex, ac) }

  def resourceAtCoords(c: ActionCoordsInSeq): Option[Subsystem] =
    loadedStep
      .filter(_.id === c.stepId)
      .flatMap(_.generator.resourceAtCoords(c.execIdx, c.actIdx))

object SequenceData:
  type Aux[F[_], S0, D0] = SequenceData[F] { type S = S0; type D = D0 }

  def gmosNorth[F[_]]: Prism[SequenceData[F], SequenceData.GmosNorth[F]]   =
    GenPrism[SequenceData[F], SequenceData.GmosNorth[F]]
  def gmosSouth[F[_]]: Prism[SequenceData[F], SequenceData.GmosSouth[F]]   =
    GenPrism[SequenceData[F], SequenceData.GmosSouth[F]]
  def flamingos2[F[_]]: Prism[SequenceData[F], SequenceData.Flamingos2[F]] =
    GenPrism[SequenceData[F], SequenceData.Flamingos2[F]]
  def igrins2[F[_]]: Prism[SequenceData[F], SequenceData.Igrins2[F]]       =
    GenPrism[SequenceData[F], SequenceData.Igrins2[F]]

  def seq[F[_]]: Lens[SequenceData[F], SequenceState[F]] =
    Lens[SequenceData[F], SequenceState[F]](_.seq)(seq => {
      case gmosNorth(s)  => s.copy(seq = seq)
      case gmosSouth(s)  => s.copy(seq = seq)
      case flamingos2(s) => s.copy(seq = seq)
      case igrins2(s)    => s.copy(seq = seq)
      case other         => other // should not happen, but needed to satisfy exhaustivity check
    })

  def overrides[F[_]]: Lens[SequenceData[F], SystemOverrides] =
    Lens[SequenceData[F], SystemOverrides](_.overrides)(overrides => {
      case gmosNorth(s)  => s.copy(overrides = overrides)
      case gmosSouth(s)  => s.copy(overrides = overrides)
      case flamingos2(s) => s.copy(overrides = overrides)
      case igrins2(s)    => s.copy(overrides = overrides)

      case other => other // should not happen, but needed to satisfy exhaustivity check
    })

  def pendingObsCmd[F[_]]: Lens[SequenceData[F], Option[PendingObserveCmd]] =
    Lens[SequenceData[F], Option[PendingObserveCmd]](_.pendingObsCmd)(pendingObsCmd => {
      case gmosNorth(s)  => s.copy(pendingObsCmd = pendingObsCmd)
      case gmosSouth(s)  => s.copy(pendingObsCmd = pendingObsCmd)
      case flamingos2(s) => s.copy(pendingObsCmd = pendingObsCmd)
      case igrins2(s)    => s.copy(pendingObsCmd = pendingObsCmd)
      case other         => other // should not happen, but needed to satisfy exhaustivity check
    })

  def observer[F[_]]: Lens[SequenceData[F], Option[Observer]] =
    Lens[SequenceData[F], Option[Observer]](_.observer)(observer => {
      case gmosNorth(s)  => s.copy(observer = observer)
      case gmosSouth(s)  => s.copy(observer = observer)
      case flamingos2(s) => s.copy(observer = observer)
      case igrins2(s)    => s.copy(observer = observer)
      case other         => other // should not happen, but needed to satisfy exhaustivity check
    })

  def visitStartDone[F[_]]: Lens[SequenceData[F], Boolean] =
    Lens[SequenceData[F], Boolean](_.visitStartDone)(visitStartDone => {
      case gmosNorth(s)  => s.copy(visitStartDone = visitStartDone)
      case gmosSouth(s)  => s.copy(visitStartDone = visitStartDone)
      case flamingos2(s) => s.copy(visitStartDone = visitStartDone)
      case igrins2(s)    => s.copy(visitStartDone = visitStartDone)
      case other         => other // should not happen, but needed to satisfy exhaustivity check
    })

  case class GmosNorth[F[_]](
    observer:          Option[Observer],
    overrides:         SystemOverrides,
    targetEnvironment: TargetEnvironment,
    constraintSet:     ConstraintSet,
    staticCfg:         gmos.StaticConfig.GmosNorth,
    seq:               SequenceState[F],
    pendingObsCmd:     Option[PendingObserveCmd],
    visitStartDone:    Boolean
  ) extends SequenceData[F]:
    type D = gmos.DynamicConfig.GmosNorth
    val instrument: Instrument = Instrument.GmosNorth

  case class GmosSouth[F[_]](
    observer:          Option[Observer],
    overrides:         SystemOverrides,
    targetEnvironment: TargetEnvironment,
    constraintSet:     ConstraintSet,
    staticCfg:         gmos.StaticConfig.GmosSouth,
    seq:               SequenceState[F],
    pendingObsCmd:     Option[PendingObserveCmd],
    visitStartDone:    Boolean
  ) extends SequenceData[F]:
    type D = gmos.DynamicConfig.GmosSouth
    val instrument: Instrument = Instrument.GmosSouth

  case class Flamingos2[F[_]](
    observer:          Option[Observer],
    overrides:         SystemOverrides,
    targetEnvironment: TargetEnvironment,
    constraintSet:     ConstraintSet,
    staticCfg:         Flamingos2StaticConfig,
    seq:               SequenceState[F],
    pendingObsCmd:     Option[PendingObserveCmd],
    visitStartDone:    Boolean
  ) extends SequenceData[F]:
    type D = Flamingos2DynamicConfig
    val instrument: Instrument = Instrument.Flamingos2

  case class Igrins2[F[_]](
    observer:          Option[Observer],
    overrides:         SystemOverrides,
    targetEnvironment: TargetEnvironment,
    constraintSet:     ConstraintSet,
    staticCfg:         Igrins2StaticConfig,
    seq:               SequenceState[F],
    pendingObsCmd:     Option[PendingObserveCmd],
    visitStartDone:    Boolean
  ) extends SequenceData[F]:
    type D = Igrins2DynamicConfig
    val instrument: Instrument = Instrument.Igrins2
