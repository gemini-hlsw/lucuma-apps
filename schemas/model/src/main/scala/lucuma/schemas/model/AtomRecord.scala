// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimestampInterval
import lucuma.schemas.model.enums.AtomExecutionState
import monocle.Focus
import monocle.Lens

enum AtomRecord[+D]:
  def id: Atom.Id
  def executionState: AtomExecutionState
  def interval: Option[TimestampInterval]
  def sequenceType: SequenceType
  def steps: List[StepRecord[D]]

  case GmosNorth protected[schemas] (
    id:             Atom.Id,
    executionState: AtomExecutionState,
    interval:       Option[TimestampInterval],
    sequenceType:   SequenceType,
    steps:          List[StepRecord.GmosNorth]
  ) extends AtomRecord[gmos.DynamicConfig.GmosNorth]

  case GmosSouth protected[schemas] (
    id:             Atom.Id,
    executionState: AtomExecutionState,
    interval:       Option[TimestampInterval],
    sequenceType:   SequenceType,
    steps:          List[StepRecord.GmosSouth]
  ) extends AtomRecord[gmos.DynamicConfig.GmosSouth]

  case Flamingos2 protected[schemas] (
    id:             Atom.Id,
    executionState: AtomExecutionState,
    interval:       Option[TimestampInterval],
    sequenceType:   SequenceType,
    steps:          List[StepRecord.Flamingos2]
  ) extends AtomRecord[Flamingos2DynamicConfig]

  case Igrins2 protected[schemas] (
    id:             Atom.Id,
    executionState: AtomExecutionState,
    interval:       Option[TimestampInterval],
    sequenceType:   SequenceType,
    steps:          List[StepRecord.Igrins2]
  ) extends AtomRecord[Igrins2DynamicConfig]

  case Ghost protected[schemas] (
    id:             Atom.Id,
    executionState: AtomExecutionState,
    interval:       Option[TimestampInterval],
    sequenceType:   SequenceType,
    steps:          List[StepRecord.Ghost]
  ) extends AtomRecord[GhostDynamicConfig]

object AtomRecord:
  given [A]: Eq[AtomRecord[A]] = Eq.derived

  object GmosNorth:
    given Eq[GmosNorth] = Eq.derived

    val id: Lens[GmosNorth, Atom.Id] =
      Focus[GmosNorth](_.id)

    val executionState: Lens[GmosNorth, AtomExecutionState] =
      Focus[GmosNorth](_.executionState)

    val interval: Lens[GmosNorth, Option[TimestampInterval]] =
      Focus[GmosNorth](_.interval)

    val sequenceType: Lens[GmosNorth, SequenceType] =
      Focus[GmosNorth](_.sequenceType)

    val steps: Lens[GmosNorth, List[StepRecord.GmosNorth]] =
      Focus[GmosNorth](_.steps)

  object GmosSouth:
    given Eq[GmosSouth] = Eq.derived

    val id: Lens[GmosSouth, Atom.Id] =
      Focus[GmosSouth](_.id)

    val interval: Lens[GmosSouth, Option[TimestampInterval]] =
      Focus[GmosSouth](_.interval)

    val sequenceType: Lens[GmosSouth, SequenceType] =
      Focus[GmosSouth](_.sequenceType)

    val steps: Lens[GmosSouth, List[StepRecord.GmosSouth]] =
      Focus[GmosSouth](_.steps)

  object Flamingos2:
    given Eq[Flamingos2] = Eq.derived

    val id: Lens[Flamingos2, Atom.Id] =
      Focus[Flamingos2](_.id)

    val executionState: Lens[Flamingos2, AtomExecutionState] =
      Focus[Flamingos2](_.executionState)

    val interval: Lens[Flamingos2, Option[TimestampInterval]] =
      Focus[Flamingos2](_.interval)

    val sequenceType: Lens[Flamingos2, SequenceType] =
      Focus[Flamingos2](_.sequenceType)

    val steps: Lens[Flamingos2, List[StepRecord.Flamingos2]] =
      Focus[Flamingos2](_.steps)

  object Igrins2:
    given Eq[Igrins2] = Eq.derived

    val id: Lens[Igrins2, Atom.Id] =
      Focus[Igrins2](_.id)

    val executionState: Lens[Igrins2, AtomExecutionState] =
      Focus[Igrins2](_.executionState)

    val interval: Lens[Igrins2, Option[TimestampInterval]] =
      Focus[Igrins2](_.interval)

    val sequenceType: Lens[Igrins2, SequenceType] =
      Focus[Igrins2](_.sequenceType)

    val steps: Lens[Igrins2, List[StepRecord.Igrins2]] =
      Focus[Igrins2](_.steps)

  object Ghost:
    given Eq[Ghost] = Eq.derived

    val id: Lens[Ghost, Atom.Id] =
      Focus[Ghost](_.id)

    val executionState: Lens[Ghost, AtomExecutionState] =
      Focus[Ghost](_.executionState)

    val interval: Lens[Ghost, Option[TimestampInterval]] =
      Focus[Ghost](_.interval)

    val sequenceType: Lens[Ghost, SequenceType] =
      Focus[Ghost](_.sequenceType)

    val steps: Lens[Ghost, List[StepRecord.Ghost]] =
      Focus[Ghost](_.steps)
