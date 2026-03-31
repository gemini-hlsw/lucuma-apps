// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.ui.sequence.SequenceData

trait OdbSequenceApi[F[_]]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]]

  def replaceGmosNorthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosNorth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosNorth]]]

  def replaceGmosSouthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosSouth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosSouth]]]

  def replaceFlamingos2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Flamingos2DynamicConfig]]
  ): F[List[Atom[Flamingos2DynamicConfig]]]
