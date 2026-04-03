// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.effect.IO
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Enums.GmosSouthFilter
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.ImagingSequenceTable

final case class GmosSouthImagingSequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         gmos.StaticConfig.GmosSouth,
  acquisition:          View[List[Atom[gmos.DynamicConfig.GmosSouth]]],
  science:              View[List[Atom[gmos.DynamicConfig.GmosSouth]]],
  snPerFilter:          Map[GmosSouthFilter, SignalToNoiseAt],
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[gmos.DynamicConfig.GmosSouth]] => IO[
    List[Atom[gmos.DynamicConfig.GmosSouth]]
  ]
) extends ReactFnProps(GmosSouthImagingSequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
    with ImagingSequenceTable[gmos.DynamicConfig.GmosSouth, GmosSouthFilter]:
  val toInstrumentVisits =
    case ExecutionVisits.GmosSouth(visits) => visits

  val filterFromDynamicConfig: gmos.DynamicConfig.GmosSouth => Option[GmosSouthFilter] =
    _.filter

object GmosSouthImagingSequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
