// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.react.table.ColumnId
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SequenceColumns
import lucuma.ui.sequence.byInstrument.ImagingSequenceTable

final case class Flamingos2ImagingSequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         Flamingos2StaticConfig,
  acquisition:          View[List[Atom[Flamingos2DynamicConfig]]],
  science:              View[List[Atom[Flamingos2DynamicConfig]]],
  snPerFilter:          Map[Flamingos2Filter, SignalToNoiseAt],
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[Flamingos2DynamicConfig]] => IO[
    List[Atom[Flamingos2DynamicConfig]]
  ]
) extends ReactFnProps(Flamingos2ImagingSequenceTable.component)
    with SequenceTable[Flamingos2StaticConfig, Flamingos2DynamicConfig]
    with ImagingSequenceTable[Flamingos2DynamicConfig, Flamingos2Filter]:
  val toInstrumentVisits =
    case ExecutionVisits.Flamingos2(visits) => visits

  val filterFromDynamicConfig: Flamingos2DynamicConfig => Option[Flamingos2Filter] =
    _.filter.some

object Flamingos2ImagingSequenceTable
    extends SequenceTableBuilder[Flamingos2StaticConfig, Flamingos2DynamicConfig](
      Instrument.Flamingos2
    ):
  override protected val hiddenColumnIds: Set[ColumnId] =
    Set(SequenceColumns.GratingColumnId, SequenceColumns.FPUColumnId)
