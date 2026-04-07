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
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class Flamingos2SequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         Flamingos2StaticConfig,
  acquisition:          View[List[Atom[Flamingos2DynamicConfig]]],
  science:              View[List[Atom[Flamingos2DynamicConfig]]],
  acquisitonSN:         Option[SignalToNoiseAt],
  scienceSN:            Option[SignalToNoiseAt],
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[Flamingos2DynamicConfig]] => IO[
    List[Atom[Flamingos2DynamicConfig]]
  ]
) extends ReactFnProps(Flamingos2SequenceTable.component)
    with SequenceTable[Flamingos2StaticConfig, Flamingos2DynamicConfig]
    with SpectroscopySequenceTable[Flamingos2DynamicConfig]:
  val toInstrumentVisits =
    case ExecutionVisits.Flamingos2(visits) => visits

object Flamingos2SequenceTable
    extends SequenceTableBuilder[Flamingos2StaticConfig, Flamingos2DynamicConfig](
      Instrument.Flamingos2
    )
