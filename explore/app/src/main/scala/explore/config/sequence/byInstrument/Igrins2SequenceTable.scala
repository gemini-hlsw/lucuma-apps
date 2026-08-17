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
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ItcResultValues
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class Igrins2SequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         Igrins2StaticConfig,
  acquisition:          View[List[Atom[Igrins2DynamicConfig]]],
  science:              View[List[Atom[Igrins2DynamicConfig]]],
  acquisitionItc:       ItcResultValues,
  scienceItc:           ItcResultValues,
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[Igrins2DynamicConfig]] => IO[
    List[Atom[Igrins2DynamicConfig]]
  ]
) extends ReactFnProps(Igrins2SequenceTable.component)
    with SequenceTable[Igrins2StaticConfig, Igrins2DynamicConfig]
    with SpectroscopySequenceTable[Igrins2DynamicConfig]:

  override val toInstrumentVisits =
    case ExecutionVisits.Igrins2(visits) => visits

object Igrins2SequenceTable
    extends SequenceTableBuilder[Igrins2StaticConfig, Igrins2DynamicConfig](Instrument.Igrins2)
