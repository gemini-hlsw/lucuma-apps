// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.Endo
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GmosNorthSpectroscopySequenceTable(
  visits:             View[Option[ExecutionVisits]],
  staticConfig:       gmos.StaticConfig.GmosNorth,
  acquisition:        Option[Atom[gmos.DynamicConfig.GmosNorth]],
  science:            Option[List[Atom[gmos.DynamicConfig.GmosNorth]]],
  acquisitonSN:       Option[SignalToNoiseAt],
  scienceSN:          Option[SignalToNoiseAt],
  isEditing:          IsEditing,
  modAcquisition:     Endo[Option[Atom[gmos.DynamicConfig.GmosNorth]]] => Callback,
  modScience:         Endo[List[Atom[gmos.DynamicConfig.GmosNorth]]] => Callback,
  isUserStaffOrAdmin: Boolean
) extends ReactFnProps(GmosNorthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosNorth]:
  val toInstrumentVisits =
    case ExecutionVisits.GmosNorth(visits) => visits

object GmosNorthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
      Instrument.GmosNorth
    )
