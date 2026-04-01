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
import lucuma.ui.sequence.EditingSequenceTypes
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable
import cats.effect.IO

final case class GmosSouthSpectroscopySequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         gmos.StaticConfig.GmosSouth,
  acquisition:          Option[Atom[gmos.DynamicConfig.GmosSouth]],
  science:              Option[List[Atom[gmos.DynamicConfig.GmosSouth]]],
  acquisitonSN:         Option[SignalToNoiseAt],
  scienceSN:            Option[SignalToNoiseAt],
  editingSequenceTypes: View[EditingSequenceTypes],
  modAcquisition:       Endo[Option[Atom[gmos.DynamicConfig.GmosSouth]]] => Callback,
  modScience:           Endo[List[Atom[gmos.DynamicConfig.GmosSouth]]] => Callback,
  isUserStaffOrAdmin:   Boolean,
  isEditable:           Boolean,
  isEditInFlight:       Boolean,
  onEditAccept:         IO[Unit],
  onEditCancel:         Callback
) extends ReactFnProps(GmosSouthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosSouth]:
  val toInstrumentVisits =
    case ExecutionVisits.GmosSouth(visits) => visits

object GmosSouthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
