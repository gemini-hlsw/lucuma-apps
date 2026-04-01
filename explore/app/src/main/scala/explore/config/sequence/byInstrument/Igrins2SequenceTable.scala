// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.EditingSequenceTypes
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable
import cats.effect.IO

final case class Igrins2SequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         Igrins2StaticConfig,
  science:              Option[List[Atom[Igrins2DynamicConfig]]],
  scienceSN:            Option[SignalToNoiseAt],
  editingSequenceTypes: View[EditingSequenceTypes],
  isUserStaffOrAdmin:   Boolean,
  isEditable:           Boolean,
  isEditInFlight:       Boolean,
  onEditAccept:         IO[Unit],
  onEditCancel:         Callback
) extends ReactFnProps(Igrins2SequenceTable.component)
    with SequenceTable[Igrins2StaticConfig, Igrins2DynamicConfig]
    with SpectroscopySequenceTable[Igrins2DynamicConfig]:

  // No acquition for igrins 2
  override val acquisition = None

  override val acquisitonSN = None

  override val modAcquisition = _ => Callback.empty

  // No edit support yet
  override val modScience = _ => Callback.empty

  override val toInstrumentVisits =
    case ExecutionVisits.Igrins2(visits) => visits

object Igrins2SequenceTable
    extends SequenceTableBuilder[Igrins2StaticConfig, Igrins2DynamicConfig](
      Instrument.Igrins2
    )
