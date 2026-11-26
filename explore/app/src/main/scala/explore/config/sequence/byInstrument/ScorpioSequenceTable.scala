// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.effect.IO
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ItcResultValues
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SequenceColumnProfile
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class ScorpioSequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         ScorpioStaticConfig,
  science:              View[List[Atom[ScorpioDynamicConfig]]],
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  wavelengthUnits:      WavelengthUnits,
  columnProfile:        SequenceColumnProfile
) extends ReactFnProps(ScorpioSequenceTable.component)
    with SequenceTable[ScorpioStaticConfig, ScorpioDynamicConfig]
    with SpectroscopySequenceTable[ScorpioDynamicConfig]:

  // The ODB generates no SCORPIO acquisition, has no SCORPIO visits and can't replace its sequence.
  override val acquisition                             = View(List.empty, (_, _) => Callback.empty)
  override val acquisitionItc: Option[ItcResultValues] = None
  override val scienceItc                              = ItcResultValues.Empty
  override val isEditEnabled                           = IsEditEnabled.False

  override val remoteReplace: SequenceType => List[Atom[ScorpioDynamicConfig]] => IO[
    List[Atom[ScorpioDynamicConfig]]
  ] = _ =>
    _ => IO.raiseError(new UnsupportedOperationException("SCORPIO sequences can't be edited"))

  override val toInstrumentVisits = PartialFunction.empty

object ScorpioSequenceTable
    extends SequenceTableBuilder[ScorpioStaticConfig, ScorpioDynamicConfig](Instrument.Scorpio)
