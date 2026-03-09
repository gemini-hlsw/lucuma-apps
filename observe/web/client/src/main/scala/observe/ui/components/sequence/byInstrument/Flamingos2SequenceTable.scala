// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence.byInstrument

import crystal.react.View
import japgolly.scalajs.react.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable
import observe.model.ExecutionState
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.SequenceTable
import observe.ui.components.sequence.SequenceTableBuilder
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode

final case class Flamingos2SequenceTable(
  clientMode:           ClientMode,
  obsId:                Observation.Id,
  config:               ExecutionConfig.Flamingos2,
  acquisitonSN:         Option[SignalToNoiseAt],
  scienceSN:            Option[SignalToNoiseAt],
  visits:               View[Option[ExecutionVisits]],
  executionState:       ExecutionState,
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  selectedStepId:       Option[Step.Id],
  setSelectedStepId:    Step.Id => Callback,
  requests:             ObservationRequests,
  isPreview:            Boolean,
  onBreakpointFlip:     (Observation.Id, Step.Id) => Callback
) extends ReactFnProps(Flamingos2SequenceTable.component)
    with SequenceTable[Flamingos2StaticConfig, Flamingos2DynamicConfig](Instrument.Flamingos2)
    with SpectroscopySequenceTable[Flamingos2DynamicConfig]:
  val toInstrumentVisits =
    case ExecutionVisits.Flamingos2(visits) => visits

object Flamingos2SequenceTable
    extends SequenceTableBuilder[Flamingos2StaticConfig, Flamingos2DynamicConfig](
      Instrument.Flamingos2
    )
