// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence.byInstrument

import crystal.react.View
import japgolly.scalajs.react.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable
import observe.model.ExecutionState
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.SequenceTable
import observe.ui.components.sequence.SequenceTableBuilder
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode

case class GnirsSequenceTable(
  clientMode:           ClientMode,
  obsId:                Observation.Id,
  config:               ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig],
  acquisitonSN:         Option[SignalToNoiseAt],
  scienceSN:            Option[SignalToNoiseAt],
  visits:               View[Option[ExecutionVisits]],
  executionState:       ExecutionState,
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  selectedRowId:        Option[SelectedRowId],
  setSelectedRowId:     SelectedRowId => Callback,
  requests:             ObservationRequests,
  isPreview:            Boolean,
  onBreakpointFlip:     (Observation.Id, Step.Id) => Callback
) extends ReactFnProps(GnirsSequenceTable.component)
    with SequenceTable[GnirsStaticConfig, GnirsDynamicConfig](Instrument.Gnirs)
    with SpectroscopySequenceTable[GnirsDynamicConfig]:
  lazy val toInstrumentVisits =
    case ExecutionVisits.Gnirs(visits) => visits

  override def selectSNValue(seqType: SequenceType)(snAt: SignalToNoiseAt): SignalToNoise =
    seqType match // In acquisition there can me multiple coadds, so we report the total S/N
      case SequenceType.Acquisition => snAt.total.value
      case SequenceType.Science     => snAt.single.value

object GnirsSequenceTable
    extends SequenceTableBuilder[GnirsStaticConfig, GnirsDynamicConfig](Instrument.Gnirs)
