// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence.byInstrument

import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos
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

final case class GmosSouthSpectroscopySequenceTable(
  clientMode:           ClientMode,
  obsId:                Observation.Id,
  config:               ExecutionConfig.GmosSouth,
  acquisitionSN:        Option[SignalToNoiseAt],
  scienceSN:            Option[SignalToNoiseAt],
  visits:               View[Option[ExecutionVisits]],
  executionState:       ExecutionState,
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  selectedRowId:        Option[SelectedRowId],
  setSelectedRowId:     SelectedRowId => Callback,
  requests:             ObservationRequests,
  isPreview:            Boolean,
  onBreakpointFlip:     (Observation.Id, Step.Id) => Callback,
  getMaskName:          Attachment.Id => Option[NonEmptyString]
) extends ReactFnProps(GmosSouthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosSouth]:
  lazy val toInstrumentVisits =
    case ExecutionVisits.GmosSouth(visits) => visits

  override def maskName(attachmentId: Attachment.Id): Option[NonEmptyString] =
    getMaskName(attachmentId)

object GmosSouthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
