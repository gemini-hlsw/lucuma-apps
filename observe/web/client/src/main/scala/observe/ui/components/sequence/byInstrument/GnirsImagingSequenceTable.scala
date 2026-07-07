// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence.byInstrument

import cats.syntax.option.*
import crystal.react.View
import japgolly.scalajs.react.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.itc.SignalToNoiseAt
import lucuma.react.common.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.byInstrument.ImagingSequenceTable
import observe.model.ExecutionState
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.SequenceTable
import observe.ui.components.sequence.SequenceTableBuilder
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode

final case class GnirsImagingSequenceTable(
  clientMode:           ClientMode,
  obsId:                Observation.Id,
  config:               ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig],
  snPerFilter:          Map[GnirsFilter, SignalToNoiseAt],
  visits:               View[Option[ExecutionVisits]],
  executionState:       ExecutionState,
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  selectedRowId:        Option[SelectedRowId],
  setSelectedRowId:     SelectedRowId => Callback,
  requests:             ObservationRequests,
  isPreview:            Boolean,
  onBreakpointFlip:     (Observation.Id, Step.Id) => Callback
) extends ReactFnProps(GnirsImagingSequenceTable.component)
    with SequenceTable[GnirsStaticConfig, GnirsDynamicConfig](Instrument.Gnirs)
    with ImagingSequenceTable[GnirsDynamicConfig, GnirsFilter]:
  lazy val toInstrumentVisits =
    case ExecutionVisits.Gnirs(visits) => visits

  val filterFromDynamicConfig: GnirsDynamicConfig => Option[GnirsFilter] =
    _.filter.some

object GnirsImagingSequenceTable
    extends SequenceTableBuilder[GnirsStaticConfig, GnirsDynamicConfig](Instrument.Gnirs)
