// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import cats.effect.IO
import crystal.react.View
import explore.config.sequence.SequenceTable
import explore.config.sequence.SequenceTableBuilder
import explore.model.AttachmentList
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Attachment
import lucuma.core.model.sequence.*
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ItcResultValues
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.byInstrument.SpectroscopySequenceTable

final case class GmosNorthSpectroscopySequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         gmos.StaticConfig.GmosNorth,
  acquisition:          View[List[Atom[gmos.DynamicConfig.GmosNorth]]],
  science:              View[List[Atom[gmos.DynamicConfig.GmosNorth]]],
  acquisitionItc:       ItcResultValues,
  scienceItc:           ItcResultValues,
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[gmos.DynamicConfig.GmosNorth]] => IO[
    List[Atom[gmos.DynamicConfig.GmosNorth]]
  ],
  attachments:          AttachmentList
) extends ReactFnProps(GmosNorthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosNorth]:
  val toInstrumentVisits =
    case ExecutionVisits.GmosNorth(visits) => visits

  override def maskName(attachmentId: Attachment.Id) =
    attachments.get(attachmentId).flatMap(_.maskName)

object GmosNorthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth](
      Instrument.GmosNorth
    )
