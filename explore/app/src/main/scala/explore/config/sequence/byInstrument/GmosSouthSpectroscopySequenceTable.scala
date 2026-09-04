// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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

final case class GmosSouthSpectroscopySequenceTable(
  visits:               View[Option[ExecutionVisits]],
  staticConfig:         gmos.StaticConfig.GmosSouth,
  acquisition:          View[List[Atom[gmos.DynamicConfig.GmosSouth]]],
  science:              View[List[Atom[gmos.DynamicConfig.GmosSouth]]],
  acquisitionItc:       ItcResultValues,
  scienceItc:           ItcResultValues,
  isEditEnabled:        IsEditEnabled,
  isEditingAcquisition: View[IsEditing],
  isEditingScience:     View[IsEditing],
  isUserStaffOrAdmin:   Boolean,
  remoteReplace:        SequenceType => List[Atom[gmos.DynamicConfig.GmosSouth]] => IO[
    List[Atom[gmos.DynamicConfig.GmosSouth]]
  ],
  attachments:          AttachmentList
) extends ReactFnProps(GmosSouthSpectroscopySequenceTable.component)
    with SequenceTable[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
    with SpectroscopySequenceTable[gmos.DynamicConfig.GmosSouth]:
  val toInstrumentVisits =
    case ExecutionVisits.GmosSouth(visits) => visits

  override def maskName(attachmentId: Attachment.Id) =
    attachments.get(attachmentId).flatMap(_.maskName)

object GmosSouthSpectroscopySequenceTable
    extends SequenceTableBuilder[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth](
      Instrument.GmosSouth
    )
