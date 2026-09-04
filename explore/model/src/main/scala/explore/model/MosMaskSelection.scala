// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Instrument

object MosMaskSelection:
  /** Masks offered for the given instrument. */
  def selectable(attachments: AttachmentList, instrument: Instrument): List[Attachment] =
    attachments.values.toList
      .filter: a =>
        a.attachmentType === AttachmentType.MosMask && a.maskInstrument.exists(_ === instrument)
      .sortBy(_.displayName.value)

  /**
   * Options shown by the picker: the selectable masks, plus the bound attachment.
   */
  def shown(
    attachments: AttachmentList,
    instrument:  Instrument,
    bound:       Option[Attachment.Id]
  ): List[Attachment] =
    val instrumentMasks = selectable(attachments, instrument)
    bound
      .flatMap(attachments.get)
      .filterNot(a => instrumentMasks.exists(_.id === a.id))
      .fold(instrumentMasks)(a => (a :: instrumentMasks).sortBy(_.displayName.value))
