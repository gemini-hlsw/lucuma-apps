// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Attachment

trait SequenceTableMeta[D]:
  def editContexts: SequenceEditContexts[D]

  // The mask name for a GMOS custom mask attachment, when known.
  def maskName(attachmentId: Attachment.Id): Option[NonEmptyString] = None
