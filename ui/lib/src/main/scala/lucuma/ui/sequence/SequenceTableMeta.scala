// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.model.Attachment

trait SequenceTableMeta[D]:
  def editContexts: SequenceEditContexts[D]

  def maskName(attachmentId: Attachment.Id): Option[NonEmptyString] = None

  def wavelengthFormat: SequenceWavelengthFormat = SequenceWavelengthFormat.Nanometers

  /** When defined, the visit and run headers offer setting a QA state on their datasets. */
  def datasetQaBulkEdit: Option[DatasetQaBulkEdit] = None

  /** The resolution mode of a GHOST sequence, which is the same for all its steps. */
  def ghostResolutionMode: Option[GhostResolutionMode] = None

  /** The well depth of a GNIRS sequence, which is the same for all its steps. */
  def gnirsWellDepth: Option[GnirsWellDepth] = None
