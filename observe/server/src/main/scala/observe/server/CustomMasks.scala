// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.syntax.all.*
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.Attachments

/**
 * Resolves the custom mask (MOS) definitions of a sequence to the mask names understood by the
 * instruments. The ODB derives the mask name from the attachment itself, and only MOS mask
 * attachments have one.
 */
final case class CustomMasks(maskNames: Map[Attachment.Id, String]) {
  def maskName(mask: MaskDefinition): Either[ObserveFailure, String] = mask match {
    case ToBeDefined => ObserveFailure.Unexpected("The custom mask has not been defined yet").asLeft
    case Defined(id) =>
      maskNames
        .get(id)
        .toRight(ObserveFailure.Unexpected(s"Cannot find the mask name of attachment $id"))
  }
}

object CustomMasks {
  val Empty: CustomMasks = CustomMasks(Map.empty)

  def fromAttachments(attachments: List[Attachments]): CustomMasks = CustomMasks(
    attachments.flatMap(a => a.mask.name.map(n => a.id -> n.value)).toMap
  )
}
