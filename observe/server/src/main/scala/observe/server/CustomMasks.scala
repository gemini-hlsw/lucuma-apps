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
 * instruments, which are the file names of the mask attachments without their extension.
 */
final case class CustomMasks(fileNames: Map[Attachment.Id, String]) {
  def maskName(mask: MaskDefinition): Either[ObserveFailure, String] = mask match {
    case ToBeDefined => ObserveFailure.Unexpected("The custom mask has not been defined yet").asLeft
    case Defined(id) =>
      fileNames
        .get(id)
        .toRight(ObserveFailure.Unexpected(s"Cannot find the custom mask attachment $id"))
  }
}

object CustomMasks {
  val Empty: CustomMasks = CustomMasks(Map.empty)

  def fromAttachments(attachments: List[Attachments]): CustomMasks = CustomMasks(
    attachments.map(a => a.id -> stripExtension(a.fileName.value)).toMap
  )

  private def stripExtension(fileName: String): String =
    fileName.lastIndexOf('.') match {
      case i if i > 0 => fileName.take(i)
      case _          => fileName
    }
}
