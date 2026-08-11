// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.ToBeDefined
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.Attachments

class CustomMasksSuite extends munit.FunSuite {

  private def attachmentId(l: Long): Attachment.Id = Attachment.Id.fromLong(l).get

  private def attachment(l: Long, maskName: Option[String]): Attachments =
    Attachments(attachmentId(l), maskName.map(NonEmptyString.unsafeFrom))

  private val masks: CustomMasks = CustomMasks.fromAttachments(
    List(
      attachment(1, "GN2026ASV051-10".some),
      attachment(2, none) // Not a MOS mask, so the ODB gives it no mask name
    )
  )

  test("the mask name is the one derived by the odb") {
    assertEquals(masks.maskName(Defined(attachmentId(1))), Right("GN2026ASV051-10"))
  }

  test("an attachment without a mask name is an error") {
    assert(masks.maskName(Defined(attachmentId(2))).isLeft)
  }

  test("an undefined mask is an error") {
    assert(masks.maskName(ToBeDefined).isLeft)
  }

  test("an unknown attachment is an error") {
    assert(masks.maskName(Defined(attachmentId(99))).isLeft)
  }
}
