// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.ToBeDefined
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.Attachments

class CustomMasksSuite extends munit.FunSuite {

  private def attachmentId(l: Long): Attachment.Id = Attachment.Id.fromLong(l).get

  private def attachment(l: Long, fileName: String): Attachments =
    Attachments(attachmentId(l), NonEmptyString.unsafeFrom(fileName))

  private val masks: CustomMasks = CustomMasks.fromAttachments(
    List(
      attachment(1, "GN2026AQ001-01.fits"),
      attachment(2, "GS2026BQ042-03"),
      attachment(3, "odd.name.with.dots.fits"),
      attachment(4, ".hidden")
    )
  )

  test("the mask name is the attachment file name without its extension") {
    assertEquals(masks.maskName(Defined(attachmentId(1))), Right("GN2026AQ001-01"))
  }

  test("a file name without extension is used as is") {
    assertEquals(masks.maskName(Defined(attachmentId(2))), Right("GS2026BQ042-03"))
  }

  test("only the last extension is dropped") {
    assertEquals(masks.maskName(Defined(attachmentId(3))), Right("odd.name.with.dots"))
  }

  test("a leading dot is not an extension") {
    assertEquals(masks.maskName(Defined(attachmentId(4))), Right(".hidden"))
  }

  test("an undefined mask is an error") {
    assert(masks.maskName(ToBeDefined).isLeft)
  }

  test("an unknown attachment is an error") {
    assert(masks.maskName(Defined(attachmentId(99))).isLeft)
  }
}
