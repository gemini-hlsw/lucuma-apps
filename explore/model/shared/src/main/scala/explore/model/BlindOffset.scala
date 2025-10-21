// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.Target
import lucuma.schemas.model.enums.BlindOffsetType
import monocle.Focus
import monocle.Lens

final case class BlindOffset(
  useBlindOffset:      Boolean,
  blindOffsetTargetId: Option[Target.Id],
  blindOffsetType:     BlindOffsetType
) derives Eq

object BlindOffset:
  val useBlindOffset: Lens[BlindOffset, Boolean]                = Focus[BlindOffset](_.useBlindOffset)
  val blindOffsetTargetId: Lens[BlindOffset, Option[Target.Id]] =
    Focus[BlindOffset](_.blindOffsetTargetId)
  val blindOffsetType: Lens[BlindOffset, BlindOffsetType]       = Focus[BlindOffset](_.blindOffsetType)

  given Decoder[BlindOffset] = Decoder.instance: c =>
    for
      useBlindOffset      <- c.get[Boolean]("useBlindOffset")
      blindOffsetTargetId <-
        c.downField("blindOffsetTarget").downField("id").success.traverse(_.as[Option[Target.Id]])
      blindOffsetType     <- c.get[BlindOffsetType]("blindOffsetType")
    yield BlindOffset(useBlindOffset, blindOffsetTargetId.flatten, blindOffsetType)
