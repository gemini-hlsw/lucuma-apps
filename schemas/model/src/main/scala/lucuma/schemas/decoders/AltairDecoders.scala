// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import io.circe.Decoder
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.odb.data.AltairConfiguration

trait AltairDecoders:
  // `defaultFieldLens` and `fieldLens` are ignored: they depend on the guide star, which Explore
  // resolves through its own AGS selection.
  given Decoder[AltairConfiguration] = Decoder.instance: c =>
    for
      mode              <- c.get[AltairMode]("mode")
      explicitFieldLens <- c.get[Option[FieldLens]]("explicitFieldLens")
      cassRotator       <- c.get[CassRotator]("cassRotator")
      ndFilter          <- c.get[AltairNdFilter]("ndFilter")
    yield AltairConfiguration(mode, explicitFieldLens, cassRotator, ndFilter)
