// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.enums

import lucuma.core.util.Enumerated

sealed abstract class ImagingCapabilities(val tag: String, val label: String)
    extends Product
    with Serializable

object ImagingCapabilities {
  case object Speckle   extends ImagingCapabilities("speckle", "Speckle")
  case object WideField extends ImagingCapabilities("wide_field", "Wide Field")

  given Enumerated[ImagingCapabilities] =
    Enumerated.from(Speckle, WideField).withTag(_.tag)
}
