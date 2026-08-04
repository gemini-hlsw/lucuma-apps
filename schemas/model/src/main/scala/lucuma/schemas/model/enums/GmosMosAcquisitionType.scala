// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.enums

import lucuma.core.util.Enumerated

/**
 * Whether the acquisition image is taken with the MOS mask in or out of the light path.
 */
enum GmosMosAcquisitionType(val tag: String, val shortName: String, val longName: String)
    derives Enumerated:
  case MaskIn  extends GmosMosAcquisitionType("MASK_IN", "In", "Mask In")
  case MaskOut extends GmosMosAcquisitionType("MASK_OUT", "Out", "Mask Out")
