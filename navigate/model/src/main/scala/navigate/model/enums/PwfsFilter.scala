// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

enum PwfsFilter(val tag: String, val wavel: Wavelength) derives Enumerated {
  case Neutral extends PwfsFilter("Neutral", Wavelength.unsafeFromIntPicometers(650_000))
  case Blue    extends PwfsFilter("Blue", Wavelength.unsafeFromIntPicometers(450_000))
  case Green   extends PwfsFilter("Green", Wavelength.unsafeFromIntPicometers(550_000))
  case Red     extends PwfsFilter("Red", Wavelength.unsafeFromIntPicometers(600_000))
  case Red04   extends PwfsFilter("Red04", Wavelength.unsafeFromIntPicometers(640_000))
  case Red1    extends PwfsFilter("Red1", Wavelength.unsafeFromIntPicometers(700_000))
}
