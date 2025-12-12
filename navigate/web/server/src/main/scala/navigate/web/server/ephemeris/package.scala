// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server

import lucuma.core.model.EphemerisKey

package object ephemeris {

  def ephemerisFileName(key: EphemerisKey): String = s"${key.toString}.eph"

}
