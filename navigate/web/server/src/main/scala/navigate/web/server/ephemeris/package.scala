// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server

import lucuma.core.model.Ephemeris

package object ephemeris {

  def ephemerisFileName(key: Ephemeris.Key): String = s"${key.toString}.eph"

}
