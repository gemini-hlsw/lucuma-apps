// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import lucuma.core.util.Enumerated

enum PwfsFieldStop(val tag: String) derives Enumerated {
  case Prism extends PwfsFieldStop("Prism")
  case Fs10  extends PwfsFieldStop("Fs10")
  case Fs6_4 extends PwfsFieldStop("Fs6_4")
  case Fs3_2 extends PwfsFieldStop("Fs3_2")
  case Fs1_6 extends PwfsFieldStop("Fs1_6")
  case Open1 extends PwfsFieldStop("open1")
  case Open2 extends PwfsFieldStop("open2")
  case Open3 extends PwfsFieldStop("open3")
  case Open4 extends PwfsFieldStop("open4")
}
