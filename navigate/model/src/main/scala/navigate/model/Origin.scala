// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import lucuma.core.math.Angle

case class Origin(
  x: Angle,
  y: Angle
)

object Origin {
  given Show[Origin] = Show.show { o =>
    f"Origin(x: ${Angle.signedDecimalArcseconds.get(o.x).toDouble}%.2f, y: ${Angle.signedDecimalArcseconds.get(o.y).toDouble}%.2f)"
  }
}
