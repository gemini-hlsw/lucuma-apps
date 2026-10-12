// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.util.NewType
import navigate.model.FocalPlaneOffset.DeltaX
import navigate.model.FocalPlaneOffset.DeltaY

case class FocalPlaneOffset(deltaX: DeltaX, deltaY: DeltaY) derives Eq

object FocalPlaneOffset {
  object DeltaX extends NewType[Angle]
  type DeltaX = DeltaX.Type
  object DeltaY extends NewType[Angle]
  type DeltaY = DeltaY.Type

  val Zero: FocalPlaneOffset = FocalPlaneOffset(DeltaX(Angle.Angle0), DeltaY(Angle.Angle0))

  // An Angle is stored in [0, 360°), so a negative value is a wrapped one that cannot be scaled by
  // sin/cos. The conversions use signed values, and go back to Angle only at the end.
  def fromOffset(off: Offset, iaa: Angle): FocalPlaneOffset = {
    val p   = off.p.toAngle.toSignedDoubleRadians
    val q   = off.q.toAngle.toSignedDoubleRadians
    val cos = iaa.cos
    val sin = iaa.sin
    FocalPlaneOffset(
      DeltaX(Angle.fromDoubleRadians(-p * cos - q * sin)),
      DeltaY(Angle.fromDoubleRadians(p * sin - q * cos))
    )
  }

  extension (a: FocalPlaneOffset) {
    def toOffset(iaa: Angle): Offset = {
      val x   = a.deltaX.value.toSignedDoubleRadians
      val y   = a.deltaY.value.toSignedDoubleRadians
      val cos = iaa.cos
      val sin = iaa.sin
      Offset(
        Offset.P(Angle.fromDoubleRadians(-x * cos + y * sin)),
        Offset.Q(Angle.fromDoubleRadians(-x * sin - y * cos))
      )
    }
  }

}
