// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  def fromOffset(off: Offset, iaa: Angle): FocalPlaneOffset = FocalPlaneOffset(
    DeltaX(-off.p.toAngle * iaa.cos - off.q.toAngle * iaa.sin),
    DeltaY(off.p.toAngle * iaa.sin - off.q.toAngle * iaa.cos)
  )

  extension (a: FocalPlaneOffset) {
    def toOffset(iaa: Angle): Offset = Offset(
      Offset.P(-a.deltaX.value * iaa.cos + a.deltaY.value * iaa.sin),
      Offset.Q(-a.deltaX.value * iaa.sin - a.deltaY.value * iaa.cos)
    )
  }

}
