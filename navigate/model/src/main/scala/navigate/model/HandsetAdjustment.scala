// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import lucuma.core.enums.GuideProbe
import lucuma.core.math.Angle
import lucuma.core.math.Offset

sealed trait HandsetAdjustment

object HandsetAdjustment {
  case class HorizontalAdjustment(deltaAz: Angle, deltaEl: Angle)  extends HandsetAdjustment
  case class FocalPlaneAdjustment(value: FocalPlaneOffset)         extends HandsetAdjustment
  case class InstrumentAdjustment(value: Offset)                   extends HandsetAdjustment
  case class EquatorialAdjustment(deltaRA: Angle, deltaDec: Angle) extends HandsetAdjustment
  case class ProbeFrameAdjustment(
    probeRefFrame: GuideProbe,
    deltaX:        Angle,
    deltaY:        Angle,
    alignAngle:    Option[Angle]
  ) extends HandsetAdjustment

  given Show[HandsetAdjustment] = Show.show {
    case self @ HorizontalAdjustment(deltaAz, deltaEl)                          =>
      f"${self.getClass.getName}(deltaAz = ${Angle.signedDecimalArcseconds.get(deltaAz)}%.2f, deltaEl = ${Angle.signedDecimalArcseconds
          .get(deltaEl)}%.2f)}"
    case self @ FocalPlaneAdjustment(value)                                     =>
      f"${self.getClass.getName}(deltaX = ${Angle.signedDecimalArcseconds.get(value.deltaX.value)}%.2f, deltaY = ${Angle.signedDecimalArcseconds
          .get(value.deltaY.value)}%.2f)}"
    case self @ InstrumentAdjustment(value)                                     =>
      f"${self.getClass.getName}(q = ${Angle.signedDecimalArcseconds.get(value.q.toAngle)}%.2f, p = ${Angle.signedDecimalArcseconds
          .get(value.q.toAngle)}%.2f)}"
    case self @ EquatorialAdjustment(deltaRA, deltaDec)                         =>
      f"${self.getClass.getName}(deltaRA = ${Angle.signedDecimalArcseconds.get(deltaRA)}%.2f, deltaDec = ${Angle.signedDecimalArcseconds
          .get(deltaDec)}%.2f)}"
    case self @ ProbeFrameAdjustment(probeRefFrame, deltaX, deltaY, alignAngle) =>
      f"${self.getClass.getName}(probeRefFrame = $probeRefFrame, deltaX = ${Angle.signedDecimalArcseconds
          .get(deltaX)}%.2f, deltaY = ${Angle.signedDecimalArcseconds.get(deltaY)}%.2f${alignAngle.map(x => f", ${Angle.signedDecimalArcseconds.get(x)}%.2f").getOrElse("")})"
  }
}
