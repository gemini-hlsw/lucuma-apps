// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import LinearAngle.angle0µas
import LinearAngle.angle180µas
import LinearAngle.Angle270µas
import LinearAngle.Angle360µas
import LinearAngle.angle90µas
import LinearAngle.MinusAngle180µas
import LinearAngle.MinusAngle270µas

object AzimuthAngle extends LinearAngle[MinusAngle180µas, Angle360µas] {
  val Zero: Type      = AzimuthAngle.fromMicroarcseconds(angle0µas)
  val OneEighty: Type = AzimuthAngle.fromMicroarcseconds(angle180µas)
}
type AzimuthAngle = AzimuthAngle.Type

object RotatorAngle extends LinearAngle[MinusAngle270µas, Angle270µas] {
  val Ninety: Type = RotatorAngle.fromMicroarcseconds(angle90µas)
}
type RotatorAngle = RotatorAngle.Type
