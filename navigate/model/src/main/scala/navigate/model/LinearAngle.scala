// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import lucuma.core.math.Angle

class LinearAngle[A <: Long & Singleton: ValueOf, B <: Long & Singleton: ValueOf] {

  type Type = LinearAngle.Value[A, B]

  val MinValue: Long = valueOf[A]
  val MaxValue: Long = valueOf[B]

  def fromMicroarcseconds(value: Long): Type = {
    val range = MaxValue - MinValue

    val v = ((((value - MinValue) % range) + range) % range) + MinValue

    LinearAngle.Value.apply[A, B](v)
  }

  def fromDoubleDegrees(ds: Double): Type =
    fromMicroarcseconds((ds * 60 * 60 * 1000 * 1000).toLong)

  def fromAngle(angle: Angle): Type = fromMicroarcseconds(angle.toMicroarcseconds)

  extension (angle: Type) {
    inline def toMicroarcseconds: Long = angle.µas

    def toDoubleDegrees: Double = toMicroarcseconds.toDouble / Angle.µasPerDegree.toDouble

  }

  given Show[Type] = Show.show(x => s"${x.toDoubleDegrees}º")

}

object LinearAngle {
  class Value[A <: Long & Singleton, B <: Long & Singleton] private (val µas: Long)

  object Value {
    def apply[A <: Long & Singleton: ValueOf, B <: Long & Singleton: ValueOf](
      value: Long
    ): LinearAngle.Value[A, B] = {
      val low  = valueOf[A]
      val high = valueOf[B]

      assert(low <= high, s"Invariant violated. Low limit is higher than high limit.")
      require(low <= value, s"Invariant violated. $value is lower that low limit.")
      require(value < high, s"Invariant violated. $value is higher that high limit.")
      new LinearAngle.Value[A, B](value)
    }
  }

  type Angle270µas      = 972_000_000_000L
  type MinusAngle270µas = -972_000_000_000L
  type MinusAngle180µas = -648_000_000_000L

}
