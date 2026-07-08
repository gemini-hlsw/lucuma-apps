// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.accepted.Millimeter
import coulomb.units.si.Meter
import lucuma.core.math.units.Micrometer

opaque type Distance = Quantity[Long, Micrometer]

object Distance {

  lazy val Min: Distance = fromLongMicrometers(Long.MinValue)

  lazy val Zero: Distance = fromLongMicrometers(0)

  def fromLongMicrometers(i: Long): Distance = i.withUnit[Micrometer]

  def fromBigDecimalMillimeter(bigDecimal: BigDecimal): Distance = fromLongMicrometers(
    (bigDecimal * 1000).toLong
  )

  def fromBigDecimalMeter(bigDecimal: BigDecimal): Distance = fromLongMicrometers(
    (bigDecimal * 1000000).toLong
  )

  extension (d: Distance) {

    private def to[U](scale: Int): Quantity[BigDecimal, U] =
      BigDecimal(toMicrometers.value, scale).withUnit[U]

    def toMicrometers: Quantity[Long, Micrometer] = d
    def µm: Quantity[Long, Micrometer]            = toMicrometers

    def toMillimeters: Quantity[BigDecimal, Millimeter] = to[Millimeter](3)
    def toMeter: Quantity[BigDecimal, Meter]            = to[Meter](6)

  }

}
