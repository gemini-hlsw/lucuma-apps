// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import lucuma.odb.graphql.binding.*
import navigate.model.Distance

object DistanceInput:

  val Micrometers: Matcher[Distance] = LongBinding.map(Distance.fromLongMicrometers)
  val Millimeters: Matcher[Distance] = BigDecimalBinding.map(Distance.fromBigDecimalMillimeters)
  val Meters: Matcher[Distance]      = BigDecimalBinding.map(Distance.fromBigDecimalMeters)

  val Binding: Matcher[Distance] =
    OneOfBinding(
      "micrometers" -> Micrometers,
      "millimeters" -> Millimeters,
      "meters"      -> Meters
    )
