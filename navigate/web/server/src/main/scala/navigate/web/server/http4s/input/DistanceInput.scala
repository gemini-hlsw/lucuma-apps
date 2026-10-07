// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s.input

import cats.syntax.all.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.oneOrFail
import navigate.model.Distance

object DistanceInput:

  val Micrometers: Matcher[Distance] = LongBinding.map(Distance.fromLongMicrometers)
  val Millimeters: Matcher[Distance] = BigDecimalBinding.map(Distance.fromBigDecimalMillimeters)
  val Meters: Matcher[Distance]      = BigDecimalBinding.map(Distance.fromBigDecimalMeters)

  val Binding: Matcher[Distance] =
    ObjectFieldsBinding.rmap:
      case List(
            Micrometers.Option("micrometers", rMicrometers),
            Millimeters.Option("millimeters", rMillimeters),
            Meters.Option("meters", rMeters)
          ) =>
        (rMicrometers, rMillimeters, rMeters).parTupled.flatMap:
          (micrometers, millimeters, meters) =>
            oneOrFail(
              micrometers -> "micrometers",
              millimeters -> "millimeters",
              meters      -> "meters"
            )
