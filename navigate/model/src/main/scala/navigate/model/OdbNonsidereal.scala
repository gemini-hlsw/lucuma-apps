// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.EphemerisKeyType

case class OdbNonsidereal(
  des:     String,
  keyType: EphemerisKeyType,
  key:     String
) derives Eq,
      io.circe.Decoder
