// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.*
import org.http4s.Uri
import org.http4s.circe.*

case class TracingConfig(
  endpoint:    Uri,
  serviceName: String,
  headers:     Map[String, String] = Map.empty
) derives Eq,
      Decoder
