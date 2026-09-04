// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.config

import cats.Eq
import cats.derived.*
import org.http4s.Uri

final case class OtelConfiguration(
  endpoint:       Option[Uri],
  key:            Option[String],
  clientEndpoint: Option[Uri]
) derives Eq:
  // if client endpoint is not defined use the server endpoint
  val effectiveClientEndpoint: Option[Uri] =
    clientEndpoint.orElse(endpoint.map(_ / "v1" / "traces"))
