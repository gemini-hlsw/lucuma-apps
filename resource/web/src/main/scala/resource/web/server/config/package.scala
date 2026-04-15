// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.web.server.config

import cats.effect.Sync
import pureconfig.ConfigObjectSource
import pureconfig.module.catseffect.syntax.*
import resource.model.config.ResourceConfiguration

def loadConfiguration[F[_]: Sync](config: ConfigObjectSource): F[ResourceConfiguration] =
  config.loadF[F, ResourceConfiguration]()
