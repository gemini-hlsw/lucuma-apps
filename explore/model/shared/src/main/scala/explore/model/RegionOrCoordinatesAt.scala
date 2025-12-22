// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Region
import lucuma.schemas.model.CoordinatesAt

type RegionOrCoordinatesAt = Either[Region, CoordinatesAt]
