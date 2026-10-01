// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.ags.AgsAnalysis
import lucuma.core.geom.ShapePolygon
import lucuma.core.math.Angle

/**
 * An AGS worker answer. The patrol-field intersection per tested position angle comes already
 * evaluated so the UI draws it without redoing the geometry on the main thread.
 */
case class AgsResult(
  usable:       List[AgsAnalysis.Usable],
  patrolFields: Map[Angle, List[ShapePolygon]]
) derives Eq
