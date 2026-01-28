// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import cats.data.NonEmptyList
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle.CommonPicklers.picklerNewType
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuideStarCandidate
import lucuma.ags.ScienceOffsets
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import lucuma.schemas.model.AGSWavelength
import workers.WorkerRequest

import java.time.Instant

object AgsMessage {
  sealed trait Request extends WorkerRequest

  case object CleanCache extends Request {
    type ResponseType = Unit
  }

  case class AgsRequest(
    id:                 Target.Id,
    vizTime:            Instant,
    constraints:        ConstraintSet,
    wavelength:         AGSWavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acqOffsets:         Option[AcquisitionOffsets],
    sciOffsets:         Option[ScienceOffsets],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  ) extends Request {
    type ResponseType = List[AgsAnalysis.Usable]
  }

  private given Pickler[CleanCache.type] = generatePickler

  given Pickler[AGSWavelength] = picklerNewType(AGSWavelength)

  given Pickler[AgsRequest] = generatePickler

  given Pickler[Request] = generatePickler
}
