// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import cats.Eq
import cats.derived.*
import explore.model.ErrorMsgOr
import explore.model.boopickle.CatalogPicklers
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.BlindOffsetCandidate
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Tracking
import lucuma.schemas.model.CoordinatesAt
import org.typelevel.cats.time.given
import workers.WorkerRequest

import java.time.Duration
import java.time.Instant

object CatalogMessage extends CatalogPicklers {
  sealed trait Request   extends WorkerRequest derives Eq
  case object CleanCache extends Request {
    type ResponseType = Unit
  }

  case class GSRequest(
    tracking:    Tracking,
    vizTime:     Instant,
    obsModeType: ObservingModeType
  ) extends Request derives Eq {
    type ResponseType = List[GuideStarCandidate]
  }

  case class GSCacheCleanupRequest(elapsedTime: Duration) extends Request derives Eq {
    type ResponseType = Nothing
  }

  case class BlindOffsetRequest(
    baseCoordinatesAt: CoordinatesAt
  ) extends Request derives Eq {
    type ResponseType = ErrorMsgOr[List[BlindOffsetCandidate]]
  }

  private given Pickler[GSRequest] = generatePickler

  private given Pickler[GSCacheCleanupRequest] = generatePickler

  private given Pickler[BlindOffsetRequest] = generatePickler

  given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
}
