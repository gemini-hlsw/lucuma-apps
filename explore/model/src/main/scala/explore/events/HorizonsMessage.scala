// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import explore.model.ErrorMsgOr
import explore.model.boopickle.HorizonsPicklers
import lucuma.core.enums.Site
import lucuma.core.model.Ephemeris
import lucuma.core.model.EphemerisTracking
import lucuma.horizons.HorizonsClient.ElementsPerDay
import workers.WorkerRequest

import java.time.Instant

object HorizonsMessage extends HorizonsPicklers:
  sealed trait Request extends WorkerRequest

  case object CleanCache extends Request:
    type ResponseType = Unit

  case class EphemerisRequest(
    key:      Ephemeris.Key.Horizons,
    site:     Site,
    start:    Instant,
    stop:     Instant,
    elements: Int
  ) extends Request:
    type ResponseType = ErrorMsgOr[EphemerisTracking]

  case class AlignedEphemerisRequest(
    key:     Ephemeris.Key.Horizons,
    site:    Site,
    start:   Instant,
    days:    Int,
    cadence: ElementsPerDay
  ) extends Request:
    type ResponseType = ErrorMsgOr[EphemerisTracking]

  given Pickler[EphemerisRequest] = generatePickler

  given Pickler[AlignedEphemerisRequest] = generatePickler

  given Pickler[CleanCache.type] = generatePickler

  given Pickler[Request] = generatePickler
