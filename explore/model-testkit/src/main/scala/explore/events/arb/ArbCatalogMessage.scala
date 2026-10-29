// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events.arb

import cats.syntax.all.*
import explore.events.CatalogMessage
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.Instrument
import lucuma.core.model.Tracking
import lucuma.core.model.arb.ArbTracking.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.schemas.model.CoordinatesAt
import lucuma.schemas.model.arb.ArbCoordinatesAt.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Duration
import java.time.Instant

trait ArbCatalogMessage:
  given Arbitrary[CatalogMessage.GSRequest] =
    Arbitrary:
      for
        tracking   <- arbitrary[Tracking]
        vizTime    <- arbitrary[Instant]
        guideProbe <- arbitrary[GuideProbe]
      yield CatalogMessage.GSRequest(tracking, vizTime, guideProbe)

  given Cogen[CatalogMessage.GSRequest] =
    Cogen[(Tracking, Instant, GuideProbe)].contramap(r => (r.tracking, r.vizTime, r.guideProbe))

  given Arbitrary[CatalogMessage.GSByIdRequest] =
    Arbitrary(arbitrary[Long].map(CatalogMessage.GSByIdRequest(_)))

  given Cogen[CatalogMessage.GSByIdRequest] =
    Cogen[Long].contramap(_.sourceId)

  given Arbitrary[CatalogMessage.GSCacheCleanupRequest] =
    // for some reason arbitrary[Duration] caused Long overflows, so...
    Arbitrary:
      Gen.choose(1L, 1000L).map(s => CatalogMessage.GSCacheCleanupRequest(Duration.ofSeconds(s)))

  given Cogen[CatalogMessage.GSCacheCleanupRequest] =
    Cogen[Duration].contramap(r => r.elapsedTime)

  given Arbitrary[CatalogMessage.BlindOffsetRequest] =
    Arbitrary:
      for
        coordsAt   <- arbitrary[CoordinatesAt]
        instrument <- arbitrary[Instrument]
      yield CatalogMessage.BlindOffsetRequest(coordsAt, instrument)

  given Cogen[CatalogMessage.BlindOffsetRequest] =
    Cogen[(CoordinatesAt, Instrument)].contramap(r => (r.baseCoordinatesAt, r.instrument))

  given Arbitrary[CatalogMessage.Request] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[CatalogMessage.GSRequest],
        arbitrary[CatalogMessage.GSByIdRequest],
        arbitrary[CatalogMessage.GSCacheCleanupRequest],
        arbitrary[CatalogMessage.BlindOffsetRequest],
        Gen.const(CatalogMessage.CleanCache)
      )

  given Cogen[CatalogMessage.Request] =
    Cogen[Either[
      Unit,
      Either[CatalogMessage.GSCacheCleanupRequest, Either[
        CatalogMessage.GSRequest,
        Either[CatalogMessage.GSByIdRequest, CatalogMessage.BlindOffsetRequest]
      ]]
    ]]
      .contramap {
        case CatalogMessage.CleanCache               => ().asLeft
        case r: CatalogMessage.GSCacheCleanupRequest => r.asLeft.asRight
        case r: CatalogMessage.GSRequest             => r.asLeft.asRight.asRight
        case r: CatalogMessage.GSByIdRequest         => r.asLeft.asRight.asRight.asRight
        case r: CatalogMessage.BlindOffsetRequest    => r.asRight.asRight.asRight.asRight
      }

object ArbCatalogMessage extends ArbCatalogMessage
