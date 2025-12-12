// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers
package horizons

import boopickle.DefaultBasic.*
import boopickle.Pickler
import cats.*
import cats.effect.Concurrent
import cats.syntax.all.*
import explore.events.HorizonsMessage
import explore.events.HorizonsMessage.given
import lucuma.horizons.HorizonsClient
import lucuma.horizons.HorizonsEphemeris

object HorizonsRequests:
  val cacheVersion = CacheVersion(2)

  private case class Error(message: String, isCacheable: Boolean)

  private given Pickler[Error] = generatePickler

  extension [A](resp: Either[String, A])
    private def asError(isCacheable: Boolean): Either[Error, A] =
      resp.leftMap(Error(_, isCacheable))

  extension [A](e: Either[Error, A])
    private def asResponse: Either[String, A] = e.leftMap(_.message)

  def ephemerisRequest[F[_]: Concurrent](
    request:  HorizonsMessage.EphemerisRequest,
    client:   HorizonsClient[F],
    cache:    Cache[F],
    callback: Either[String, HorizonsEphemeris] => F[Unit]
  ): F[Unit] =
    def doRequest(request: HorizonsMessage.EphemerisRequest): F[Either[Error, HorizonsEphemeris]] =
      client
        .ephemeris(
          request.key,
          request.site,
          request.start,
          request.stop,
          request.elements
        )
        .map(_.asError(true))
        .handleError(t => Error(s"Error getting Ephemeris: ${t.getMessage}", false).asLeft)

    val cacheableRequest =
      Cacheable(
        CacheName("horizonsEphemeris"),
        cacheVersion,
        doRequest,
        (_, result) => result.fold(_.isCacheable, _ => true)
      )

    cache.eval(cacheableRequest).apply(request).flatMap(e => callback(e.asResponse))

  def alignedEphemerisRequest[F[_]: Concurrent](
    request:  HorizonsMessage.AlignedEphemerisRequest,
    client:   HorizonsClient[F],
    cache:    Cache[F],
    callback: Either[String, HorizonsEphemeris] => F[Unit]
  ): F[Unit] =
    def doRequest(
      request: HorizonsMessage.AlignedEphemerisRequest
    ): F[Either[Error, HorizonsEphemeris]] =
      client
        .alignedEphemeris(
          request.key,
          request.site,
          request.start,
          request.days,
          request.cadence
        )
        .map(_.asError(true))
        .handleError(t => Error(s"Error getting Ephemeris: ${t.getMessage}", false).asLeft)

    val cacheableRequest =
      Cacheable(
        CacheName("horizonsAlignedEphemeris"),
        cacheVersion,
        doRequest,
        (_, result) => result.fold(_.isCacheable, _ => true)
      )
    cache.eval(cacheableRequest).apply(request).flatMap(e => callback(e.asResponse))
