// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.Async
import cats.syntax.all.*
import explore.model.AppConfig
import org.http4s.Uri
import org.http4s.dom.FetchClientBuilder
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Workers read the same configuration file the main thread does, keyed on their own host.
 */
object WorkerConfig:
  private val ConfigFile: String           = "environments.conf.json"
  private val FetchTimeout: FiniteDuration = 5.seconds

  /** `None` if the configuration cannot be read; callers degrade instead of failing to start. */
  def load[F[_]: {Async as F, Logger as L}](origin: String, host: String): F[Option[AppConfig]] =
    F
      .fromEither(Uri.fromString(s"$origin/$ConfigFile"))
      .flatMap: uri =>
        FetchClientBuilder[F].withRequestTimeout(FetchTimeout).create.expect[String](uri)
      .flatMap(json => F.fromEither(AppConfig.parseConf(host, json)))
      .map(_.some)
      .handleErrorWith: t =>
        L.warn(t)(s"Could not read the configuration for host [$host]").as(none)
