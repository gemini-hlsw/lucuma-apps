// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Show
import cats.derived.*
import cats.syntax.all.*
import io.circe.*
import io.circe.parser.decode
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.util.NewType
import lucuma.ui.sso.SSOConfig
import org.http4s.Uri
import org.http4s.circe.*

object TracingConfig extends NewType[Uri]:
  given Show[Type] = Show[Uri].contramap(_.value)
type TracingConfig = TracingConfig.Type

case class AppConfig(
  hostName:         String,
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  odbRestURI:       Uri,
  itcURI:           Uri,
  sso:              SSOConfig,
  // Renamed from `tracing`. previous pwa clients will get a None and should load anyway
  otelEndpoint:     Option[TracingConfig]
) derives Eq,
      Show

object AppConfig:
  given Decoder[AppConfig] = Decoder.instance: c =>
    for
      hostName         <- c.get[String]("hostName")
      environment      <- c.get[ExecutionEnvironment]("environment")
      preferencesDBURI <- c.get[Uri]("preferencesDBURI")
      odbURI           <- c.get[Uri]("odbURI")
      odbRestURI       <- c.get[Uri]("odbRestURI")
      itcURI           <- c.get[Uri]("itcURI")
      sso              <- c.get[SSOConfig]("sso")
      otelEndpoint     <- c.get[Option[TracingConfig]]("otelEndpoint")
    yield AppConfig(
      hostName,
      environment,
      preferencesDBURI,
      odbURI,
      odbRestURI,
      itcURI,
      sso,
      otelEndpoint
    )

  def parseConf(host: String, json: String): Either[Throwable, AppConfig] =
    decode[List[AppConfig]](json)
      .leftMap(err => new Exception("Could not parse configuration from JSON.", err))
      .flatMap: confs =>
        confs
          .find(conf => host.startsWith(conf.hostName))
          .orElse(confs.find(_.hostName === "*"))
          .toRight(new Exception("Host not found in configuration."))
