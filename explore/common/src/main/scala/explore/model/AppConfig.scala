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
import lucuma.ui.sso.SSOConfig
import org.http4s.Uri
import org.http4s.circe.*

case class AppConfig(
  hostName:         String,
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  odbRestURI:       Uri,
  itcURI:           Uri,
  sso:              SSOConfig,
  tracing:          Option[TracingConfig]
) derives Eq,
      Show,
      Decoder

object AppConfig:
  def parseConf(host: String, json: String): AppConfig =
    decode[List[AppConfig]](json) match
      case Left(err)    =>
        throw new Exception("Could not parse configuration from JSON.", err)
      case Right(confs) =>
        confs
          .find(conf => host.startsWith(conf.hostName))
          .orElse(confs.find(_.hostName === "*"))
          .getOrElse(throw new Exception("Host not found in configuration."))
