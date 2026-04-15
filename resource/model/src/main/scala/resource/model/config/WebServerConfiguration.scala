// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import pureconfig.*
import pureconfig.error.CannotConvert
import pureconfig.module.ip4s.*

import java.net.URI
import java.nio.file.Path

private given Eq[Path] = Eq.fromUniversalEquals

case class TLSConfig(keyStore: Path, keyStorePwd: String, certPwd: String) derives Eq, ConfigReader

case class WebServerConfiguration(
  host:            Host,
  port:            Port,
  insecurePort:    Port,
  externalBaseUrl: String,
  tls:             Option[TLSConfig]
) derives Eq,
      ConfigReader

case class DatabaseConfiguration(
  maxConnections: Int,
  host:           String,
  port:           Int,
  database:       String,
  user:           String,
  password:       String
) derives Eq:
  // We use Flyway (which uses JDBC) to perform schema migrations. Savor the irony.
  def jdbcUrl: String = s"jdbc:postgresql://${host}:${port}/${database}?sslmode=require"

object DatabaseConfiguration:
  def fromDatabaseUrl(
    maxConnections: Int,
    uri:            URI
  ): Either[pureconfig.error.FailureReason, resource.model.config.DatabaseConfiguration] =
    uri.getUserInfo.split(":") match
      case Array(user, password) =>
        DatabaseConfiguration(
          maxConnections = maxConnections,
          host = uri.getHost,
          port = uri.getPort,
          database = uri.getPath.drop(1),
          user = user,
          password = password
        ).asRight
      case _                     =>
        Left(CannotConvert(uri.toString, "DatabaseConfiguration", "Unable to parse database-url"))

// Parse as individual fields or single database url
given ConfigReader[DatabaseConfiguration] =
  ConfigReader
    .forProduct2("max-connections", "database-url")(
      DatabaseConfiguration.fromDatabaseUrl
    )
    .emap(identity)
    .orElse(
      ConfigReader
        .forProduct6(
          "max-connections",
          "host",
          "port",
          "database",
          "user",
          "password"
        )(DatabaseConfiguration.apply)
    )
