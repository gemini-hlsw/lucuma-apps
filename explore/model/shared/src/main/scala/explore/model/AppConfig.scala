// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Show
import cats.effect.Async
import cats.syntax.all.*
import explore.model.enums.ExecutionEnvironment
import io.circe.*
import io.circe.generic.semiauto.*
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.all.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

case class SSOConfig(
  uri:                        Uri,
  readTimeoutSeconds:         Long = 3,
  refreshTimeoutDeltaSeconds: Long = 10, // time before expiration to renew
  refreshIntervalFactor:      Long = 1
) {
  val readTimeout: FiniteDuration         = FiniteDuration(readTimeoutSeconds, TimeUnit.SECONDS)
  val refreshTimeoutDelta: FiniteDuration =
    FiniteDuration(refreshTimeoutDeltaSeconds, TimeUnit.SECONDS)
}

object SSOConfig {
  implicit val eqSSOConfig: Eq[SSOConfig]     = Eq.fromUniversalEquals
  implicit val showSSOConfig: Show[SSOConfig] = Show.fromToString

  implicit val encoderSSOConfig: Encoder[SSOConfig] = deriveEncoder[SSOConfig]
  implicit val decoderSSOConfig: Decoder[SSOConfig] = deriveDecoder[SSOConfig]
}

case class AppConfig(
  environment:      ExecutionEnvironment,
  preferencesDBURI: Uri,
  odbURI:           Uri,
  itcURI:           Uri,
  sso:              SSOConfig
)

object AppConfig {
  implicit val eqAppConfig: Eq[AppConfig]     = Eq.fromUniversalEquals
  implicit val showAppConfig: Show[AppConfig] = Show.fromToString

  implicit val encoderAppConfig: Encoder[AppConfig] = deriveEncoder[AppConfig]
  implicit val decoderAppConfig: Decoder[AppConfig] = deriveDecoder[AppConfig]

  def fetchConfig[F[_]: Async](client: Client[F]): F[AppConfig] =
    client
      .get(uri"/conf.json")(_.decodeJson[AppConfig])
      .adaptError { case t =>
        new Exception("Could not retrieve configuration.", t)
      }
}
