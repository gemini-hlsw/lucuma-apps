// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.config

import cats.effect.Sync
import cats.syntax.all.*
import com.typesafe.config.Config
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import lucuma.sso.client.util.GpgPublicKeyReader
import observe.model.config.*
import observe.model.enums.SubsystemControlMode
import org.http4s.Uri
import pureconfig.*
import pureconfig.error.*
import pureconfig.module.catseffect.syntax.*
import pureconfig.module.http4s.*
import pureconfig.module.ip4s.*

import java.nio.file.Path
import java.security.PublicKey
import scala.concurrent.duration.FiniteDuration

case class EnumValueUnknown(value: String, available: List[String]) extends FailureReason {
  def description: String =
    s"enumerated value '$value' invalid. Should be one of ${available.mkString("[", ", ", "]")}"
}

case class ControlModeValueUnknown(mode: String) extends FailureReason {
  def description: String = s"subsystem control mode '$mode' invalid"
}
case class PublicKeyUnknown(value: String)       extends FailureReason {
  def description: String = s"publicKey '$value' invalid"
}

/**
 * Settings and decoders to parse the configuration files
 */
given [A: Enumerated]: ConfigReader[A] = ConfigReader.fromCursor[A]: cf =>
  cf.asString.flatMap: c =>
    Enumerated[A].fromTag(c) match
      case Some(x) => x.asRight
      case _       => cf.failed(EnumValueUnknown(c, Enumerated[A].all.map(_.toString)))

given ConfigReader[SubsystemControlMode] =
  ConfigReader.fromCursor[SubsystemControlMode] { cf =>
    cf.asString.flatMap { c =>
      Enumerated[SubsystemControlMode].fromTag(c) match {
        case Some(x) => x.asRight
        case _       => cf.failed(ControlModeValueUnknown(c))
      }
    }
  }

given ConfigReader[PublicKey] =
  ConfigReader.fromCursor[PublicKey] { cf =>
    cf.asString.flatMap { c =>
      GpgPublicKeyReader
        .publicKey(c)
        .leftFlatMap(_ => cf.failed(PublicKeyUnknown(c)))
    }
  }

given ConfigReader[SubsystemControlModes] = ConfigReader.derived

given ConfigReader[GpiUriSettings] = ConfigReader[Uri].map(GpiUriSettings.apply)

given ConfigReader[GhostUriSettings] = ConfigReader[Uri].map(GhostUriSettings.apply)

given ConfigReader[Igrins2UriSettings] = ConfigReader[Uri].map(Igrins2UriSettings.apply)

given ConfigReader[ObserveEngineConfiguration] = ConfigReader.derived

given ConfigReader[TLSConfig] = ConfigReader.derived

given ConfigReader[WebServerConfiguration] = ConfigReader.derived

given ConfigReader[LucumaSSOConfiguration] = ConfigReader.derived

given ConfigReader[OtelConfiguration] = ConfigReader.derived

given ConfigReader[ObserveConfiguration] = ConfigReader.derived

def loadConfiguration[F[_]: Sync](config: ConfigObjectSource): F[ObserveConfiguration] =
  config.loadF[F, ObserveConfiguration]()

/** Key under which the control mode of each subsystem is configured. */
val SubsystemControlModesKey: String = "observe-engine.subsystem-control-modes"

/** Former name of [[SubsystemControlModesKey]], still accepted with a warning. */
val DeprecatedSystemControlKey: String = "observe-engine.system-control"

/** Whether the configuration still uses the deprecated `observe-engine.system-control` key. */
def hasDeprecatedSystemControl(config: Config): Boolean =
  config.hasPath(DeprecatedSystemControlKey)

/**
 * Moves the values under the deprecated `observe-engine.system-control` key to
 * `observe-engine.subsystem-control-modes`. The deprecated values take precedence over the ones
 * already under the new key (e.g. the defaults in `base.conf`), as the deprecated key can only come
 * from a hand-written file that was not migrated.
 */
def migrateSystemControl(config: Config): Config =
  if hasDeprecatedSystemControl(config) then
    val deprecated: Config = config.getConfig(DeprecatedSystemControlKey)
    val migrated: Config   =
      if config.hasPath(SubsystemControlModesKey) then
        deprecated.withFallback(config.getConfig(SubsystemControlModesKey))
      else deprecated
    config
      .withoutPath(DeprecatedSystemControlKey)
      .withValue(SubsystemControlModesKey, migrated.root)
  else config
