// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.config

import cats.effect.Sync
import cats.syntax.all.*
import com.typesafe.config.Config
import lucuma.core.enums.Site
import lucuma.sso.client.util.GpgPublicKeyReader
import navigate.model.config.*
import pureconfig.*
import pureconfig.error.*
import pureconfig.module.catseffect.syntax.*
import pureconfig.module.http4s.*
import pureconfig.module.ip4s.*

import java.security.PublicKey

case class SiteValueUnknown(site: String)        extends FailureReason {
  def description: String = s"site '$site' invalid"
}
case class ModeValueUnknown(mode: String)        extends FailureReason {
  def description: String = s"mode '$mode' invalid"
}
case class ControlModeValueUnknown(mode: String) extends FailureReason {
  def description: String = s"control mode '$mode' invalid"
}
case class PublicKeyUnknown(value: String)       extends FailureReason {
  def description: String = s"publicKey '$value' invalid"
}

given ConfigReader[PublicKey] =
  ConfigReader.fromCursor[PublicKey] { cf =>
    cf.asString.flatMap { c =>
      GpgPublicKeyReader
        .publicKey(c)
        .leftFlatMap(_ => cf.failed(PublicKeyUnknown(c)))
    }
  }

given ConfigReader[Site] = ConfigReader.fromCursor[Site] { cf =>
  cf.asString.flatMap {
    case "GS" => Site.GS.asRight
    case "GN" => Site.GN.asRight
    case s    => cf.failed(SiteValueUnknown(s))
  }
}

given ConfigReader[Mode] = ConfigReader.fromCursor[Mode] { cf =>
  cf.asString.flatMap {
    case "production" => Mode.Production.asRight
    case "dev"        => Mode.Development.asRight
    case s            => cf.failed(ModeValueUnknown(s))
  }
}

given ConfigReader[SubsystemControlMode] =
  ConfigReader.fromCursor[SubsystemControlMode] { cf =>
    cf.asString.flatMap { c =>
      SubsystemControlMode.fromString(c) match {
        case Some(x) => x.asRight
        case _       => cf.failed(ControlModeValueUnknown(c))
      }
    }
  }

given ConfigReader[TLSConfig] = ConfigReader.derived

given ConfigReader[fs2.io.file.Path] = ConfigReader.fromCursor[fs2.io.file.Path] { cf =>
  cf.asString.map(fs2.io.file.Path.apply)
}

def loadConfiguration[F[_]: Sync](config: ConfigObjectSource): F[NavigateConfiguration] = {
  given ConfigReader[NavigateConfiguration] =
    ConfigReader.derived[NavigateConfiguration]
  config.loadF[F, NavigateConfiguration]()
}

/** Key under which the control mode of each subsystem is configured. */
val SubsystemControlModesKey: String = "navigate-engine.subsystem-control-modes"

/** Former name of [[SubsystemControlModesKey]], still accepted with a warning. */
val DeprecatedSystemControlKey: String = "navigate-engine.system-control"

/** Whether the configuration still uses the deprecated `navigate-engine.system-control` key. */
def hasDeprecatedSystemControl(config: Config): Boolean =
  config.hasPath(DeprecatedSystemControlKey)

/**
 * Moves the values under the deprecated `navigate-engine.system-control` key to
 * `navigate-engine.subsystem-control-modes`. The deprecated values take precedence over the ones
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
