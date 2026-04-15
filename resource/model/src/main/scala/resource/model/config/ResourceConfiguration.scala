// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import pureconfig.*
import pureconfig.error.*

case class EnumValueUnknown(value: String, available: List[String]) extends FailureReason:
  def description: String =
    s"enumerated value '$value' invalid. Should be one of ${available.mkString("[", ", ", "]")}"

given [A: Enumerated]: ConfigReader[A] = ConfigReader.fromCursor[A]: cf =>
  cf.asString.flatMap: c =>
    Enumerated[A].fromTag(c) match
      case Some(x) => x.asRight
      case _       => cf.failed(EnumValueUnknown(c, Enumerated[A].all.map(_.toString)))

final case class ResourceConfiguration(
  site:        Site,
  database:    DatabaseConfiguration,
  environment: ExecutionEnvironment,
  webServer:   WebServerConfiguration
) derives Eq,
      ConfigReader
