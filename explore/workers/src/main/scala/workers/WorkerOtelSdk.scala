// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import explore.model.AppConfig
import lucuma.ui.otel.OtelSdk
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.semconv.attributes.ServiceAttributes

/** A worker gets its own sdk: it shares no global with the main thread. */
object WorkerOtelSdk:
  def build(serviceName: String, config: Option[AppConfig])(using
    Logger[IO]
  ): Resource[IO, OtelSdk.OtelResources] =
    val uri = config.flatMap(_.otelEndpoint.map(_.value))

    Resource.eval(
      uri.fold(
        Logger[IO].warn(s"[$serviceName] No tracing endpoint configured, spans are not exported")
      )(u => Logger[IO].info(s"[$serviceName] Tracing spans to [$u]"))
    ) >> OtelSdk.build(
      uri,
      serviceName,
      TelemetryResource(Attributes(Attribute(ServiceAttributes.ServiceName, serviceName))),
      OtelSdk.DefaultScheduleDelay
    )
