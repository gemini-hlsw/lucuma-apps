// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.otel

import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import io.opentelemetry.instrumentation.runtimetelemetry.RuntimeTelemetry
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Site
import observe.model.config.OtelConfiguration
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.instrumentation.ce.IORuntimeMetrics
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters.*

case class OtelServices[F[_]](
  tracer:         Tracer[F],
  tracerProvider: TracerProvider[F],
  meterProvider:  MeterProvider[F]
)

object ObserveOtel:

  def resource[F[_]: Async: LiftIO](
    serviceName:    String,
    serviceVersion: String,
    site:           Site,
    environment:    ExecutionEnvironment,
    config:         OtelConfiguration
  ): Resource[F, OtelServices[F]] =
    (config.endpoint, config.key).tupled match
      case Some((endpoint, key)) =>
        given LocalProvider[F, Context] = LocalProvider.fromLiftIO[F, Context]

        OtelJava
          .autoConfigured[F]: builder =>
            builder.addPropertiesSupplier: () =>
              Map(
                "otel.service.name"           -> serviceName,
                "otel.resource.attributes"    ->
                  s"service.version=$serviceVersion,deployment.environment.name=${environment.tag.toLowerCase},site=${site.tag}",
                "otel.exporter.otlp.protocol" -> "http/protobuf",
                "otel.exporter.otlp.endpoint" -> endpoint.renderString,
                "otel.exporter.otlp.headers"  -> s"Authorization=Basic $key",
                "otel.exporter.otlp.timeout"  -> "30000"
              ).asJava
          .flatTap: otel =>
            Resource.fromAutoCloseable(
              Sync[F].delay(RuntimeTelemetry.create(otel.underlying))
            )
          .flatTap: otel =>
            given MeterProvider[F] = otel.meterProvider
            IORuntimeMetrics.register[F](IORuntime.global.metrics, IORuntimeMetrics.Config.default)
          .evalMap: otel =>
            otel.tracerProvider
              .get(serviceName)
              .map: tracer =>
                OtelServices(
                  tracer = tracer,
                  tracerProvider = otel.tracerProvider,
                  meterProvider = otel.meterProvider
                )
      case None                  =>
        Resource.pure(
          OtelServices(
            tracer = Tracer.noop,
            tracerProvider = TracerProvider.noop,
            meterProvider = MeterProvider.noop
          )
        )
