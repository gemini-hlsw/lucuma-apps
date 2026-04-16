// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.*
import cats.effect.std.Random
import cats.syntax.all.*
import explore.model.TracingConfig
import fs2.io.compression.fs2ioCompressionForLiftIO
import lucuma.core.enums.ExecutionEnvironment
import lucuma.ui.sso.UserVault
import org.http4s.Header
import org.http4s.Headers
import org.http4s.dom.FetchClientBuilder
import org.typelevel.ci.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.context.LocalContext
import org.typelevel.otel4s.sdk.exporter.otlp.trace.OtlpSpanExporter
import org.typelevel.otel4s.sdk.trace.SdkTracerProvider
import org.typelevel.otel4s.sdk.trace.exporter.NonEmptySpanExporter
import org.typelevel.otel4s.sdk.trace.processor.BatchSpanProcessor
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.*

// Setup the otel scala sdk to send instrumentation
object OtelSdk:
  private val InstrumentationScope = "explore"

  def build(
    config:         Option[TracingConfig],
    serviceVersion: String,
    environment:    ExecutionEnvironment,
    vault:          Option[UserVault]
  ): Resource[IO, Tracer[IO]] =
    config.fold(Resource.pure[IO, Tracer[IO]](Tracer.noop[IO])): cfg =>
      for
        given Random[IO]      <- Resource.eval(Random.scalaUtilRandom[IO])
        localContext          <- Resource.eval(LocalProvider[IO, Context].local)
        given LocalContext[IO] = localContext
        client                <- FetchClientBuilder[IO].resource
        exporter              <-
          OtlpSpanExporter
            .builder[IO]
            .withEndpoint(cfg.endpoint)
            .addHeaders(headersFor(cfg.headers))
            .withClient(client)
            .build
        processor             <- BatchSpanProcessor
                                   .builder[IO](NonEmptySpanExporter(exporter))
                                   .withScheduleDelay(30.seconds)
                                   .build
        tp                    <-
          Resource.eval:
            SdkTracerProvider
              .builder[IO]
              .addResource(resourceFor(cfg.serviceName, serviceVersion, environment, vault))
              .addSpanProcessor(processor)
              .build
        tracer                <- Resource.eval(tp.tracer(InstrumentationScope).get)
      yield tracer

  private def headersFor(headers: Map[String, String]): Headers =
    Headers(headers.toList.map((k, v) => Header.Raw(CIString(k), v)))

  private def resourceFor(
    serviceName:    String,
    serviceVersion: String,
    environment:    ExecutionEnvironment,
    vault:          Option[UserVault]
  ): TelemetryResource =
    val base = Attributes(
      Attribute("service.name", serviceName),
      Attribute("service.version", serviceVersion),
      Attribute("deployment.environment.name", environment.tag.toLowerCase)
    )

    val userAttrs = vault.foldMap: v =>
      Attributes(
        Attribute("user.id", v.user.id.show),
        Attribute("user.roles", v.roleNames.toSeq: Seq[String]),
        Attribute("user.name", v.user.displayName)
      )
    TelemetryResource(base |+| userAttrs)
