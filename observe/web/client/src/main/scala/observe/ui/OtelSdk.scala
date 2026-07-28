// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui

import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.io.compression.fs2ioCompressionForLiftIO
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Site
import org.http4s.Uri
import org.http4s.dom.FetchClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.context.LocalContext
import org.typelevel.otel4s.sdk.exporter.otlp.trace.OtlpSpanExporter
import org.typelevel.otel4s.sdk.trace.SdkTracerProvider
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.sdk.trace.processor.BatchSpanProcessor
import org.typelevel.otel4s.semconv.attributes.DeploymentAttributes
import org.typelevel.otel4s.semconv.attributes.ServiceAttributes
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.*

// Setup the otel scala sdk to send instrumentation
object OtelSdk:
  private val ServiceName = "observe"

  case class OtelResources(tracer: Tracer[IO], tracerProvider: TracerProvider[IO])

  private val Noop = OtelResources(Tracer.noop[IO], TracerProvider.noop[IO])

  // Tracing must never keep the app from starting, so any failure degrades to a noop tracer.
  def build(
    endpoint:    Option[Uri],
    version:     NonEmptyString,
    site:        Site,
    environment: ExecutionEnvironment
  )(using Logger[IO]): Resource[IO, OtelResources] =
    endpoint
      .fold(Resource.pure[IO, OtelResources](Noop))(buildSdk(_, version, site, environment))
      .handleErrorWith: (t: Throwable) =>
        Resource
          .eval(Logger[IO].warn(t)("Error initializing tracing, continuing without it"))
          .as(Noop)

  private def buildSdk(
    uri:         Uri,
    version:     NonEmptyString,
    site:        Site,
    environment: ExecutionEnvironment
  ): Resource[IO, OtelResources] =
      for
        local                 <- Resource.eval(LocalProvider[IO, Context].local)
        given LocalContext[IO] = local
        client                <- FetchClientBuilder[IO].resource
        exporter              <- OtlpSpanExporter
                                   .builder[IO]
                                   .withEndpoint(uri)
                                   .withClient(client)
                                   .build
        processor             <- BatchSpanProcessor
                                   .builder[IO](exporter)
                                   .withScheduleDelay(30.seconds) // default is 5, seems too short
                                   .build
        traceProvider         <- Resource.eval:
                                   SdkTracerProvider
                                     .builder[IO]
                                     .addResource(resourceAttr(version, site, environment))
                                     .addSpanProcessor(processor)
                                     .addTextMapPropagators(W3CTraceContextPropagator.default)
                                     .build
        tracer                <- Resource.eval(traceProvider.tracer(ServiceName).get)
      yield OtelResources(tracer, traceProvider)

  private def resourceAttr(
    serviceVersion: NonEmptyString,
    site:           Site,
    environment:    ExecutionEnvironment
  ): TelemetryResource =
    TelemetryResource:
      Attributes(
        Attribute(ServiceAttributes.ServiceName, ServiceName),
        Attribute(ServiceAttributes.ServiceVersion, serviceVersion.value),
        Attribute(DeploymentAttributes.DeploymentEnvironmentName, environment.tag.toLowerCase),
        Attribute("site", site.tag)
      )
