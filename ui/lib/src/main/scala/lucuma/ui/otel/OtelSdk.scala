// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.otel

import cats.effect.*
import cats.syntax.all.*
import fs2.io.compression.fs2ioCompressionForLiftIO
import lucuma.core.enums.ExecutionEnvironment
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
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

/**
 * Setup of the otel scala sdk for browser apps, sending spans to an OTLP endpoint over `fetch`.
 */
object OtelSdk:
  /** The default is 5 seconds, which is too short for a browser app. */
  val DefaultScheduleDelay: FiniteDuration = 30.seconds

  case class OtelResources(tracer: Tracer[IO], tracerProvider: TracerProvider[IO])

  val Noop: OtelResources = OtelResources(Tracer.noop[IO], TracerProvider.noop[IO])

  /**
   * Build the sdk, tracing to `endpoint`. Tracing must never keep the app from starting, so a
   * missing endpoint or any failure while building degrades to a noop tracer.
   */
  def build(
    endpoint:        Option[Uri],
    serviceName:     String,
    serviceVersion:  String,
    environment:     ExecutionEnvironment,
    extraAttributes: Attributes = Attributes.empty,
    scheduleDelay:   FiniteDuration = DefaultScheduleDelay
  )(using Logger[IO]): Resource[IO, OtelResources] =
    endpoint
      .fold(Resource.pure(Noop)): uri =>
        buildSdk(uri, serviceName, serviceVersion, environment, extraAttributes, scheduleDelay)
      .handleErrorWith: (t: Throwable) =>
        Resource
          .eval(Logger[IO].warn(t)("Error initializing tracing, continuing without it"))
          .as(Noop)

  private def buildSdk(
    uri:             Uri,
    serviceName:     String,
    serviceVersion:  String,
    environment:     ExecutionEnvironment,
    extraAttributes: Attributes,
    scheduleDelay:   FiniteDuration
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
                                 .withScheduleDelay(scheduleDelay)
                                 .build
      traceProvider         <- Resource.eval:
                                 SdkTracerProvider
                                   .builder[IO]
                                   .addResource:
                                     resourceAttr(
                                       serviceName,
                                       serviceVersion,
                                       environment,
                                       extraAttributes
                                     )
                                   .addSpanProcessor(processor)
                                   .addTextMapPropagators(W3CTraceContextPropagator.default)
                                   .build
      tracer                <- Resource.eval(traceProvider.tracer(serviceName).get)
    yield OtelResources(tracer, traceProvider)

  private def resourceAttr(
    serviceName:     String,
    serviceVersion:  String,
    environment:     ExecutionEnvironment,
    extraAttributes: Attributes
  ): TelemetryResource =
    TelemetryResource:
      Attributes(
        Attribute(ServiceAttributes.ServiceName, serviceName),
        Attribute(ServiceAttributes.ServiceVersion, serviceVersion),
        Attribute(DeploymentAttributes.DeploymentEnvironmentName, environment.tag.toLowerCase)
      ) |+| extraAttributes

  /**
   * Middleware tracing outgoing http calls, propagating the current span context so that the server
   * side of the call joins the same trace.
   */
  def traceMiddleware[F[_]: {Async, TracerProvider}]: F[Client[F] => Client[F]] =
    ClientMiddleware
      .builder[F](ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {}))
      .build
      .map(middleware => (client: Client[F]) => middleware(client))
