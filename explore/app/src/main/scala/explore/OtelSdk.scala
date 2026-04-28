// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.*
import cats.syntax.all.*
import explore.model.TracingConfig
import fs2.io.compression.fs2ioCompressionForLiftIO
import lucuma.core.enums.ExecutionEnvironment
import lucuma.ui.sso.UserVault
import org.http4s.dom.FetchClientBuilder
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
import org.typelevel.otel4s.semconv.attributes.ServiceAttributes
import org.typelevel.otel4s.semconv.experimental.attributes.DeploymentExperimentalAttributes
import org.typelevel.otel4s.semconv.experimental.attributes.UserExperimentalAttributes
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.*

// Setup the otel scala sdk to send instrumentation
object OtelSdk:
  private val ServiceName = "explore"

  case class OtelResources(tracer: Tracer[IO], tracerProvider: TracerProvider[IO])

  private val Noop = OtelResources(Tracer.noop[IO], TracerProvider.noop[IO])

  def build(
    config:      Option[TracingConfig],
    version:     String,
    environment: ExecutionEnvironment,
    vault:       Option[UserVault]
  ): Resource[IO, OtelResources] =
    config.fold(Resource.pure[IO, OtelResources](Noop)): cfg =>
      for {
        local                 <- Resource.eval(LocalProvider[IO, Context].local)
        given LocalContext[IO] = local
        client                <- FetchClientBuilder[IO].resource
        exporter              <- OtlpSpanExporter
                                   .builder[IO]
                                   .withEndpoint(cfg.value)
                                   .withClient(client)
                                   .build
        processor             <- BatchSpanProcessor
                                   .builder[IO](exporter)
                                   .withScheduleDelay(30.seconds) // default is 5, seems too short
                                   .build
        traceProvider         <- Resource.eval:
                                   SdkTracerProvider
                                     .builder[IO]
                                     .addResource(resourceAttr(version, environment, vault))
                                     .addSpanProcessor(processor)
                                     .addTextMapPropagators(W3CTraceContextPropagator.default)
                                     .build
        tracer                <- Resource.eval(traceProvider.tracer(ServiceName).get)
      } yield OtelResources(tracer, traceProvider)

  private def resourceAttr(
    serviceVersion: String,
    environment:    ExecutionEnvironment,
    vault:          Option[UserVault]
  ): TelemetryResource =
    val base = Attributes(
      Attribute(ServiceAttributes.ServiceName, ServiceName),
      Attribute(ServiceAttributes.ServiceVersion, serviceVersion),
      Attribute(DeploymentExperimentalAttributes.DeploymentEnvironmentName,
                environment.tag.toLowerCase
      )
    )

    val userAttrs = vault.foldMap: v =>
      Attributes(
        Attribute(UserExperimentalAttributes.UserId, v.user.id.show),
        Attribute(UserExperimentalAttributes.UserRoles, v.roleNames),
        Attribute(UserExperimentalAttributes.UserName, v.user.displayName)
      )

    TelemetryResource(base |+| userAttrs)
