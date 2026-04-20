// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Random
import fs2.io.compression.fs2ioCompressionForLiftIO
import org.http4s.Uri
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
import org.typelevel.otel4s.sdk.trace.exporter.NonEmptySpanExporter
import org.typelevel.otel4s.sdk.trace.processor.BatchSpanProcessor
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.*

object WorkerOtelSdk:
  case class Resources(tracer: Tracer[IO], localCtx: LocalContext[IO])

  def build(endpoint: Option[String]): Resource[IO, Resources] =
    for
      given Random[IO]      <- Resource.eval(Random.scalaUtilRandom[IO])
      localCtx              <- Resource.eval(LocalProvider[IO, Context].local)
      given LocalContext[IO] = localCtx
      tracer                <- endpoint
                                 .flatMap(Uri.fromString(_).toOption)
                                 .fold(
                                   Resource.pure(Tracer.noop[IO])
                                 ) { uri =>
                                   for
                                     client   <- FetchClientBuilder[IO].resource
                                     exporter <- OtlpSpanExporter
                                                   .builder[IO]
                                                   .withEndpoint(uri)
                                                   .withClient(client)
                                                   .build
                                     proc     <- BatchSpanProcessor
                                                   .builder[IO](NonEmptySpanExporter(exporter))
                                                   .withScheduleDelay(30.seconds)
                                                   .build
                                     tp       <- Resource.eval:
                                                   SdkTracerProvider
                                                     .builder[IO]
                                                     .addResource(
                                                       TelemetryResource(
                                                         Attributes(Attribute("service.name", "explore-worker"))
                                                       )
                                                     )
                                                     .addSpanProcessor(proc)
                                                     .addTextMapPropagators(W3CTraceContextPropagator.default)
                                                     .build
                                     t        <- Resource.eval(tp.tracer("explore-worker").get)
                                   yield t
                                 }
    yield Resources(tracer, localCtx)
