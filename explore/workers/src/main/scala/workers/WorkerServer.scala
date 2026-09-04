// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.Pickler
import cats.Monoid
import cats.effect.Fiber
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import explore.model.AppConfig
import explore.model.boopickle.Boopickle.*
import explore.utils.version
import lucuma.core.enums.ExecutionEnvironment
import lucuma.ui.otel.OtelSdk
import org.scalajs.dom
import org.scalajs.dom.DedicatedWorkerGlobalScope
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.console.ConsoleLoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.context.LocalContext
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import WorkerMessage.*

/**
 * Implements the server side of a simple client/server protocol that provides a somewhat more
 * functional/effecful way of communicating with workers.
 */
trait WorkerServer[T: Pickler](using Monoid[IO[Unit]]):
  /** Reported as `service.name` on this worker's spans. */
  protected val serviceName: String = "explore-worker"

  protected val run: IO[Unit] =
    (for {
      dispatcher              <- Dispatcher.parallel[IO]
      given LoggerFactory[IO] <- Resource.eval(setupLoggerFactory)
      given Logger[IO]         = LoggerFactory[IO].getLoggerFromName("worker-server")
      self                    <- Resource.eval(IO.delay(dom.DedicatedWorkerGlobalScope.self))
      config                  <- Resource.eval(
                                   WorkerConfig
                                     .load[IO](self.location.origin, self.location.host)
                                 )
      otel                    <- setupOtel(serviceName, config)
      _                       <- Resource.eval(runInternal(dispatcher, self, config, otel))
    } yield ()).useForever.void

  private def setupOtel(
    serviceName: String,
    config:      Option[AppConfig]
  )(using Logger[IO]): Resource[IO, OtelSdk.OtelResources] =
    val endpoint = config.flatMap(_.otelEndpoint.map(_.value))
    Resource.eval(
      endpoint.fold(
        Logger[IO].warn(s"[$serviceName] No tracing endpoint configured, spans are not exported")
      )(u => Logger[IO].info(s"[$serviceName] Tracing spans to [$u]"))
    ) *> OtelSdk.build(
      endpoint,
      serviceName,
      config.map(c => version(c.environment).value).getOrElse(""),
      config.map(_.environment).getOrElse(ExecutionEnvironment.Development)
    )

  /**
   * Provide an interface to handlers with an incoming message and a method to send responses (which
   * can be invoked multiple times; the client will receive a `Stream` of responses).
   */
  protected case class Invocation(data: T, rawData: Pickled, respondRaw: Pickled => IO[Unit]) {
    def respond[S: Pickler](value: S): IO[Unit] = respondRaw(Pickled(asBytes(value)))
  }

  /**
   * Handle server-specific messages. Tracer[IO] is the per-worker SDK tracer (or noop).
   */
  protected def handler(
    config: Option[AppConfig]
  ): (LoggerFactory[IO], Tracer[IO], TracerProvider[IO]) ?=> IO[Invocation => IO[Unit]]

  protected def setupLoggerFactory: IO[LoggerFactory[IO]] =
    IO.pure(ConsoleLoggerFactory.create[IO])

  protected def mount(
    self:         DedicatedWorkerGlobalScope,
    handlerFn:    Invocation => IO[Unit],
    cancelTokens: Ref[IO, Map[WorkerProcessId, IO[Unit]]],
    localCtx:     LocalContext[IO]
  )(dispatcher: Dispatcher[IO])(using LoggerFactory[IO], Tracer[IO]): IO[Unit] =
    given Logger[IO] = LoggerFactory[IO].getLoggerFromName("worker-server")

    IO.delay(
      self.onmessage = (msg: dom.MessageEvent) =>
        dispatcher.unsafeRunAndForget(
          decodeFromTransferable[FromClient](msg)
            .map {
              case FromClient.ClientReady               =>
                // Re-send ServerReady if main thread reconnects
                postAsTransferable[IO, FromServer](self, FromServer.ServerReady)
              case FromClient.Start(id, payload, tpOpt) =>
                val parentCtx = tpOpt.fold(Context.root) { tp =>
                  W3CTraceContextPropagator.default
                    .extract(Context.root, Map("traceparent" -> tp))
                }
                for
                  data <- IO.fromEither(fromBytes[T](payload.value))
                  _    <-
                    (localCtx.scope(
                      Tracer[IO]
                        .span(s"worker.handle ${WorkerRequest.name(data)}")
                        .surround(
                          handlerFn(
                            Invocation(
                              data,
                              payload,
                              pickled =>
                                postAsTransferable[IO, FromServer](
                                  self,
                                  FromServer.Data(id, pickled)
                                ) >>
                                  // Important so that long-running processes don't hog the scheduler.
                                  IO.cede
                            )
                          )
                        )
                    )(parentCtx) >>
                      postAsTransferable[IO, FromServer](self, FromServer.Complete(id)))
                      .handleErrorWith(t =>
                        postAsTransferable[IO, FromServer](
                          self,
                          FromServer.Error(id, WorkerException.fromThrowable(t))
                        ) >> IO.cede
                      )
                      .guarantee(cancelTokens.update(_ - id))
                      .start
                      .flatMap((fiber: Fiber[IO, Throwable, Unit]) =>
                        cancelTokens.update(_ + (id -> fiber.cancel))
                      )
                yield ()
              case FromClient.End(id)                   =>
                cancelTokens.modify { tokenMap =>
                  val token = tokenMap.get(id).orEmpty
                  (tokenMap - id, token)
                }.flatten
            }
            .orEmpty
            .handleErrorWith(e => Logger[IO].error(e)("Error processing message in worker"))
        )
    ).handleErrorWith(e => Logger[IO].error(e)("Error initializing worker"))

  protected def runInternal(
    dispatcher: Dispatcher[IO],
    self:       DedicatedWorkerGlobalScope,
    config:     Option[AppConfig],
    otel:       OtelSdk.OtelResources
  )(using LoggerFactory[IO]): IO[Unit] =
    given Logger[IO]         = LoggerFactory[IO].getLoggerFromName("worker-server")
    given Tracer[IO]         = otel.tracer
    given TracerProvider[IO] = otel.tracerProvider

    for {
      handlerFn    <- handler(config)
      cancelTokens <- Ref[IO].of(Map.empty[WorkerProcessId, IO[Unit]])
      _            <- debug"Mounting"
      _            <- mount(self, handlerFn, cancelTokens, otel.localCtx)(dispatcher)
      _            <- debug"Mounted, sending ready"
      // The client may have missed the ServerReady we send in response to its ClientReady (or we
      // may have missed its ClientReady altogether), so always send one here too. This assures the
      // client gets at least one ServerReady.
      _            <- postAsTransferable[IO, FromServer](self, FromServer.ServerReady)
      _            <- debug"Ready sent!"
    } yield ()
