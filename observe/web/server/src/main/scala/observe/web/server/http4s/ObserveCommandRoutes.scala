// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import _root_.io.circe.refined.given
import cats.effect.Async
import cats.syntax.all.*
import fs2.compression.Compression
import lucuma.core.circe.coulomb.given
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Step
import lucuma.core.refined.given
import lucuma.sso.client.SsoClient
import observe.model.ClientId
import observe.model.QueueItemId
import observe.model.QueuePauseId
import observe.model.enums.RunOverride
import observe.server.ObserveEngine
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.*
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.server.middleware.GZip
import org.typelevel.otel4s.Attribute

object ObserveCommandRoutes:
  private val ApiPrefix: List[String] = List("api", "observe")

  /** First segment of the daytime queue routes. */
  private val QueueRoute: String = "queue"

  /** The fixed words of the daytime queue routes, which their templates keep. */
  private val QueueRouteWords: Set[String] =
    Set(
      "add",
      "remove",
      "move",
      "obs",
      "pause",
      "orderingMode",
      "run",
      "holdAfterSteps",
      "holdAfterObservations",
      "abortAll",
      "abort",
      "retry",
      "clearCompleted",
      "lampsOff",
      "cancel"
    )

  /** A path segment, recognized by the same extractors the routes below use. */
  private enum PathVar:
    case ObsId(value: Observation.Id)
    case StepId(value: Step.Id)
    case Client(value: ClientId)
    case PauseId(value: QueuePauseId)
    case Other(value: String)

  /** A pause id is a UUID like a client id; it is told apart by the "pause" segment before it. */
  private def parseSegment(previous: Option[String], segment: String): PathVar =
    (previous, segment) match
      case (Some("pause"), QueuePauseIdVar(pauseId)) => PathVar.PauseId(pauseId)
      case (_, ObsIdVar(obsId))                      => PathVar.ObsId(obsId)
      case (_, StepIdVar(stepId))                    => PathVar.StepId(stepId)
      case (_, ClientIDVar(clientId))                => PathVar.Client(clientId)
      case (_, other)                                => PathVar.Other(other)

  private def parseSegments(segments: List[String]): List[PathVar] =
    segments
      .zip(none[String] :: segments.map(_.some))
      .map((segment, previous) => parseSegment(previous, segment))

  /** See `routeClassifier`. */
  private[http4s] def routeTemplate(segments: List[String]): String =
    val (prefix: List[String], rest: List[String])   = segments.splitAt(ApiPrefix.length)
    val (templated: List[String], _: Option[String]) =
      parseSegments(rest).foldLeft((List.empty[String], none[String])):
        case ((acc, command), segment) =>
          segment match
            case PathVar.ObsId(_)                                                                 =>
              ("{obsId}" :: acc, command)
            case PathVar.StepId(_)                                                                =>
              ("{stepId}" :: acc, command)
            case PathVar.Client(_)                                                                =>
              ("{clientId}" :: acc, command)
            case PathVar.PauseId(_)                                                               =>
              ("{pauseId}" :: acc, command)
            case PathVar.Other(c) if command.isEmpty                                              =>
              (c :: acc, c.some)
            case PathVar.Other(c) if command.contains_(QueueRoute) && QueueRouteWords.contains(c) =>
              (c :: acc, command)
            case PathVar.Other(_)                                                                 =>
              ("{param}" :: acc, command)
    (prefix ++ templated.reverse).mkString("/", "/", "")

  /**
   * The ids `routeTemplate` includes as span attributes, so that a trace can be found by what it
   * acted on — `{span.observe.obs.id = "o-5488"}` gathers every load, configure and step advance
   * for one observation.
   */
  private[http4s] def pathAttributes(segments: List[String]): List[Attribute[?]] =
    parseSegments(segments.drop(ApiPrefix.length))
      .flatMap:
        case PathVar.ObsId(obsId)     => List(Attribute("observe.obs.id", obsId.show))
        case PathVar.StepId(stepId)   => List(Attribute("observe.step.id", stepId.show))
        case PathVar.Client(clientId) =>
          List(Attribute("observe.client.id", clientId.value.toString))
        case PathVar.PauseId(pauseId) =>
          List(Attribute("observe.queue.pause.id", pauseId.value.toString))
        case PathVar.Other(_)         => Nil

/**
 * Rest Endpoints under the /api route
 */
class ObserveCommandRoutes[F[_]: {Async, Compression}](
  ssoClient: SsoClient[F, User],
  oe:        ObserveEngine[F]
) extends Http4sDsl[F] {

  given wvDecoder: EntityDecoder[F, WaterVapor]       = jsonOf[F, WaterVapor]
  given iqDecoder: EntityDecoder[F, ImageQuality]     = jsonOf[F, ImageQuality]
  given sbDecoder: EntityDecoder[F, SkyBackground]    = jsonOf[F, SkyBackground]
  given ceDecoder: EntityDecoder[F, CloudExtinction]  = jsonOf[F, CloudExtinction]
  given stepsDecoder: EntityDecoder[F, List[Step.Id]] = jsonOf[F, List[Step.Id]]

  private val commandServices: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "startSequence" /
        ObserverVar(obs) :? OptionalRunOverride(runOverride) =>
      ssoClient.require(req): user =>
        oe.startSequence(obsId, user, obs, clientId, runOverride.getOrElse(RunOverride.Default)) *>
          NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / StepIdVar(stepId) / ClientIDVar(clientId) /
        "execute" / SubsystemVar(subsystem) / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.configureSubsystem(obsId, obs, user, stepId, subsystem, clientId) *> NoContent()

    // In a number of endpoints, clientId is not used but we keep it anyway so that it's logged.
    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "holdSequence" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.requestSequenceHold(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "cancelHoldSequence" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.cancelSequenceHoldRequest(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "proceedAfterPrompt" /
        ObserverVar(obs) / SequenceTypeVar(seqType) :? OptionalRunOverride(runOverride) =>
      ssoClient.require(req): user =>
        oe.proceedAfterPrompt(
          obsId,
          user,
          obs,
          seqType,
          clientId,
          runOverride.getOrElse(RunOverride.Default)
        ) *>
          NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "skipAcquisition" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.skipAcquisition(obsId, user, obs, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "resetAcquisition" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.resetAcquisition(obsId, user, obs, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "reloadAfterEdit" /
        ObserverVar(obs) / SequenceTypeVar(seqType) =>
      ssoClient.require(req): user =>
        oe.reloadAfterEdit(obsId, user, obs, seqType, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / StepIdVar(stepId) / ClientIDVar(clientId) /
        "breakpoint" / ObserverVar(obs) / BreakpointVar(bp) =>
      ssoClient.require(req): user =>
        oe.setBreakpoints(obsId, user, obs, Set(stepId), bp) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "breakpoints" / ObserverVar(obs) / BreakpointVar(bp) =>
      ssoClient.require(req): user =>
        req.decode[List[Step.Id]]: steps =>
          oe.setBreakpoints(obsId, user, obs, steps.toSet, bp) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "stopExposure" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.stopExposure(obsId, obs, user, graceful = false) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "stopExposureGracefully" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.stopExposure(obsId, obs, user, graceful = true) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "abortExposure" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.abortExposure(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "pauseExposure" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.pauseExposure(obsId, obs, user, graceful = false) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "rewindStep" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.rewindStep(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "cancelRewindStep" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.cancelRewindRequest(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "pauseExposureGracefully" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.pauseExposure(obsId, obs, user, graceful = true) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "resumeExposure" / ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.resumeExposure(obsId, obs, user) *> NoContent()

    case req @ POST -> Root / ClientIDVar(clientId) / "operator" / OperatorVar(op) =>
      ssoClient.require(req): user =>
        oe.setOperator(user, op) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "observer" /
        ObserverVar(obs) =>
      ssoClient.require(req): user =>
        oe.setObserver(obsId, user, obs) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "tcsEnabled" /
        SubsystemEnabledVar(tcsEnabled) =>
      ssoClient.require(req): user =>
        oe.setTcsEnabled(obsId, user, tcsEnabled, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "gcalEnabled" /
        SubsystemEnabledVar(gcalEnabled) =>
      ssoClient.require(req): user =>
        oe.setGcalEnabled(obsId, user, gcalEnabled, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "instrumentEnabled" /
        SubsystemEnabledVar(instEnabled) =>
      ssoClient.require(req): user =>
        oe.setInstrumentEnabled(obsId, user, instEnabled, clientId) *> NoContent()

    case req @ POST -> Root / ObsIdVar(obsId) / ClientIDVar(clientId) / "dhsEnabled" /
        SubsystemEnabledVar(dhsEnabled) =>
      ssoClient.require(req): user =>
        oe.setDhsEnabled(obsId, user, dhsEnabled, clientId) *> NoContent()

    case req @ POST -> Root / ClientIDVar(clientId) / "iq" =>
      ssoClient.require(req): user =>
        req.decode[ImageQuality](iq => oe.setImageQuality(iq, user, clientId) *> NoContent())

    case req @ POST -> Root / ClientIDVar(clientId) / "wv" =>
      ssoClient.require(req): user =>
        req.decode[WaterVapor](wv => oe.setWaterVapor(wv, user, clientId) *> NoContent())

    case req @ POST -> Root / ClientIDVar(clientId) / "sb" =>
      ssoClient.require(req): user =>
        req.decode[SkyBackground](sb => oe.setSkyBackground(sb, user, clientId) *> NoContent())

    case req @ POST -> Root / ClientIDVar(clientId) / "ce" =>
      ssoClient.require(req): user =>
        req.decode[CloudExtinction](ce => oe.setCloudExtinction(ce, user, clientId) *> NoContent())

    case req @ POST -> Root / "load" / InstrumentVar(i) / ObsIdVar(obsId) /
        ClientIDVar(clientId) / ObserverVar(observer) =>
      ssoClient.require(req): user =>
        oe.loadSequence(i, obsId, observer, user, clientId) *> NoContent()

    case req @ POST -> Root / "unload" / ObsIdVar(obsId) / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.unloadSequence(obsId, user, clientId) *> NoContent()

    // case POST -> Root / "unload" / "all" as user         =>
    //   oe.clearLoadedSequences(inputQueue, user) *> Ok(s"Queue cleared")

    case req @ POST -> Root / "queue" / "add" / ObsIdVar(obsId) / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.addToDaytimeQueue(obsId, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "remove" / "obs" / ObsIdVar(obsId) /
        ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.removeFromDaytimeQueue(QueueItemId.Obs(obsId), user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "remove" / "pause" / QueuePauseIdVar(pauseId) /
        ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.removeFromDaytimeQueue(QueueItemId.Pause(pauseId), user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "move" / "obs" / ObsIdVar(obsId) / IntVar(toIndex) /
        ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.moveInDaytimeQueue(QueueItemId.Obs(obsId), toIndex, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "move" / "pause" / QueuePauseIdVar(pauseId) /
        IntVar(toIndex) / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.moveInDaytimeQueue(QueueItemId.Pause(pauseId), toIndex, user, clientId) *>
          NoContent()

    case req @ POST -> Root / "queue" / "pause" / IntVar(atIndex) / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.addDaytimeQueuePause(atIndex, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "orderingMode" / QueueOrderingModeVar(mode) /
        ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.setDaytimeQueueOrderingMode(mode, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "run" / ClientIDVar(clientId) / ObserverVar(observer) =>
      ssoClient.require(req): user =>
        oe.runDaytimeQueue(observer, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "holdAfterSteps" / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.holdDaytimeQueueAfterCurrentSteps(user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "holdAfterObservations" / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.holdDaytimeQueueAfterCurrentObservations(user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "abortAll" / ClientIDVar(clientId) /
        ObserverVar(observer) =>
      ssoClient.require(req): user =>
        oe.abortDaytimeQueue(observer, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "obs" / ObsIdVar(obsId) / ClientIDVar(clientId) / "run" /
        ObserverVar(observer) =>
      ssoClient.require(req): user =>
        oe.runDaytimeQueueItem(obsId, observer, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "obs" / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "abort" / ObserverVar(observer) =>
      ssoClient.require(req): user =>
        oe.abortDaytimeQueueItem(obsId, observer, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "obs" / ObsIdVar(obsId) / ClientIDVar(clientId) /
        "retry" =>
      ssoClient.require(req): user =>
        oe.retryDaytimeQueueItem(obsId, user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "clearCompleted" / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.clearCompletedDaytimeQueue(user, clientId) *> NoContent()

    case req @ POST -> Root / "queue" / "lampsOff" / "cancel" / ClientIDVar(clientId) =>
      ssoClient.require(req): user =>
        oe.cancelDaytimeQueueLampsOff(user, clientId) *> NoContent()

    case GET -> Root / ClientIDVar(clientId) / "refresh" =>
      oe.requestRefresh(clientId) *> NoContent()

    case req @ POST -> Root / "resetconditions" =>
      ssoClient.require(req): _ =>
        oe.resetConditions *> NoContent()
  }

  /**
   * Supplies the `http.route` span attribute, which the otel4s server middleware uses to name spans
   * — `POST /api/observe/{obsId}/{clientId}/startSequence/{param}` rather than a bare `POST`.
   */
  val routeClassifier: RouteClassifier =
    RouteClassifier.of[F]:
      case req
          if req.uri.path.segments.map(_.decoded()).startsWith(ObserveCommandRoutes.ApiPrefix) =>
        ObserveCommandRoutes.routeTemplate(req.uri.path.segments.map(_.decoded()).toList)

  val service: HttpRoutes[F] =
    GZip(commandServices)
}
