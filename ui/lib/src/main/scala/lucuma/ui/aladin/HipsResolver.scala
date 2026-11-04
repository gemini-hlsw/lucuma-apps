// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.aladin

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import org.scalajs.dom

import scala.concurrent.duration.*

/**
 * Picks the first HiPS mirror of a survey that answers, probing servers in order. Results are
 * shared by every Aladin view and kept in localStorage, so a server is probed at most once per
 * cache window. Probes ask for `properties` and one tile, as tile failures are invisible to us once
 * Aladin has loaded a survey.
 */
object HipsResolver:
  private val ProbeTimeout: FiniteDuration = 3.seconds
  private val UpTtl: FiniteDuration        = 15.minutes
  private val DownTtl: FiniteDuration      = 10.minutes
  private val StoragePrefix: String        = "lucuma-hips-status:"

  private case class Status(up: Boolean, checkedAt: Long):
    def isFresh(now: Long): Boolean =
      now - checkedAt < (if up then UpTtl else DownTtl).toMillis

  private val memory: Ref[IO, Map[String, Status]] = Ref.unsafe(Map.empty)

  private val inFlight: Ref[IO, Map[ImageSurvey, Deferred[IO, Option[HipsMirror]]]] =
    Ref.unsafe(Map.empty)

  private def readStorage(url: String): IO[Option[Status]] =
    IO(Option(dom.window.localStorage.getItem(StoragePrefix + url)))
      .map:
        _.flatMap: s =>
          s.split(':') match
            case Array(up, at) => at.toLongOption.map(Status(up == "1", _))
            case _             => none
      .handleError(_ => none)

  private def writeStorage(url: String, status: Status): IO[Unit] =
    IO(
      dom.window.localStorage.setItem(
        StoragePrefix + url,
        s"${if status.up then "1" else "0"}:${status.checkedAt}"
      )
    ).handleError(_ => ())

  private def cachedStatus(url: String, now: Long): IO[Option[Status]] =
    memory.get
      .map(_.get(url).filter(_.isFresh(now)))
      .flatMap(_.fold(readStorage(url).map(_.filter(_.isFresh(now))))(_.some.pure[IO]))

  private def fetchOk(url: String): IO[Boolean] =
    IO(new dom.AbortController()).flatMap: controller =>
      val init = new dom.RequestInit {}
      init.mode = dom.RequestMode.cors
      init.cache = dom.RequestCache.`no-store`
      init.signal = controller.signal
      IO.fromPromise(IO(dom.fetch(url, init)))
        .map(_.ok)
        .onCancel(IO(controller.abort()))
        .timeout(ProbeTimeout)
        .handleError(_ => false)

  // The `?probe` suffix keeps the service worker tile cache from answering for the server.
  private def probe(mirror: HipsMirror): IO[Boolean] =
    fetchOk(s"${mirror.url}/properties") &&
      fetchOk(s"${mirror.url}/Norder3/Dir0/Npix0.jpg?probe")

  extension (io: IO[Boolean])
    private def &&(other: => IO[Boolean]): IO[Boolean] =
      io.flatMap(ok => if ok then other else false.pure[IO])

  private def isUp(mirror: HipsMirror): IO[Boolean] =
    IO.realTime
      .map(_.toMillis)
      .flatMap: now =>
        cachedStatus(mirror.url, now).flatMap:
          case Some(status) => status.up.pure[IO]
          case None         =>
            probe(mirror).flatTap: up =>
              IO.realTime
                .map(_.toMillis)
                .flatMap: checkedAt =>
                  val status = Status(up, checkedAt)
                  memory.update(_ + (mirror.url -> status)) >> writeStorage(mirror.url, status)

  private def pick(survey: ImageSurvey): IO[Option[HipsMirror]] =
    survey.mirrors.toList.findM(isUp)

  /**
   * The first mirror of the survey that is up, or `None` if every mirror is down. Concurrent calls
   * for the same survey share a single probe, which keeps running if the caller is cancelled.
   */
  def resolve(survey: ImageSurvey): IO[Option[HipsMirror]] =
    IO.uncancelable: poll =>
      Deferred[IO, Option[HipsMirror]].flatMap: deferred =>
        inFlight
          .modify: running =>
            running.get(survey) match
              case Some(existing) => (running, poll(existing.get))
              case None           =>
                val run =
                  pick(survey)
                    .handleError(_ => none)
                    .flatMap(deferred.complete)
                    .guarantee(inFlight.update(_ - survey))
                (running + (survey -> deferred), run.start >> poll(deferred.get))
          .flatten

  /**
   * Emits the resolved mirror, and while no mirror is up, resolves again once the down status
   * expires.
   */
  def watch(survey: ImageSurvey): fs2.Stream[IO, Option[HipsMirror]] =
    fs2.Stream
      .eval(resolve(survey))
      .flatMap:
        case None   => fs2.Stream.emit(none) ++ fs2.Stream.sleep_[IO](DownTtl) ++ watch(survey)
        case mirror => fs2.Stream.emit(mirror)
