// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import mouse.boolean.*
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.server.ApplyCommandResult
import navigate.server.acm.CadDirective
import navigate.server.acm.ParameterList.*
import navigate.server.acm.writeCadParam
import navigate.server.tcs.OiwfsEpicsSystem.commandWaitTime

import scala.concurrent.duration.FiniteDuration

trait CircularBufferControl[F[_]] {
  def startCircularBufferCommand(
    timeout: FiniteDuration
  ): CircularBufferControl.WfsCircularBufferCommand[F]
  def circularBufferStatus: VerifiedEpics[F, F, CircularBufferControl.CircularBufferStatus]
  def imgCircularBufferStrean: VerifiedEpics[F, Resource[F, *], Stream[F, Boolean]]

}

object CircularBufferControl {

  trait WfsCircularBufferCommand[F[_]] {
    def post: VerifiedEpics[F, F, ApplyCommandResult]
    def setEnabled(v: Boolean): WfsCircularBufferCommand[F]
  }

  case class CircularBufferStatus(
    imageEnabled: Boolean,
    aoEnabled:    Boolean,
    fgEnabled:    Boolean
  )

  class WfsCircularBufferCommandImpl[F[_]: {Temporal, Parallel}](
    chs:     CircularBufferChannels[F],
    timeout: FiniteDuration,
    params:  ParameterList[F] = List.empty[VerifiedEpics[F, F, Unit]]
  ) extends WfsCircularBufferCommand[F] {

    private def addParams(l: List[VerifiedEpics[F, F, Unit]]): WfsCircularBufferCommand[F] =
      new WfsCircularBufferCommandImpl(chs, timeout, params ++ l)

    override def post: VerifiedEpics[F, F, ApplyCommandResult] =
      writeChannel(chs.telltale, chs.dir)(
        CadDirective.CLEAR.pure[F]
      ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime)) *>
        params.compile *> writeChannel(chs.telltale, chs.dir)(
          CadDirective.START.pure[F]
        ) *>
        VerifiedEpics.liftF(Temporal[F].sleep(commandWaitTime).as(ApplyCommandResult.Completed))

    override def setEnabled(v: Boolean): WfsCircularBufferCommand[F] = addParams(
      List(
        writeCadParam(chs.telltale, chs.enableIm)(v.fold(1, 0)),
        writeCadParam(chs.telltale, chs.enableAo)(v.fold(1, 0)),
        writeCadParam(chs.telltale, chs.enableFg)(v.fold(1, 0))
      )
    )
  }

  def buildSystem[F[_]: {Async, Parallel, Dispatcher}](
    chs: CircularBufferChannels[F]
  ): CircularBufferControl[F] = new CircularBufferControl[F] {
    override def startCircularBufferCommand(timeout: FiniteDuration): WfsCircularBufferCommand[F] =
      new WfsCircularBufferCommandImpl[F](chs, timeout)

    override def circularBufferStatus: VerifiedEpics[F, F, CircularBufferStatus] = for {
      imF <- readChannel(chs.telltale, chs.imEnabled).map(_.map(_ === "TRUE"))
      aoF <- readChannel(chs.telltale, chs.aoEnabled).map(_.map(_ === "TRUE"))
      fgF <- readChannel(chs.telltale, chs.fgEnabled).map(_.map(_ === "TRUE"))
    } yield for {
      im <- imF
      ao <- aoF
      fg <- fgF
    } yield CircularBufferStatus(im, ao, fg)

    override def imgCircularBufferStrean: VerifiedEpics[F, Resource[F, *], Stream[F, Boolean]] =
      VerifiedEpics
        .eventStream(chs.telltale, chs.imEnabled)
        .map(
          _.map(
            _.collect { case StreamEvent.ValueChanged(t) =>
              t === "TRUE"
            }
          )
        )
  }

  def build[F[_]: {Async, Parallel, Dispatcher}](
    server:   EpicsService[F],
    top:      NonEmptyString,
    telltale: Either[String, TelltaleChannel[F]]
  ): Resource[F, CircularBufferControl[F]] =
    CircularBufferChannels.build(server, top, telltale).map(buildSystem)

}
