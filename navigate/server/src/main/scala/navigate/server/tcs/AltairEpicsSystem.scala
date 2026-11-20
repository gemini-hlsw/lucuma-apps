// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.MonadThrow
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.AltairNdFilter
import mouse.boolean.*
import navigate.epics.EpicsService
import navigate.epics.VerifiedEpics.*
import navigate.server.ApplyCommandResult
import navigate.server.acm.CarState
import navigate.server.acm.TaskControlCommand
import navigate.server.acm.writeCadParam
import navigate.server.epicsdata.BinaryOnOffCapitalized

import scala.concurrent.duration.FiniteDuration

trait AltairEpicsSystem[F[_]] {
  val status: AltairEpicsSystem.AltairStatus[F]
  val commands: AltairEpicsSystem.AltairCommands[F]
}

object AltairEpicsSystem {

  trait AltairStatus[F[_]] {

    /** Is Altair set up to use the laser wavefront sensor (LGS)? */
    def usingLaserWfs: VerifiedEpics[F, F, Boolean]

    /** Is the LGS U stage in? */
    def uStageIn: VerifiedEpics[F, F, Boolean]

    /** Is the LGS zoom tracking? */
    def zoomTracking: VerifiedEpics[F, F, Boolean]

    /** Is the LGS zoom moving? */
    def zoomBusy: VerifiedEpics[F, F, Boolean]

    /** Is the LGS zoom in position? */
    def zoomInPosition: VerifiedEpics[F, F, Boolean]

    /** Is the AOWFS probe moving, or following? This includes the gimbals. */
    def wfsPositionBusy: VerifiedEpics[F, F, Boolean]

    /** Is Altair, at its top level, busy? */
    def busy: VerifiedEpics[F, F, Boolean]
  }

  /**
   * Altair commands. `useLaserWfs` and `initWfs` are taskControl records, and the ND filters are
   * assemblyControl records, which behave the same way: they end when the record goes back to idle,
   * or fail if it reports an error.
   */
  trait AltairCommands[F[_]] {

    /** Switch between the natural and laser wavefront sensors */
    def useLaserWfs(
      enable:  Boolean,
      timeout: FiniteDuration
    ): VerifiedEpics[F, F, ApplyCommandResult]

    /** Initialize the wavefront sensor, laser or natural */
    def initWfs(laser: Boolean, timeout: FiniteDuration): VerifiedEpics[F, F, ApplyCommandResult]

    /** Move the neutral density filter of the natural guide star wavefront sensor */
    def ngsNdFilter(
      position: AltairNdFilter,
      timeout:  FiniteDuration
    ): VerifiedEpics[F, F, ApplyCommandResult]

    /** Move the neutral density filter of the laser guide star wavefront sensor */
    def lgsNdFilter(
      position: AltairNdFilter,
      timeout:  FiniteDuration
    ): VerifiedEpics[F, F, ApplyCommandResult]
  }

  // Value of the MODE parameter of the assemblies that makes them move to the demanded position
  private val AssemblyMoveMode: String = "1"

  private def encodeNdFilter(f: AltairNdFilter): String = f match {
    case AltairNdFilter.In  => "IN"
    case AltairNdFilter.Out => "OUT"
  }

  private[tcs] def buildSystem[F[_]: MonadThrow](
    ch:        AltairChannels[F],
    useLwfs:   TaskControlCommand[F],
    wfsInit:   TaskControlCommand[F],
    ngsNdFilt: TaskControlCommand[F],
    lgsNdFilt: TaskControlCommand[F]
  ): AltairEpicsSystem[F] = new AltairEpicsSystem[F] {

    private def read[A](c: navigate.epics.Channel[F, A]): VerifiedEpics[F, F, A] =
      readChannel(ch.telltale, c)

    override val status: AltairStatus[F] = new AltairStatus[F] {
      override def usingLaserWfs: VerifiedEpics[F, F, Boolean] =
        read(ch.useLwfs).map(_.map(_.equalsIgnoreCase("ON")))

      override def uStageIn: VerifiedEpics[F, F, Boolean] =
        read(ch.uStage).map(_.map(_.equalsIgnoreCase("IN")))

      override def zoomTracking: VerifiedEpics[F, F, Boolean] =
        read(ch.zoomMode).map(_.map(_.equalsIgnoreCase("TRACK")))

      override def zoomBusy: VerifiedEpics[F, F, Boolean] =
        read(ch.zoomBusy).map(_.map(_.equalsIgnoreCase("BUSY")))

      override def zoomInPosition: VerifiedEpics[F, F, Boolean] =
        read(ch.zoomInPosition).map(_.map(_ =!= 0))

      override def wfsPositionBusy: VerifiedEpics[F, F, Boolean] =
        read(ch.wfsPositionBusy).map(_.map(_ === CarState.BUSY))

      override def busy: VerifiedEpics[F, F, Boolean] =
        read(ch.gaosBusy).map(_.map(_ === CarState.BUSY))
    }

    override val commands: AltairCommands[F] = new AltairCommands[F] {
      override def useLaserWfs(
        enable:  Boolean,
        timeout: FiniteDuration
      ): VerifiedEpics[F, F, ApplyCommandResult] =
        useLwfs.post(
          timeout,
          writeCadParam(ch.telltale, ch.useLwfsParam)(
            enable.fold(BinaryOnOffCapitalized.On, BinaryOnOffCapitalized.Off)
          )
        )

      override def initWfs(
        laser:   Boolean,
        timeout: FiniteDuration
      ): VerifiedEpics[F, F, ApplyCommandResult] =
        wfsInit.post(
          timeout,
          writeCadParam(ch.telltale, ch.wfsInitParam)(laser.fold(1, 0))
        )

      private def moveNdFilter(
        cmd:      TaskControlCommand[F],
        params:   AltairChannels.NdFilterParamChannels[F],
        position: AltairNdFilter,
        timeout:  FiniteDuration
      ): VerifiedEpics[F, F, ApplyCommandResult] =
        cmd.post(
          timeout,
          writeCadParam(ch.telltale, params.position)(encodeNdFilter(position)) *>
            writeCadParam(ch.telltale, params.mode)(AssemblyMoveMode)
        )

      override def ngsNdFilter(
        position: AltairNdFilter,
        timeout:  FiniteDuration
      ): VerifiedEpics[F, F, ApplyCommandResult] =
        moveNdFilter(ngsNdFilt, ch.ngsNdFilterParams, position, timeout)

      override def lgsNdFilter(
        position: AltairNdFilter,
        timeout:  FiniteDuration
      ): VerifiedEpics[F, F, ApplyCommandResult] =
        moveNdFilter(lgsNdFilt, ch.lgsNdFilterParams, position, timeout)
    }
  }

  def build[F[_]: {Dispatcher, Temporal}](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, AltairEpicsSystem[F]] = for {
    ch  <- AltairChannels.build(service, top)
    ul  <- TaskControlCommand.build(
             service,
             ch.telltale,
             s"${top.value}${AltairChannels.UseLwfsRecord}"
           )
    wi  <- TaskControlCommand.build(
             service,
             ch.telltale,
             s"${top.value}${AltairChannels.WfsInitRecord}"
           )
    ngs <- TaskControlCommand.build(
             service,
             ch.telltale,
             s"${top.value}${AltairChannels.NgsNdFilterRecord}"
           )
    lgs <- TaskControlCommand.build(
             service,
             ch.telltale,
             s"${top.value}${AltairChannels.LgsNdFilterRecord}"
           )
  } yield buildSystem(ch, ul, wi, ngs, lgs)

}
