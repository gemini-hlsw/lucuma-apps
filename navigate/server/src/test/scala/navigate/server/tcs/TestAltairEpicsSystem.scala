// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.MonadThrow
import cats.effect.Ref
import cats.syntax.all.*
import lucuma.core.enums.AltairNdFilter
import monocle.Focus
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.server.ApplyCommandResult

import scala.concurrent.duration.FiniteDuration

object TestAltairEpicsSystem {

  /**
   * The statuses are what Altair reports, and the commands are recorded in the order they are
   * executed.
   */
  case class State(
    usingLaserWfs:  Boolean,
    uStageIn:       Boolean,
    zoomTracking:   Boolean,
    zoomBusy:       Boolean,
    zoomInPosition: Boolean,
    useLaserWfs:    Option[Boolean],
    initWfs:        Option[Boolean],
    ngsNdFilter:    Option[AltairNdFilter],
    lgsNdFilter:    Option[AltairNdFilter],
    commands:       List[String]
  )

  val defaultState: State = State(
    usingLaserWfs = false,
    uStageIn = false,
    zoomTracking = false,
    zoomBusy = false,
    zoomInPosition = false,
    useLaserWfs = none,
    initWfs = none,
    ngsNdFilter = none,
    lgsNdFilter = none,
    commands = Nil
  )

  def build[F[_]: MonadThrow](s: Ref[F, State]): AltairEpicsSystem[F] =
    new AltairEpicsSystem[F] {
      private def record[A](
        name: String
      )(f: State => State): VerifiedEpics[F, F, ApplyCommandResult] =
        VerifiedEpics.liftF(
          s.update(f.andThen(Focus[State](_.commands).modify(_ :+ name)))
            .as(ApplyCommandResult.Completed: ApplyCommandResult)
        )

      override val status: AltairEpicsSystem.AltairStatus[F] =
        new AltairEpicsSystem.AltairStatus[F] {
          override def usingLaserWfs: VerifiedEpics[F, F, Boolean]  =
            VerifiedEpics.liftF(s.get.map(_.usingLaserWfs))
          override def uStageIn: VerifiedEpics[F, F, Boolean]       =
            VerifiedEpics.liftF(s.get.map(_.uStageIn))
          override def zoomTracking: VerifiedEpics[F, F, Boolean]   =
            VerifiedEpics.liftF(s.get.map(_.zoomTracking))
          override def zoomBusy: VerifiedEpics[F, F, Boolean]       =
            VerifiedEpics.liftF(s.get.map(_.zoomBusy))
          override def zoomInPosition: VerifiedEpics[F, F, Boolean] =
            VerifiedEpics.liftF(s.get.map(_.zoomInPosition))
        }

      override val commands: AltairEpicsSystem.AltairCommands[F] =
        new AltairEpicsSystem.AltairCommands[F] {
          override def useLaserWfs(
            enable:  Boolean,
            timeout: FiniteDuration
          ): VerifiedEpics[F, F, ApplyCommandResult] =
            record("useLaserWfs")(Focus[State](_.useLaserWfs).replace(enable.some))

          override def initWfs(
            laser:   Boolean,
            timeout: FiniteDuration
          ): VerifiedEpics[F, F, ApplyCommandResult] =
            record("initWfs")(Focus[State](_.initWfs).replace(laser.some))

          override def ngsNdFilter(
            position: AltairNdFilter,
            timeout:  FiniteDuration
          ): VerifiedEpics[F, F, ApplyCommandResult] =
            record("ngsNdFilter")(Focus[State](_.ngsNdFilter).replace(position.some))

          override def lgsNdFilter(
            position: AltairNdFilter,
            timeout:  FiniteDuration
          ): VerifiedEpics[F, F, ApplyCommandResult] =
            record("lgsNdFilter")(Focus[State](_.lgsNdFilter).replace(position.some))
        }
    }
}
