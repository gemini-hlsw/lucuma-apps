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
import lucuma.core.refined.auto.*
import navigate.epics.EpicsService

type PwfsEpicsSystem[F[_]] = WfsEpicsSystem[F] & CircularBufferControl[F]

object PwfsEpicsSystem {
  def build[F[_]: {Temporal, Parallel, Async, Dispatcher}](
    service:       EpicsService[F],
    sysName:       String,
    top:           NonEmptyString,
    gainResetName: NonEmptyString = "dc:initSigInit.J".refined,
    fluxName:      NonEmptyString = "dc:fgDiag1PW.VALQ".refined,
    centroidName:  NonEmptyString = "dc:fgDiag1PW.VALB".refined
  ): Resource[F, PwfsEpicsSystem[F]] = for {
    wfs <- WfsEpicsSystem.build(service, sysName, top, gainResetName, fluxName, centroidName)
    cbc <- CircularBufferControl.build(service, top, wfs.telltale.asRight)
  } yield new WfsEpicsSystem[F] with CircularBufferControl[F] {
    export wfs.*
    export cbc.*
  }
}
