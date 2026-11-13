// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import cats.effect.Resource
import navigate.epics.Channel
import navigate.epics.EpicsService
import navigate.epics.given
import navigate.server.epicsdata.DirSuffix

/**
 * The channels used to run a command on a TaskControl or an assemblyControl record, which behave
 * the same way. They have the parameters (A, B, MODE...) and the directive (DIR) of a CAD, but they
 * report their own progress in BUSY, like a CAR, and acknowledge the directive in VAL, like an
 * apply: zero is ACCEPT and any other value is REJECT. The error message is in MESS.
 */
case class TaskControlRecord[F[_]](
  name:  String,
  dir:   Channel[F, CadDirective],
  oval:  Channel[F, Int],
  state: Channel[F, CarState],
  mess:  Channel[F, String]
)

object TaskControlRecord {
  private val ValSuffix   = ".VAL"
  private val StateSuffix = ".BUSY"
  private val MsgSuffix   = ".MESS"

  def build[F[_]](srv: EpicsService[F], recordName: String): Resource[F, TaskControlRecord[F]] =
    for {
      dir <- srv.getChannel[CadDirective](recordName + DirSuffix)
      v   <- srv.getChannel[Int](recordName + ValSuffix)
      st  <- srv.getChannel[CarState](recordName + StateSuffix)
      ms  <- srv.getChannel[String](recordName + MsgSuffix)
    } yield TaskControlRecord(recordName, dir, v, st, ms)
}
