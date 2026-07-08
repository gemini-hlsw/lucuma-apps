// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import navigate.model.enums.{FollowStatus, ParkStatus}
import navigate.model.enums.FollowStatus.NotFollowing
import navigate.model.enums.ParkStatus.Parked

case class MechSystemState(
  parked:    ParkStatus,
  following: FollowStatus
) derives Eq

object MechSystemState {
  val default: MechSystemState = MechSystemState(Parked, NotFollowing)
}
