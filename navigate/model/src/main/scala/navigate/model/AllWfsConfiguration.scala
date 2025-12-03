// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import monocle.Focus

case class AllWfsConfiguration(
  pwfs1: WfsConfiguration,
  pwfs2: WfsConfiguration,
  oiwfs: WfsConfiguration
)

object AllWfsConfiguration {

  val pwfs1 = Focus[AllWfsConfiguration](_.pwfs1)
  val pwfs2 = Focus[AllWfsConfiguration](_.pwfs2)
  val oiwfs = Focus[AllWfsConfiguration](_.oiwfs)

  val default: AllWfsConfiguration = AllWfsConfiguration(
    pwfs1 = WfsConfiguration.default,
    pwfs2 = WfsConfiguration.default,
    oiwfs = WfsConfiguration.default
  )
}
