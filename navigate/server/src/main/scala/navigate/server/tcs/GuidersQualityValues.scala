// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Eq
import cats.derived.*

case class GuidersQualityValues(
  pwfs1: GuidersQualityValues.GuiderQuality,
  pwfs2: GuidersQualityValues.GuiderQuality,
  oiwfs: GuidersQualityValues.GuiderQuality
) derives Eq

object GuidersQualityValues {

  case class GuiderQuality(
    flux:             Int,
    centroidDetected: Boolean
  )

  lazy val default: GuidersQualityValues = GuidersQualityValues(
    GuiderQuality(0, false),
    GuiderQuality(0, false),
    GuiderQuality(0, false)
  )

}
