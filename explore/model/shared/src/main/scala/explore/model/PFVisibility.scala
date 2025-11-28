// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import io.circe.Decoder
import monocle.Focus

case class PFVisibility(
  showBase:              Boolean,
  showBlindOffset:       Boolean,
  showAcquisitionOffset: Boolean,
  showScienceOffset:     Boolean,
  showIntersection:      Boolean,
  showAllAngles:         Boolean
) derives Decoder

object PFVisibility:
  given Eq[PFVisibility] = Eq.fromUniversalEquals

  val showBase              = Focus[PFVisibility](_.showBase)
  val showBlindOffset       = Focus[PFVisibility](_.showBlindOffset)
  val showAcquisitionOffset = Focus[PFVisibility](_.showAcquisitionOffset)
  val showScienceOffset     = Focus[PFVisibility](_.showScienceOffset)
  val showIntersection      = Focus[PFVisibility](_.showIntersection)
  val showAllAngles         = Focus[PFVisibility](_.showAllAngles)

  val Default = PFVisibility(
    showBase = false,
    showBlindOffset = true,
    showAcquisitionOffset = false,
    showScienceOffset = false,
    showIntersection = false,
    showAllAngles = false
  )
