// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.model.enums.Visible
import io.circe.Decoder
import monocle.Focus

case class PFVisibility(
  showBase:              Visible,
  showBlindOffset:       Visible,
  showAcquisitionOffset: Visible,
  showScienceOffset:     Visible,
  showIntersection:      Visible,
  showAllAngles:         Visible
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
    showBase = Visible.Hidden,
    showBlindOffset = Visible.Hidden,
    showAcquisitionOffset = Visible.Hidden,
    showScienceOffset = Visible.Hidden,
    showIntersection = Visible.Hidden,
    showAllAngles = Visible.Hidden
  )
