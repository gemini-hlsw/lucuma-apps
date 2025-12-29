// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.model.enums.Visible
import io.circe.Decoder
import monocle.Focus

case class AGSVisibility(
  showBase:              Visible,
  showBlindOffset:       Visible,
  showAcquisitionOffset: Visible,
  showScienceOffset:     Visible,
  showIntersection:      Visible,
  showAllAngles:         Visible
) derives Decoder

object AGSVisibility:
  given Eq[AGSVisibility] = Eq.fromUniversalEquals

  val showBase          = Focus[AGSVisibility](_.showBase)
  val showBlindOffset   = Focus[AGSVisibility](_.showBlindOffset)
  val showAcqOffset     = Focus[AGSVisibility](_.showAcquisitionOffset)
  val showScienceOffset = Focus[AGSVisibility](_.showScienceOffset)
  val showIntersection  = Focus[AGSVisibility](_.showIntersection)
  val showAllAngles     = Focus[AGSVisibility](_.showAllAngles)

  val Default = AGSVisibility(
    showBase = Visible.Hidden,
    showBlindOffset = Visible.Hidden,
    showAcquisitionOffset = Visible.Hidden,
    showScienceOffset = Visible.Hidden,
    showIntersection = Visible.Hidden,
    showAllAngles = Visible.Hidden
  )
