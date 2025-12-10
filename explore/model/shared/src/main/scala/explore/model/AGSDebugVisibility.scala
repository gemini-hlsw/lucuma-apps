// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.model.enums.Visible
import io.circe.Decoder
import monocle.Focus

case class AGSDebugVisibility(
  showBase:              Visible,
  showBlindOffset:       Visible,
  showAcquisitionOffset: Visible,
  showScienceOffset:     Visible,
  showIntersection:      Visible,
  showAllAngles:         Visible
) derives Decoder

object AGSDebugVisibility:
  given Eq[AGSDebugVisibility] = Eq.fromUniversalEquals

  val showBase              = Focus[AGSDebugVisibility](_.showBase)
  val showBlindOffset       = Focus[AGSDebugVisibility](_.showBlindOffset)
  val showAcquisitionOffset = Focus[AGSDebugVisibility](_.showAcquisitionOffset)
  val showScienceOffset     = Focus[AGSDebugVisibility](_.showScienceOffset)
  val showIntersection      = Focus[AGSDebugVisibility](_.showIntersection)
  val showAllAngles         = Focus[AGSDebugVisibility](_.showAllAngles)
  val showAllCatalogStars   = Focus[AGSDebugVisibility](_.showAllCatalogStars)

  val Default = AGSDebugVisibility(
    showBase = Visible.Hidden,
    showBlindOffset = Visible.Hidden,
    showAcquisitionOffset = Visible.Hidden,
    showScienceOffset = Visible.Hidden,
    showIntersection = Visible.Hidden,
    showAllAngles = Visible.Hidden,
    showAllCatalogStars = Visible.Shown
  )
