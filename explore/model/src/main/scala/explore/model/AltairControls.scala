// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.AltairMode
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.util.Display
import lucuma.itc.AltairParameters
import lucuma.odb.data.AltairConfiguration
import lucuma.ui.display.given

import java.time.Instant

// How the Altair configuration controls behave for each mode.
object AltairControls:

  /**
   * The angular distance between the observation base and the guide star, both at `at`. None until
   * both are known.
   */
  def guideStarSeparation(
    base:      Option[Coordinates],
    guideStar: Option[GuideStarCandidate],
    at:        Instant
  ): Option[Angle] =
    (base, guideStar.flatMap(_.tracking.at(at))).mapN(_.angularDistance(_))

  /**
   * What the ITC gets of Altair. LGS+P1 needs no guide star; NGS and LGS need the selected star's
   * separation and R brightness, and are None until both are known.
   */
  def itcParameters(
    altair:              AltairConfiguration,
    guideStar:           Option[GuideStarCandidate],
    guideStarSeparation: Option[Angle]
  ): Option[AltairParameters] =
    altair.mode match
      case AltairMode.LgsP1                =>
        AltairParameters.LgsP1.some
      case AltairMode.Ngs | AltairMode.Lgs =>
        guideStarSeparation.flatMap(altair.itcParameters(_, guideStar.flatMap(_.rBrightness)))

  // With Altair, the ITC tiles hold their requests until the parameters are known.
  def itcAwaitingGuideStar(
    altair:     Option[AltairConfiguration],
    parameters: Option[AltairParameters]
  ): Boolean =
    altair.isDefined && parameters.isEmpty

  val ItcAwaitingGuideStarMessage: String = "Waiting for the Altair guide star"

  // The laser modes always use the field lens, so there is nothing to choose.
  def fieldLensSelectable(mode: AltairMode): Boolean =
    !mode.usesLaser

  // The ODB forces the ND filter out for the laser modes.
  def ndFilterLocked(mode: AltairMode): Boolean =
    mode.usesLaser

  /** What the NGS field lens selector shows when no explicit position is chosen. */
  def fieldLensPlaceholder(guideStarSeparation: Option[Angle]): String =
    AltairConfiguration
      .defaultFieldLens(AltairMode.Ngs, guideStarSeparation)
      .fold("Auto")(fieldLens => s"Auto (${Display[FieldLens].shortName(fieldLens)})")
