// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.display.altairModeLabel
import explore.model.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.FieldLens
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.probes
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.odb.data.AltairConfiguration
import lucuma.ui.display.given

// An entry of the guider selector: a plain guide probe, or observing behind Altair in some mode.
enum GuiderChoice derives Eq:
  case Probe(probe: GuideProbe)
  case Altair(mode: AltairMode)

object GuiderChoice:
  given Display[GuiderChoice] = Display.byShortName:
    case Probe(probe = probe) => Display[GuideProbe].shortName(probe)
    case Altair(mode = mode)  => altairModeLabel(mode)

  // The probes the mode allows without Altair, best-first, then the Altair modes if supported.
  def options(mode: ObservingModeType): List[GuiderChoice] =
    val probeChoices: List[GuiderChoice]  = probes.allowedProbes(mode, none).toList.map(Probe(_))
    val altairChoices: List[GuiderChoice] =
      if mode.supportsAltair then Enumerated[AltairMode].all.map(Altair(_)) else List.empty
    probeChoices ++ altairChoices

  def current(guiding: GuidingConfiguration): Option[GuiderChoice] =
    guiding.altair
      .map(configuration => Altair(configuration.mode))
      .orElse(guiding.explicitGuideProbe.map(Probe(_)))

  /**
   * The guiding configuration that results from picking `choice`. Picking an Altair mode keeps the
   * rest of an existing Altair configuration, except for the settings the laser modes cannot use,
   * which the ODB would reject.
   */
  def select(
    choice: Option[GuiderChoice],
    altair: Option[AltairConfiguration]
  ): GuidingConfiguration =
    choice match
      case Some(Probe(probe = probe)) => GuidingConfiguration(probe.some, none)
      case Some(Altair(mode = mode))  =>
        val configuration: AltairConfiguration =
          altair.fold(AltairConfiguration.default(mode))(AltairConfiguration.mode.replace(mode))
        GuidingConfiguration(none, forMode(configuration).some)
      case None                       => GuidingConfiguration.Empty

  private def forMode(configuration: AltairConfiguration): AltairConfiguration =
    if configuration.mode.usesLaser then
      configuration.copy(
        explicitFieldLens = configuration.explicitFieldLens.filterNot(_ === FieldLens.Out),
        ndFilter = AltairNdFilter.Out
      )
    else configuration
