// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The filter on the Altair rows of the modes tables.
 */
enum AdaptiveOpticsFilter(val tag: String, val label: String) derives Enumerated:
  case All   extends AdaptiveOpticsFilter("all", "All")
  case Ngs   extends AdaptiveOpticsFilter("ngs", "NGS")
  case Lgs   extends AdaptiveOpticsFilter("lgs", "LGS")
  case LgsP1 extends AdaptiveOpticsFilter("lgs_p1", "LGS+P1")
  case NoAo  extends AdaptiveOpticsFilter("none", "None")

  def admits(altair: Option[AltairMode]): Boolean =
    this match
      case All   => true
      case NoAo  => altair.isEmpty
      case Ngs   => altair.contains_(AltairMode.Ngs)
      case Lgs   => altair.contains_(AltairMode.Lgs)
      case LgsP1 => altair.contains_(AltairMode.LgsP1)

  /**
   * Whether the filter admits NGS or LGS rows, which only exist once the guide star search is done.
   * NoAo and LgsP1 do not depend on it.
   */
  def awaitsGuideStar: Boolean =
    this match
      case All | Ngs | Lgs => true
      case LgsP1 | NoAo    => false

object AdaptiveOpticsFilter:
  // We use None as the default until AO is supported.
  val Default: AdaptiveOpticsFilter = NoAo

  def forMode(mode: Option[AltairMode]): AdaptiveOpticsFilter =
    mode.fold(Default):
      case AltairMode.Ngs   => Ngs
      case AltairMode.Lgs   => Lgs
      case AltairMode.LgsP1 => LgsP1

  given Display[AdaptiveOpticsFilter] = Display.byShortName(_.label)
