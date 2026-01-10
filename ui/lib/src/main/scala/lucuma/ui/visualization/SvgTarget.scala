// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.SequenceType
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.Css

sealed trait SvgTarget derives Eq:
  def coordinates: Coordinates
  def css: Css

sealed trait SelectableProgramTarget extends SvgTarget:
  def selected: Boolean
  def selectedCss: Css

object SvgTarget:
  final case class CircleTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class CrosshairTarget(
    coordinates: Coordinates,
    css:         Css,
    side:        Double,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class ScienceTarget(
    coordinates: Coordinates,
    css:         Css,
    selectedCss: Css,
    side:        Double,
    selected:    Boolean,
    title:       Option[String] = None
  ) extends SelectableProgramTarget derives Eq

  final case class LineTo(
    coordinates: Coordinates,
    to:          Coordinates,
    css:         Css,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class GuideStarCandidateTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    analysis:    AgsAnalysis.Usable,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class GuideStarTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    analysis:    AgsAnalysis.Usable,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class OffsetIndicator(
    coordinates: Coordinates,
    pos:         NonNegInt,
    offset:      Offset,
    oType:       SequenceType,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SvgTarget derives Eq

  final case class BlindOffsetTarget(
    coordinates: Coordinates,
    css:         Css,
    selectedCss: Css,
    radius:      Double,
    selected:    Boolean,
    title:       Option[String] = None
  ) extends SelectableProgramTarget derives Eq

  given Reusability[SvgTarget] = Reusability.byEq
