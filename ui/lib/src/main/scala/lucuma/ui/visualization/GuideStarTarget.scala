// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

case class GuideStarTarget(
  offP:                Double,
  offQ:                Double,
  maxP:                Long,
  radius:              Double,
  pointCss:            Css,
  analysis:            AgsAnalysis,
  reachableAtOtherPAs: Boolean  // derived from usableAt.isDefined
) extends ReactFnProps(GuideStarTarget)

object GuideStarTarget
    extends ReactFnComponent[GuideStarTarget](p =>
      val analysisCss: Css =
        if p.reachableAtOtherPAs then VisualizationStyles.CatalogStarReachableAtOtherPAs
        else
          p.analysis match
            case _: AgsAnalysis.Usable                 => VisualizationStyles.CatalogStarUsable
            case _: AgsAnalysis.NotReachableAtPosition => VisualizationStyles.CatalogStarNotReachable
            case _: AgsAnalysis.VignettesScience       => VisualizationStyles.CatalogStarVignetted
            case _: AgsAnalysis.MagnitudeTooFaint      => VisualizationStyles.CatalogStarTooFaint
            case _: AgsAnalysis.MagnitudeTooBright     => VisualizationStyles.CatalogStarTooBright
            case _                                     => VisualizationStyles.CatalogStarOther

      val pointCss: Css =
        VisualizationStyles.GuideStarCandidateTarget |+| analysisCss |+| p.pointCss

      <.circle(VisualizationStyles.VisualizationTooltipTarget |+| p.analysis.target.selector)(
        ^.cx := scale(p.offP),
        ^.cy := scale(p.offQ),
        ^.r  := scale(p.maxP * p.radius),
        pointCss
      )
    )
