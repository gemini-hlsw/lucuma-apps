// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.Band
import lucuma.core.enums.GuideSpeed
import lucuma.core.math.Angle
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.ui.LucumaIcons
import lucuma.ui.syntax.all.given

case class GuideStarTooltip(
  analysis:            AgsAnalysis,
  reachableAtOtherPAs: Boolean,
  exampleUsableAngle:  Option[Angle]
) extends ReactFnProps(GuideStarTooltip)

object GuideStarTooltip
    extends ReactFnComponent[GuideStarTooltip](p =>

      extension (s: String)
        private def toSentenceCase: String =
          s"${s.charAt(0).toUpper}${s.substring(1)}"

      val id = s"Gaia DR3 ${p.analysis.target.id}"

      def speedIcon(guideSpeed: GuideSpeed) = guideSpeed match
        case GuideSpeed.Fast   =>
          LucumaIcons.CircleSmall.withClass(VisualizationStyles.AgsFast)
        case GuideSpeed.Medium =>
          LucumaIcons.CircleSmall.withClass(VisualizationStyles.AgsMedium)
        case GuideSpeed.Slow   =>
          LucumaIcons.CircleSmall.withClass(VisualizationStyles.AgsSlow)

      p.analysis match
        case usable: AgsAnalysis.Usable =>
          val guideSpeedIcon = speedIcon(usable.guideSpeed)
          val speedText      = usable.guideSpeed.tag.toSentenceCase

          <.div(VisualizationStyles.AgsTooltip)(
            <.div(id),
            <.div(
              usable.target.gBrightness.map:
                case (Band.GaiaRP, v) =>
                  React.Fragment("G", <.sub("RP"), f": ${v}%.2f ")
                case (b, v)           => React.Fragment(f"${b.shortName}: ${v}%.2f "),
              guideSpeedIcon,
              speedText
            )
          )
        case other                      =>
          val message =
            if p.reachableAtOtherPAs then
              p.exampleUsableAngle match
                case Some(angle) =>
                  val degrees = angle.toDoubleDegrees
                  f"Reachable at other position angles (e.g., $degrees%.0f°)"
                case None        =>
                  "Reachable at other position angles"
            else other.message(false)
          <.div(VisualizationStyles.AgsTooltip)(
            <.div(id),
            <.div(message)
          )
    )
