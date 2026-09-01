// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.CustomizableEnumSelect
import explore.components.CustomizableInputText
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.InputValidWedge
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.given
import monocle.Lens

/**
 * How the ITC samples the IFU field. `GmosIfuAnalysis` is a `@oneOf`, so the shape is picked first
 * and then its single angle edited; switching shape keeps the angle, which is the value the
 * observer was just looking at.
 */
enum GmosIfuAnalysisKind(val tag: String, val name: String) derives Enumerated, Display:
  case Sum    extends GmosIfuAnalysisKind("Sum", "Summed")
  case Single extends GmosIfuAnalysisKind("Single", "Single element")

object GmosIfuAnalysisKind:
  def fromGmosIfuAnalysis(analysis: GmosIfuAnalysis): GmosIfuAnalysisKind =
    analysis match
      case GmosIfuAnalysis.Sum(_)    => GmosIfuAnalysisKind.Sum
      case GmosIfuAnalysis.Single(_) => GmosIfuAnalysisKind.Single

final case class GmosIfuAnalysisEditor(
  analysis:                 View[GmosIfuAnalysis],
  default:                  GmosIfuAnalysis,
  readonly:                 Boolean,
  showCustomization:        Boolean,
  allowRevertCustomization: Boolean
) extends ReactFnProps(GmosIfuAnalysisEditor)

object GmosIfuAnalysisEditor
    extends ReactFnComponent[GmosIfuAnalysisEditor](props =>
      // The angle survives a shape change: it is the number the observer is looking at.
      val angleOf: GmosIfuAnalysis => Angle =
        case GmosIfuAnalysis.Sum(radius)    => radius
        case GmosIfuAnalysis.Single(offset) => offset

      // Zero is a legal offset -- the element on the field centre -- but not a legal radius, so
      // it is the one angle that cannot cross into a Sum; carry the default across instead.
      val asSumRadius: Angle => Angle = a =>
        if Angle.signedMicroarcseconds.get(a) > 0 then a else GmosIfuAnalysis.DefaultSumRadius

      val kindLens: Lens[GmosIfuAnalysis, GmosIfuAnalysisKind] =
        Lens(GmosIfuAnalysisKind.fromGmosIfuAnalysis): k =>
          a =>
            k match
              case GmosIfuAnalysisKind.Sum    => GmosIfuAnalysis.Sum(asSumRadius(angleOf(a)))
              case GmosIfuAnalysisKind.Single => GmosIfuAnalysis.Single(angleOf(a))

      val angleLens: Lens[GmosIfuAnalysis, Angle] =
        Lens(angleOf): angle =>
          case GmosIfuAnalysis.Sum(_)    => GmosIfuAnalysis.Sum(angle)
          case GmosIfuAnalysis.Single(_) => GmosIfuAnalysis.Single(angle)

      val kind: View[GmosIfuAnalysisKind] = props.analysis.zoom(kindLens)
      val angle: View[Angle]              = props.analysis.zoom(angleLens)

      val angleLabel: String = kind.get match
        case GmosIfuAnalysisKind.Sum    => "Radius"
        case GmosIfuAnalysisKind.Single => "Offset"

      // A radius of zero encloses no elements, but an offset of zero is the element on the field
      // centre -- the OCS default -- so only the radius is barred from being zero.
      val angleFormat: InputValidWedge[Angle] = kind.get match
        case GmosIfuAnalysisKind.Sum    => ExploreModelValidators.positiveDecimalArcsecondsValidWedge
        case GmosIfuAnalysisKind.Single =>
          ExploreModelValidators.nonNegativeDecimalArcsecondsValidWedge

      // Each control contributes its own label/value pair to the panel's two-column grid;
      // wrapping them together collapses both into one cell.
      React.Fragment(
        CustomizableEnumSelect(
          id = "ifu-analysis-kind".refined,
          view = kind,
          defaultValue = GmosIfuAnalysisKind.fromGmosIfuAnalysis(props.default),
          label = "IFU Analysis".some,
          helpId = Some("configuration/gmos/ifu-analysis.md".refined),
          disabled = props.readonly,
          showCustomization = props.showCustomization,
          allowRevertCustomization = props.allowRevertCustomization
        ),
        <.div, // Empty div for the labels column of the grid.
        <.div(ExploreStyles.GmosIfuAnalysisAngle)(
          CustomizableInputText(
            id = "ifu-analysis-angle".refined,
            value = angle,
            validFormat = angleFormat,
            // `posBigDecimal` denies the minus sign outright, so neither shape can be typed
            // negative; the validator still rejects a zero radius, which is legal to type.
            changeAuditor = ChangeAuditor.bigDecimal(3.refined, 2.refined).denyNeg,
            // Indented to read as a property of the analysis above rather than its own setting.
            label = angleLabel,
            defaultValue = angleOf(props.default),
            units = "\"".some,
            disabled = props.readonly,
            showCustomization = props.showCustomization,
            allowRevertCustomization = props.allowRevertCustomization
          )
        )
      )
    )
