// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.option.*
import explore.model.AsterismVisualOptions
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.util.NewBoolean
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.aladin.Fov
import lucuma.ui.reusability.given
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document

given Reusability[AgsAnalysis.Usable] =
  Reusability.by(u => (u.target, u.posAngle))

object AreAdding extends NewBoolean
type AreAdding = AreAdding.Type

// The `SpectralDefinition*Input`s are `@oneOf`s, so `Aligner` drills into them through a
// `Prism`. The base input we start from must therefore be of the same kind as the current
// spectral definition, otherwise the prism doesn't match and the edit is silently dropped,
// resulting in an empty (and invalid) edit of the wrong kind being sent to the ODB.
private def emptySpectralDefinitionIntegratedInput(
  sd: SpectralDefinition[Integrated]
): SpectralDefinitionIntegratedInput =
  sd match
    case SpectralDefinition.BandNormalized(_, _) =>
      SpectralDefinitionIntegratedInput.BandNormalized(BandNormalizedIntegratedInput())
    case SpectralDefinition.EmissionLines(_, _)  =>
      SpectralDefinitionIntegratedInput.EmissionLines(EmissionLinesIntegratedInput())

private def emptySpectralDefinitionSurfaceInput(
  sd: SpectralDefinition[Surface]
): SpectralDefinitionSurfaceInput =
  sd match
    case SpectralDefinition.BandNormalized(_, _) =>
      SpectralDefinitionSurfaceInput.BandNormalized(BandNormalizedSurfaceInput())
    case SpectralDefinition.EmissionLines(_, _)  =>
      SpectralDefinitionSurfaceInput.EmissionLines(EmissionLinesSurfaceInput())

extension (options: AsterismVisualOptions) inline def fov: Fov = Fov(options.fovRA, options.fovDec)

extension (fov: Fov)
  def isDifferentEnough(newFov: Fov): Boolean =
    (fov.x.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e7 ||
      (fov.y.toMicroarcseconds - newFov.y.toMicroarcseconds).abs < 1e7

val domRoot: Option[HTMLElement] =
  Option(document.querySelector(":root")) match
    case Some(r: HTMLElement) => r.some
    case _                    => none

def setVariable(root: Option[HTMLElement], variableName: String, value: Int): Callback =
  root
    .map: root =>
      Callback(root.style.setProperty(s"--aladin-image-$variableName", s"${value / 100.0}"))
    .getOrEmpty
