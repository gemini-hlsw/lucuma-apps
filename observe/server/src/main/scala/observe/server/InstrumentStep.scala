// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import coulomb.Quantity
import coulomb.units.accepted.Millimeter
import lucuma.core.enums.LightSinkName
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import observe.model.SystemOverrides
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient

import java.time.temporal.ChronoUnit

trait InstrumentStep[F[_]] extends InstrumentGuide {
  def stepType: StepKind
  // SeqTranslate.calcStepType(instrument, stepConfig, obsClass)

  override val oiOffsetGuideThreshold: Option[Quantity[Double, Millimeter]] = None

  // The name used for this instrument in the science fold configuration
  def sfName: LightSinkName

  def defocusB: Option[Length] = None

  def centralWavelength: Option[Wavelength] // = None

  def observeTimeout: TimeSpan = TimeSpan.unsafeFromDuration(1, ChronoUnit.MINUTES)

  def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F]

  def instrumentHeader(client: KeywordsClient[F]): Header[F]

}
