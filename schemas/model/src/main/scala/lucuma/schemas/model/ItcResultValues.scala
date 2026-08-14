// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.itc.SignalToNoiseAt

/**
 * The ITC results we display for one sequence type (or, for imaging, one filter). Corresponds to
 * `ItcResult` in the ODB schema.
 *
 * `peakPixelFlux` is the highest electron count in any single pixel for a single exposure, taken as
 * the maximum across the CCDs.
 */
case class ItcResultValues(
  signalToNoise: Option[SignalToNoiseAt],
  peakPixelFlux: Option[Double]
) derives Eq

object ItcResultValues:
  val Empty: ItcResultValues = ItcResultValues(none, none)

  def fromSignalToNoise(signalToNoise: Option[SignalToNoiseAt]): ItcResultValues =
    ItcResultValues(signalToNoise, none)
