// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.itc.SignalToNoiseAt

/**
 * The brightest pixel the ITC found for a single exposure, taken across the CCDs it reported.
 * Corresponds to `ItcPeakPixel` in the ODB schema.
 */
case class PeakPixel(
  flux: Double,
  adu:  Int
) derives Eq

/**
 * The ITC results we display for one sequence type (or, for imaging, one filter). Corresponds to
 * `ItcResult` in the ODB schema.
 */
case class ItcResultValues(
  signalToNoise: Option[SignalToNoiseAt],
  peakPixel:     Option[PeakPixel]
) derives Eq

object ItcResultValues:
  val Empty: ItcResultValues = ItcResultValues(none, none)

  def fromSignalToNoise(signalToNoise: Option[SignalToNoiseAt]): ItcResultValues =
    ItcResultValues(signalToNoise, none)
