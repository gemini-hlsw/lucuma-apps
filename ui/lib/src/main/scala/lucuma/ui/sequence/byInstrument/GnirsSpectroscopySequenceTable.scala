// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.schemas.model.GnirsCentralWavelengthItcResult
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.PeakPixel

trait GnirsSpectroscopySequenceTable:
  def acquisitionItc: ItcResultValues
  def scienceItc: List[GnirsCentralWavelengthItcResult]

  // A wavelength may repeat in the list, so more than one entry can match a step. The atom
  // description's occurrence ordinal picks among them when possible (see `forScienceStep`); entries
  // differing only in the "S/N at" wavelength are otherwise indistinguishable from the step alone.
  // A value is shown only when every remaining candidate carries it and they all agree.
  private def agreed[A: Eq](occurrence: Option[Int], d: GnirsDynamicConfig)(
    f: ItcResultValues => Option[A]
  ): Option[A] =
    GnirsCentralWavelengthItcResult
      .forScienceStep(scienceItc, occurrence, d)
      .map(_.values)
      .traverse(f)
      .flatMap:
        case Nil          => none
        case head :: tail => Option.when(tail.forall(_ === head))(head)

  def signalToNoise
    : SequenceType => Option[NonEmptyString] => GnirsDynamicConfig => Option[SignalToNoise] =
    // GNIRS acquisition repeats are coadds, so the total S/N is what one acquisition step delivers.
    case SequenceType.Acquisition => _ => _ => acquisitionItc.signalToNoise.map(_.total.value)
    case SequenceType.Science     =>
      desc =>
        val occurrence = GnirsCentralWavelengthItcResult.occurrence(desc)
        d =>
          agreed(occurrence, d)(_.signalToNoise.map(sn => (sn.wavelength, sn.single.value)))
            .map(_._2)

  def peakPixel: SequenceType => Option[NonEmptyString] => GnirsDynamicConfig => Option[PeakPixel] =
    case SequenceType.Acquisition => _ => _ => acquisitionItc.peakPixel
    case SequenceType.Science     =>
      desc =>
        val occurrence = GnirsCentralWavelengthItcResult.occurrence(desc)
        d => agreed(occurrence, d)(_.peakPixel)
