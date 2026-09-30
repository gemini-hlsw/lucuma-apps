// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.util.TimeSpan

/**
 * The ITC result for one entry of a GNIRS spectroscopy observation's central wavelength list.
 * Entries are matched to science steps by central wavelength, exposure time and coadds; a
 * wavelength may appear more than once in the list. Corresponds to `ItcGnirsSpectroscopyResultSet`
 * in the ODB schema.
 */
case class GnirsCentralWavelengthItcResult(
  centralWavelength: Wavelength,
  exposureTime:      TimeSpan,
  coadds:            PosInt,
  values:            ItcResultValues
) derives Eq

object GnirsCentralWavelengthItcResult:
  // The ODB suffixes atom titles with the wavelength and a 1-based occurrence ordinal when a
  // central wavelength repeats, e.g. "Science Cycle (2200 nm #2)".
  private val OccurrenceSuffix = """#(\d+)\)?\s*$""".r

  private def occurrence(atomDescription: Option[NonEmptyString]): Option[Int] =
    atomDescription
      .flatMap(d => OccurrenceSuffix.findFirstMatchIn(d.value))
      .flatMap(_.group(1).toIntOption)

  /**
   * The ITC results that may have produced a science step, in list order. A step is matched by
   * central wavelength, exposure time and coadds. Since a wavelength may repeat in the list with an
   * otherwise identical configuration, several entries can match; then the atom description's
   * occurrence ordinal picks the n-th entry at that wavelength, provided it is one of the matches.
   * If no ordinal is present, or it points elsewhere, all matches are returned.
   */
  def forScienceStep(
    results:         List[GnirsCentralWavelengthItcResult],
    atomDescription: Option[NonEmptyString],
    config:          GnirsDynamicConfig
  ): List[GnirsCentralWavelengthItcResult] =
    val sameWavelength = results.filter(_.centralWavelength === config.centralWavelength)
    val sameTriple     =
      sameWavelength.filter(r => r.exposureTime === config.exposure && r.coadds === config.coadds)
    sameTriple match
      case _ :: _ :: _ =>
        occurrence(atomDescription)
          .flatMap(n => sameWavelength.lift(n - 1))
          .filter(r => sameTriple.exists(_ === r))
          .fold(sameTriple)(List(_))
      case _           => sameTriple
