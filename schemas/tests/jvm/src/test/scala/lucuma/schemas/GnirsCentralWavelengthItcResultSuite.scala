// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsDynamicConfig.given
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.SignalToNoiseAt
import lucuma.schemas.model.GnirsCentralWavelengthItcResult
import lucuma.schemas.model.ItcResultValues
import munit.FunSuite
import org.scalacheck.Arbitrary.arbitrary

class GnirsCentralWavelengthItcResultSuite extends FunSuite:

  private def nm(n: Int): Wavelength =
    Wavelength.intPicometers.getOption(n * 1000).get

  private val coadds1: PosInt = PosInt.unsafeFrom(1)

  private val step: GnirsDynamicConfig =
    arbitrary[GnirsDynamicConfig].sample.get.copy(
      exposure = 30.secondTimeSpan,
      coadds = coadds1,
      acquisitionMirror = GnirsAcquisitionMirrorMode.Out(
        GnirsPrism.Mirror,
        GnirsGrating.D32,
        GnirsGratingWavelength(nm(2200))
      )
    )

  private def snAt(nmAt: Int): ItcResultValues =
    val sn = SignalToNoise.FromBigDecimalExact.getOption(BigDecimal(10)).get
    ItcResultValues.fromSignalToNoise(SignalToNoiseAt(nm(nmAt), SingleSN(sn), TotalSN(sn)).some)

  private def entry(
    centralNm: Int,
    exposure:  TimeSpan,
    snAtNm:    Int
  ): GnirsCentralWavelengthItcResult =
    GnirsCentralWavelengthItcResult(nm(centralNm), exposure, coadds1, snAt(snAtNm))

  private def occ(s: String): Option[Int] =
    GnirsCentralWavelengthItcResult.occurrence(NonEmptyString.from(s).toOption)

  private val other  = entry(2100, 30.secondTimeSpan, 2100)
  private val first  = entry(2200, 30.secondTimeSpan, 2190)
  private val second = entry(2200, 30.secondTimeSpan, 2210)

  test("single triple match ignores the ordinal"):
    assertEquals(
      GnirsCentralWavelengthItcResult
        .forScienceStep(List(other, first), occ("Science Cycle (2200 nm #2)"), step),
      List(first)
    )

  test("ordinal picks among identical triples"):
    val results = List(other, first, second)
    assertEquals(
      GnirsCentralWavelengthItcResult
        .forScienceStep(results, occ("Science Cycle (2200 nm #2)"), step),
      List(second)
    )
    assertEquals(
      GnirsCentralWavelengthItcResult
        .forScienceStep(results, occ("Science Cycle (2200 nm #1)"), step),
      List(first)
    )

  test("no ordinal returns all matches"):
    val results = List(other, first, second)
    assertEquals(GnirsCentralWavelengthItcResult.forScienceStep(results, none, step),
                 List(first, second)
    )
    assertEquals(
      GnirsCentralWavelengthItcResult.forScienceStep(results, occ("Science Cycle"), step),
      List(first, second)
    )

  test("ordinal pointing to a different exposure falls back to all matches"):
    val results = List(first, entry(2200, 60.secondTimeSpan, 2200), second)
    assertEquals(
      GnirsCentralWavelengthItcResult
        .forScienceStep(results, occ("Science Cycle (2200 nm #2)"), step),
      List(first, second)
    )

  test("no wavelength match returns Nil"):
    assertEquals(
      GnirsCentralWavelengthItcResult.forScienceStep(List(other), none, step),
      Nil
    )
