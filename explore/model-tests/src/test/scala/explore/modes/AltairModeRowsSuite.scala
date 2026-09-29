// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.InstrumentConfigAndItcResult
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.itc.AltairParameters
import lucuma.schemas.model.BasicConfiguration
import munit.FunSuite

class AltairModeRowsSuite extends FunSuite:
  private val wavelength: Wavelength = Wavelength.unsafeFromIntPicometers(1_650_000)

  private val separation: Angle = Angle.fromDoubleArcseconds(5.0)

  private val rBrightness: BrightnessValue = BrightnessValue.unsafeFrom(12.5)

  private val ngsParameters: AltairParameters =
    AltairParameters.Ngs(separation, rBrightness, FieldLens.In)

  private val lgsParameters: AltairParameters =
    AltairParameters.Lgs(separation, rBrightness)

  private val gnirsSpectroscopy: ItcInstrumentConfig =
    ItcInstrumentConfig.GnirsSpectroscopy(
      GnirsGrating.D32,
      GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      GnirsFilter.Order4,
      GnirsPrism.Mirror,
      GnirsCamera.ShortBlue,
      ItcInstrumentConfig.PlaceholderEtm,
      none,
      none
    )

  private val gmosNorthSpectroscopy: ItcInstrumentConfig =
    ItcInstrumentConfig.GmosNorthSpectroscopy(
      GmosNorthGrating.B1200_G5301,
      GmosNorthFpu.LongSlit_1_00.some,
      none,
      ItcInstrumentConfig.PlaceholderEtm,
      none,
      none
    )

  private def spectroscopyRow(config: ItcInstrumentConfig): SpectroscopyModeRow =
    SpectroscopyModeRow(
      none,
      config,
      NonEmptyString.unsafeFrom("config"),
      FocalPlane.SingleSlit,
      none,
      ModeAO.NoAO,
      ModeWavelength(wavelength),
      ModeWavelength(wavelength),
      ModeWavelength(wavelength),
      WavelengthDelta(wavelength.pm),
      PosInt.unsafeFrom(1000),
      SlitLength(ModeSlitSize(Angle.fromDoubleArcseconds(10.0))),
      SlitWidth(ModeSlitSize(Angle.fromDoubleArcseconds(0.3))),
      none
    )

  private def gnirsImaging(filter: GnirsFilter): ItcInstrumentConfig =
    ItcInstrumentConfig.GnirsImaging(
      filter,
      GnirsCamera.ShortBlue,
      ItcInstrumentConfig.PlaceholderEtm,
      ItcInstrumentConfig.PlaceholderCoadds,
      none
    )

  private def imagingRow(config: ItcInstrumentConfig): ImagingModeRow =
    ImagingModeRow(none, config, ModeAO.NoAO, Angle.fromDoubleArcseconds(50.0), none, none)

  test("GNIRS rows are followed by an NGS and an LGS copy, never LGS+P1"):
    val expanded = AltairModeRows.expandSpectroscopy(List(spectroscopyRow(gnirsSpectroscopy)))
    assertEquals(expanded.map(_.altair), List(none, AltairMode.Ngs.some, AltairMode.Lgs.some))
    assertEquals(expanded.map(_.instrumentConfig).distinct, List(gnirsSpectroscopy))

  test("rows of instruments without Altair are not duplicated"):
    val rows = List(spectroscopyRow(gmosNorthSpectroscopy))
    assertEquals(AltairModeRows.expandSpectroscopy(rows), rows)

  test("imaging rows expand the same way"):
    val expanded = AltairModeRows.expandImaging(List(imagingRow(gnirsImaging(GnirsFilter.Order4))))
    assertEquals(expanded.map(_.altair), List(none, AltairMode.Ngs.some, AltairMode.Lgs.some))

  test("numbering after the expansion gives every row its own id"):
    val expanded =
      AltairModeRows
        .expandSpectroscopy(
          List(spectroscopyRow(gnirsSpectroscopy), spectroscopyRow(gmosNorthSpectroscopy))
        )
        .zipWithIndex
        .map((r, i) => r.copy(id = i.some))
    assertEquals(expanded.flatMap(_.id).distinct.length, 4)

  test("the Altair mode is appended to the instrument label"):
    val labels =
      AltairModeRows.expandSpectroscopy(List(spectroscopyRow(gnirsSpectroscopy))).map(_.instrumentLabel)
    assertEquals(labels, List("GNIRS SC", "GNIRS SC AO:NGS", "GNIRS SC AO:LGS"))

  test("an Altair row without parameters for its mode is hidden"):
    val row = spectroscopyRow(gnirsSpectroscopy).copy(altair = AltairMode.Lgs.some)
    assertEquals(row.withAltairParameters(Map(AltairMode.Ngs -> ngsParameters)), none)
    assertEquals(row.withAltairParameters(Map.empty), none)

  test("an Altair row gets the parameters of its mode"):
    val row      = spectroscopyRow(gnirsSpectroscopy).copy(altair = AltairMode.Ngs.some)
    val params   = Map(AltairMode.Ngs -> ngsParameters, AltairMode.Lgs -> lgsParameters)
    val expected = gnirsSpectroscopy.withAltair(ngsParameters.some)
    assertEquals(row.withAltairParameters(params).map(_.instrumentConfig), expected.some)

  test("plain rows are left untouched"):
    val row = spectroscopyRow(gnirsSpectroscopy)
    assertEquals(row.withAltairParameters(Map(AltairMode.Ngs -> ngsParameters)), row.some)
    assertEquals(row.withAltairParameters(Map.empty), row.some)
    val img = imagingRow(gnirsImaging(GnirsFilter.Order4))
    assertEquals(img.withAltairParameters(Map.empty), img.some)

  test("the Altair mode of a configuration follows its parameters"):
    assertEquals(gnirsSpectroscopy.altairMode, none)
    assertEquals(gnirsSpectroscopy.withAltair(ngsParameters.some).altairMode, AltairMode.Ngs.some)
    assertEquals(gnirsSpectroscopy.withAltair(lgsParameters.some).altairMode, AltairMode.Lgs.some)
    assertEquals(
      gnirsImaging(GnirsFilter.Order4).withAltair(AltairParameters.LgsP1.some).altairMode,
      AltairMode.LgsP1.some
    )
    assertEquals(gmosNorthSpectroscopy.withAltair(ngsParameters.some).altairMode, none)

  test("GNIRS imaging rows with different Altair modes are not combined"):
    val ngsH      = gnirsImaging(GnirsFilter.Order4).withAltair(ngsParameters.some)
    val ngsK      = gnirsImaging(GnirsFilter.K).withAltair(ngsParameters.some)
    val lgsJ      = gnirsImaging(GnirsFilter.J).withAltair(lgsParameters.some)
    val plainY    = gnirsImaging(GnirsFilter.Y)
    val selection = ConfigSelection.fromInstrumentConfigs(List(ngsH, ngsK, lgsJ, plainY))
    assertEquals(selection.configs.map(_.instrumentConfig), List(ngsH, ngsK))
    assertEquals(selection.altairMode, AltairMode.Ngs.some)
    assert(!selection.canAdd(InstrumentConfigAndItcResult(lgsJ, none)))
    assertEquals(
      selection.toBasicConfiguration(),
      BasicConfiguration
        .GnirsImaging(cats.data.NonEmptyList.of(GnirsFilter.Order4, GnirsFilter.K), GnirsCamera.ShortBlue)
        .some
    )

  test("a plain selection has no Altair mode"):
    val selection = ConfigSelection.fromInstrumentConfigs(List(gnirsImaging(GnirsFilter.Order4)))
    assertEquals(selection.altairMode, none)
