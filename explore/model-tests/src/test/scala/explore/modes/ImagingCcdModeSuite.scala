// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Order.given
import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.Pot
import explore.model.Observation
import explore.model.TargetList
import explore.model.arb.ArbObservation.given
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import scala.collection.immutable.SortedMap

class ImagingCcdModeSuite extends ScalaCheckSuite:
  private val noTargets: TargetList = SortedMap.empty

  private val point: SourceProfile =
    SourceProfile.Point(SpectralDefinition.BandNormalized(none, SortedMap.empty))

  private val wideGaussian: SourceProfile =
    SourceProfile.Gaussian(
      Angle.fromDoubleArcseconds(2.0),
      SpectralDefinition.BandNormalized(none, SortedMap.empty)
    )

  private def ccdMode(bin: GmosBinning): GmosCcdMode =
    GmosCcdMode(
      GmosXBinning(bin),
      GmosYBinning(bin),
      GmosAmpCount.Twelve,
      GmosAmpGain.Low,
      GmosAmpReadMode.Slow
    )

  private val gmosNorth: ItcInstrumentConfig =
    ItcInstrumentConfig.GmosNorthImaging(GmosNorthFilter.GPrime, ItcInstrumentConfig.PlaceholderEtm)

  private val gmosSouth: ItcInstrumentConfig =
    ItcInstrumentConfig.GmosSouthImaging(GmosSouthFilter.GPrime, ItcInstrumentConfig.PlaceholderEtm)

  private def row(config: ItcInstrumentConfig): ImagingModeRow =
    ImagingModeRow(none, config, ModeAO.NoAO, Angle.fromDoubleArcseconds(330.0), none, none)

  private def defaultCcdMode(
    config:   ItcInstrumentConfig,
    profiles: NonEmptyList[SourceProfile],
    iq:       ImageQuality.Preset
  ): Option[GmosCcdMode] =
    row(config).withDefaultCcdMode(profiles.some, iq).instrumentConfig match
      case ItcInstrumentConfig.GmosNorthImaging(_, _, ccdMode) => ccdMode
      case ItcInstrumentConfig.GmosSouthImaging(_, _, ccdMode) => ccdMode
      case _                                                   => none

  test("a point source at 1.0\" IQ defaults to 2x2, slow read, low gain"):
    val ps = NonEmptyList.one(point)
    assertEquals(defaultCcdMode(gmosNorth, ps, ImageQuality.Preset.OnePointZero),
                 ccdMode(GmosBinning.Two).some
    )
    assertEquals(defaultCcdMode(gmosSouth, ps, ImageQuality.Preset.OnePointZero),
                 ccdMode(GmosBinning.Two).some
    )

  test("binning is capped at 2"):
    assertEquals(
      defaultCcdMode(gmosNorth, NonEmptyList.one(wideGaussian), ImageQuality.Preset.TwoPointZero),
      ccdMode(GmosBinning.Two).some
    )

  test("the smallest binning in the asterism is used"):
    assertEquals(
      defaultCcdMode(gmosNorth, NonEmptyList.one(wideGaussian), ImageQuality.Preset.PointFour),
      ccdMode(GmosBinning.Two).some
    )
    assertEquals(
      defaultCcdMode(gmosNorth,
                     NonEmptyList.of(wideGaussian, point),
                     ImageQuality.Preset.PointFour
      ),
      ccdMode(GmosBinning.One).some
    )

  test("an explicit ccd mode, e.g. from a reverted observation, is reset to the default"):
    val config =
      ItcInstrumentConfig.GmosNorthImaging(
        GmosNorthFilter.GPrime,
        ItcInstrumentConfig.PlaceholderEtm,
        GmosCcdMode(
          GmosXBinning.Four,
          GmosYBinning.Four,
          GmosAmpCount.Twelve,
          GmosAmpGain.High,
          GmosAmpReadMode.Fast
        ).some
      )
    assertEquals(
      defaultCcdMode(config, NonEmptyList.one(point), ImageQuality.Preset.OnePointZero),
      ccdMode(GmosBinning.Two).some
    )

  test("rows are unchanged without targets, or for other instruments"):
    assertEquals(row(gmosNorth).withDefaultCcdMode(none, ImageQuality.Preset.OnePointZero),
                 row(gmosNorth)
    )
    val flamingos2 =
      row(
        ItcInstrumentConfig.Flamingos2Imaging(Flamingos2Filter.J,
                                              ItcInstrumentConfig.PlaceholderEtm
        )
      )
    assertEquals(
      flamingos2.withDefaultCcdMode(NonEmptyList.one(point).some, ImageQuality.Preset.OnePointZero),
      flamingos2
    )

  property("GMOS North imaging observations use the observation's ccd mode"):
    forAll: (obs: Observation, mode: ObservingMode.GmosNorthImaging) =>
      val expected =
        GmosCcdMode(
          GmosXBinning(mode.bin),
          GmosYBinning(mode.bin),
          GmosAmpCount.Twelve,
          mode.ampGain,
          mode.ampReadMode
        )
      val configs  = obs.copy(observingMode = Pot(mode.some)).toInstrumentConfig(noTargets)
      assertEquals(configs.length, mode.filters.length)
      configs.foreach:
        case ItcInstrumentConfig.GmosNorthImaging(_, _, ccdMode) =>
          assertEquals(ccdMode, expected.some)
        case other                                               =>
          fail(s"Unexpected config $other")

  property("GMOS South imaging observations use the observation's ccd mode"):
    forAll: (obs: Observation, mode: ObservingMode.GmosSouthImaging) =>
      val expected =
        GmosCcdMode(
          GmosXBinning(mode.bin),
          GmosYBinning(mode.bin),
          GmosAmpCount.Twelve,
          mode.ampGain,
          mode.ampReadMode
        )
      val configs  = obs.copy(observingMode = Pot(mode.some)).toInstrumentConfig(noTargets)
      assertEquals(configs.length, mode.filters.length)
      configs.foreach:
        case ItcInstrumentConfig.GmosSouthImaging(_, _, ccdMode) =>
          assertEquals(ccdMode, expected.some)
        case other                                               =>
          fail(s"Unexpected config $other")
