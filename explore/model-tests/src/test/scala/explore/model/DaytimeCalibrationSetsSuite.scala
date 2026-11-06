// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptySet
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.parser.decode
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.igrins2 as igrins2
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.DaytimeCalibrationSetup
import munit.FunSuite

class DaytimeCalibrationSetsSuite extends FunSuite:
  import DaytimeCalibrationSets.*

  val north = GmosDaytimeSetup.north

  val spectro: DynamicConfig.GmosNorth =
    DynamicConfig.GmosNorth(
      TimeSpan.unsafeFromMicroseconds(15_000_000),
      DefaultReadout,
      GmosDtax.Zero,
      GmosRoi.FullFrame,
      GmosGratingConfig
        .North(GmosNorthGrating.R831_G5302,
               GmosGratingOrder.One,
               Wavelength.unsafeFromIntPicometers(615_000)
        )
        .some,
      GmosNorthFilter.GPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_1_00).some
    )

  val flat: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Flat,
      PosInt.unsafeFrom(3).some,
      StepConfig
        .Gcal(
          StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
          GcalFilter.Nd20,
          GcalDiffuser.Visible,
          GcalShutter.Closed
        )
        .some,
      DaytimeCalibrationSetup.GmosNorth(spectro)
    )

  val dark: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Dark,
      PosInt.unsafeFrom(10).some,
      none,
      DaytimeCalibrationSetup.GmosNorth(north.withoutOptics(spectro))
    )

  test("Dark to Flat gets the default lamp setup and keeps the count"):
    val s = withKind(dark, CalibrationSetType.Flat)
    assertEquals(s.explicitCount, dark.explicitCount)
    assertEquals(
      s.gcal,
      StepConfig
        .Gcal(
          StepConfig.Gcal.Lamp.fromContinuum(DefaultContinuum),
          DefaultGcalFilter,
          GcalDiffuser.Visible,
          DefaultShutter
        )
        .some
    )

  test("Flat to Arc swaps the lamp but keeps the GCAL filter and shutter"):
    val s = withKind(flat, CalibrationSetType.Arc)
    assertEquals(s.gcal.map(_.lamp.arcs), Some(NonEmptySet.one(GcalArc.ArArc).some))
    assertEquals(s.gcal.map(_.filter), GcalFilter.Nd20.some)
    assertEquals(s.gcal.map(_.shutter), GcalShutter.Closed.some)

  test("Flat to Bias drops the lamp, the optics and the exposure"):
    val s = withKind(flat, CalibrationSetType.Bias)
    assertEquals(s.gcal, none)
    val d = DaytimeCalibrationSetup.gmosNorth.getOption(s.setup).map(_.config)
    assertEquals(d.map(_.exposure), TimeSpan.Zero.some)
    assertEquals(d.flatMap(_.gratingConfig), none)
    assertEquals(d.flatMap(_.filter), none)
    assertEquals(d.flatMap(_.fpu), none)
    assertEquals(d.map(_.readout), spectro.readout.some)

  test("Bias to Dark restores a usable exposure"):
    val bias = withKind(flat, CalibrationSetType.Bias)
    val s    = withKind(bias, CalibrationSetType.Dark)
    assertEquals(north.setup.getOption(s.setup).map(_.exposure), DefaultExposure.some)

  test("Flat to Daytime Observe keeps the optics and drops the lamp"):
    val s = withKind(flat, CalibrationSetType.DaytimeObserve)
    assertEquals(s.gcal, none)
    assertEquals(DaytimeCalibrationSetup.gmosNorth.getOption(s.setup).map(_.config), spectro.some)

  test("Picking a grating prefills the wavelength from the filter"):
    val imaging = north.grating.replace(none)(spectro)
    val d       = north.grating.replace(GmosNorthGrating.R600_G5304.some)(imaging)
    assertEquals(d.gratingConfig.map(_.wavelength), GmosNorthFilter.GPrime.wavelength.some)
    assertEquals(d.gratingConfig.map(_.order), GmosGratingOrder.One.some)

  test("Picking a grating with no filter uses the default wavelength"):
    val bare = north.filter.replace(none)(north.grating.replace(none)(spectro))
    val d    = north.grating.replace(GmosNorthGrating.R600_G5304.some)(bare)
    assertEquals(d.gratingConfig.map(_.wavelength), DefaultWavelength.some)

  test("Central wavelength comes from the grating, else the filter"):
    assertEquals(north.centralWavelength(spectro), Wavelength.unsafeFromIntPicometers(615_000).some)
    val imaging = north.grating.replace(none)(spectro)
    assertEquals(north.centralWavelength(imaging), GmosNorthFilter.GPrime.wavelength.some)
    assertEquals(north.centralWavelength(north.filter.replace(none)(imaging)), none)

  test("A new row copies the last one, or starts as a single bias"):
    assertEquals(newRow(List(dark, flat), Instrument.GmosNorth), flat.some)
    val first = newRow(Nil, Instrument.GmosSouth)
    assertEquals(first.map(_.kind), CalibrationSetType.Bias.some)
    assertEquals(first.flatMap(_.explicitCount).map(_.value), 1.some)
    assertEquals(first.map(_.setup.instrument), Instrument.GmosSouth.some)
    assertEquals(newRow(Nil, Instrument.Ghost), none)

  test("A new Flamingos 2 row starts as a single dark with no window cover"):
    val first = newRow(Nil, Instrument.Flamingos2)
    assertEquals(first.map(_.kind), CalibrationSetType.Dark.some)
    assertEquals(first.flatMap(_.explicitCount).map(_.value), 1.some)
    assertEquals(first.map(_.setup.instrument), Instrument.Flamingos2.some)
    assertEquals(
      first.flatMap(s => DaytimeCalibrationSetup.flamingos2.getOption(s.setup)).map(_.config),
      Flamingos2DaytimeSetup.default.some
    )
    assertEquals(Flamingos2DaytimeSetup.default.windowCover, none)

  test("Flat to Smart Flat drops the count and lamp setup, zeroes the exposure, keeps the optics"):
    val s = withKind(flat, CalibrationSetType.SmartFlat)
    assertEquals(s.explicitCount, none)
    assertEquals(s.gcal, none)
    assertEquals(s.setup,
                 DaytimeCalibrationSetup.GmosNorth(north.exposure.replace(TimeSpan.Zero)(spectro))
    )

  test("Smart Flat back to Flat starts the count at one and restores the default exposure"):
    // The ODB reads back the count the lookup resolved; it is not the user's to keep.
    val smart =
      withKind(flat, CalibrationSetType.SmartFlat).copy(explicitCount = PosInt.unsafeFrom(3).some)
    val s     = withKind(smart, CalibrationSetType.Flat)
    assertEquals(s.explicitCount.map(_.value), 1.some)
    assertEquals(s.gcal, gcalFor(Instrument.GmosNorth, CalibrationSetType.Flat, none))
    assertEquals(s.setup,
                 DaytimeCalibrationSetup.GmosNorth(north.exposure.replace(DefaultExposure)(spectro))
    )

  test("Changing the kind leaves the resolved count pending until the ODB reads it back"):
    val resolved = flat.copy(count = DaytimeCalibrationSet.Count.Resolved(PosInt.unsafeFrom(3)))
    assertEquals(withKind(resolved, CalibrationSetType.SmartFlat).count,
                 DaytimeCalibrationSet.Count.Pending
    )
    assertEquals(withKind(resolved, CalibrationSetType.Flat).count, resolved.count)

  test("Smart Arc gets a grating when the setup has none, and keeps one it has"):
    val fromDark = withKind(dark, CalibrationSetType.SmartArc)
    assertEquals(north.setup.getOption(fromDark.setup).flatMap(north.grating.get),
                 north.defaultGrating.some
    )
    val fromFlat = withKind(flat, CalibrationSetType.SmartArc)
    assertEquals(north.setup.getOption(fromFlat.setup).flatMap(north.grating.get),
                 GmosNorthGrating.R831_G5302.some
    )
    val f2       = Flamingos2DaytimeSetup.forKind(CalibrationSetType.SmartArc, CalibrationSetType.Dark)(
      Flamingos2DaytimeSetup.default
    )
    assertEquals(f2.disperser, Flamingos2DaytimeSetup.DefaultDisperser.some)

  test("Only GHOST may use the 100W QH lamp"):
    Instruments.foreach: i =>
      assertEquals(excludedContinua(i), Set(GcalContinuum.QuartzHalogen100W))
    assertEquals(excludedContinua(Instrument.Ghost), Set.empty[GcalContinuum])

  test("The default lamp is offered for every instrument"):
    Instruments.foreach: i =>
      assert(!excludedContinua(i).contains(DefaultContinuum))

  test("Picking a Flamingos 2 read mode also picks its read count"):
    val d = Flamingos2DaytimeSetup.readMode.replace(Flamingos2ReadMode.Faint)(
      Flamingos2DaytimeSetup.default
    )
    assertEquals(d.readMode, Flamingos2ReadMode.Faint)
    assertEquals(d.reads, Flamingos2ReadMode.Faint.readCount)
    assertEquals(d.exposure, Flamingos2ReadMode.Faint.minimumExposureTime)

  test("Re-picking the Flamingos 2 read mode keeps a hand-set read count"):
    val d = Flamingos2DaytimeSetup.default.copy(reads = Flamingos2Reads.Reads_16)
    assertEquals(Flamingos2DaytimeSetup.readMode.replace(d.readMode)(d), d)

  test("Clearing the Flamingos 2 FPU puts the imaging mask back"):
    val fpu = Flamingos2DaytimeSetup.fpu
    val d   = fpu.replace(Flamingos2Fpu.LongSlit2.some)(Flamingos2DaytimeSetup.default)
    assertEquals(fpu.get(d), Flamingos2Fpu.LongSlit2.some)
    assertEquals(fpu.replace(none)(d).fpu, Flamingos2FpuMask.Imaging)
    assertEquals(fpu.replace(none)(d).decker, Flamingos2Decker.Imaging)

  test("Picking a Flamingos 2 FPU picks its decker"):
    val d = Flamingos2DaytimeSetup.fpu.replace(Flamingos2Fpu.LongSlit2.some)(
      Flamingos2DaytimeSetup.default
    )
    assertEquals(d.decker, Flamingos2Fpu.LongSlit2.decker)

  test("Changing a Flamingos 2 kind keeps the window cover"):
    val f2     = Flamingos2DaytimeSetup.defaultSet
    val open   = f2.copy(
      kind = CalibrationSetType.Flat,
      gcal = gcalFor(Instrument.Flamingos2, CalibrationSetType.Flat, none),
      setup = DaytimeCalibrationSetup.Flamingos2(
        Flamingos2DaytimeSetup.default.copy(windowCover = Flamingos2WindowCover.Open.some)
      )
    )
    val config = (s: DaytimeCalibrationSet) =>
      DaytimeCalibrationSetup.flamingos2.getOption(s.setup).flatMap(_.config.windowCover)
    assertEquals(config(withKind(open, CalibrationSetType.Flat)), Flamingos2WindowCover.Open.some)
    assertEquals(config(withKind(open, CalibrationSetType.Dark)), Flamingos2WindowCover.Open.some)

  test("A Flamingos 2 smart kind zeroes the exposure, and leaving it restores the default"):
    val exposure =
      (s: DaytimeCalibrationSet) => Flamingos2DaytimeSetup.setup.getOption(s.setup).map(_.exposure)
    val long     = Flamingos2DaytimeSetup.defaultSet.copy(setup =
      DaytimeCalibrationSetup.Flamingos2(
        Flamingos2DaytimeSetup.default.copy(exposure = TimeSpan.unsafeFromMicroseconds(45_000_000))
      )
    )
    val smart    = withKind(long, CalibrationSetType.SmartFlat)
    assertEquals(exposure(smart), TimeSpan.Zero.some)
    assertEquals(exposure(withKind(smart, CalibrationSetType.Flat)),
                 Flamingos2ReadMode.Bright.recommendedExposureTime.some
    )
    assertEquals(exposure(withKind(long, CalibrationSetType.Arc)),
                 TimeSpan.unsafeFromMicroseconds(45_000_000).some
    )

  test("Infrared flats use the IR diffuser, GMOS flats the visible one"):
    List(
      Flamingos2DaytimeSetup.defaultSet,
      Igrins2DaytimeSetup.defaultSet,
      GnirsDaytimeSetup.defaultSet
    ).foreach: set =>
      assertEquals(withKind(set, CalibrationSetType.Flat).gcal.map(_.diffuser),
                   GcalDiffuser.Ir.some
      )
    assertEquals(withKind(dark, CalibrationSetType.Flat).gcal.map(_.diffuser),
                 GcalDiffuser.Visible.some
    )

  test("A Flamingos 2 dark gets the default optics"):
    val flat   = withKind(Flamingos2DaytimeSetup.defaultSet, CalibrationSetType.Flat)
    val config = (s: DaytimeCalibrationSet) =>
      DaytimeCalibrationSetup.flamingos2.getOption(s.setup).map(_.config)
    val custom = flat.copy(setup =
      DaytimeCalibrationSetup.Flamingos2(
        Flamingos2DaytimeSetup.fpu.replace(Flamingos2Fpu.LongSlit2.some)(
          Flamingos2DaytimeSetup.default.copy(
            filter = Flamingos2Filter.J,
            disperser = Flamingos2Disperser.R1200JH.some
          )
        )
      )
    )
    val dark   = config(withKind(custom, CalibrationSetType.Dark))
    assertEquals(dark.map(_.filter), Flamingos2DaytimeSetup.default.filter.some)
    assertEquals(dark.map(_.disperser), Flamingos2DaytimeSetup.default.disperser.some)
    assertEquals(dark.map(_.fpu), Flamingos2DaytimeSetup.default.fpu.some)
    assertEquals(dark.map(_.decker), Flamingos2DaytimeSetup.default.decker.some)
    assertEquals(config(withKind(custom, CalibrationSetType.Arc)).map(_.filter),
                 Flamingos2Filter.J.some
    )

  test("A new IGRINS-2 row starts as a single dark of the IGRINS-2 default exposure"):
    val first = newRow(Nil, Instrument.Igrins2)
    assertEquals(first, Igrins2DaytimeSetup.defaultSet.some)
    assertEquals(first.map(_.kind), CalibrationSetType.Dark.some)
    assertEquals(first.flatMap(_.explicitCount).map(_.value), 1.some)
    assertEquals(Igrins2DaytimeSetup.default.exposure, igrins2.DefaultExposureTime)

  test("An IGRINS-2 exposure outside its limits is rejected"):
    val valid = inputCodecs.Igrins2CalibrationExposure.valid
    assertEquals(valid.getValid("3.08").toOption, igrins2.MinExposureTime.some)
    assertEquals(valid.getValid("600").toOption, igrins2.MaxExposureTime.some)
    assertEquals(valid.getValid("5.5").toOption, TimeSpan.unsafeFromMicroseconds(5_500_000).some)
    assertEquals(valid.getValid("3.07").leftMap(_.head.value),
                 "Must be between 3.08 and 600 s".asLeft
    )
    assert(valid.getValid("600.01").isLeft)

  test("IGRINS-2 takes neither a bias nor a daytime observe"):
    assertEquals(kinds(Instrument.Igrins2), kinds(Instrument.Flamingos2))
    assert(!kinds(Instrument.Igrins2).contains(CalibrationSetType.Bias))
    assert(!kinds(Instrument.Igrins2).contains(CalibrationSetType.DaytimeObserve))
    assert(supports(Instrument.Igrins2))

  test("An IGRINS-2 smart kind zeroes the exposure, and leaving it restores the default"):
    val exposure =
      (s: DaytimeCalibrationSet) => Igrins2DaytimeSetup.setup.getOption(s.setup).map(_.exposure)
    val long     = Igrins2DaytimeSetup.defaultSet.copy(setup =
      DaytimeCalibrationSetup.Igrins2(
        Igrins2DynamicConfig(TimeSpan.unsafeFromMicroseconds(45_000_000))
      )
    )
    val smart    = withKind(long, CalibrationSetType.SmartFlat)
    assertEquals(exposure(smart), TimeSpan.Zero.some)
    assertEquals(smart.explicitCount, none)
    assertEquals(exposure(withKind(smart, CalibrationSetType.Dark)),
                 igrins2.DefaultExposureTime.some
    )
    assertEquals(exposure(withKind(long, CalibrationSetType.Flat)),
                 TimeSpan.unsafeFromMicroseconds(45_000_000).some
    )

  test("An IGRINS-2 calibration set decodes its setup"):
    val json =
      """{
        "kind": "DARK",
        "count": 2,
        "explicitCount": 2,
        "gcal": null,
        "gmosNorth": null,
        "gmosSouth": null,
        "flamingos2": null,
        "igrins2": { "exposure": { "microseconds": 5000000 } }
      }"""
    assertEquals(
      decode[DaytimeCalibrationSet](json).map(_.setup),
      DaytimeCalibrationSetup
        .Igrins2(Igrins2DynamicConfig(TimeSpan.unsafeFromMicroseconds(5_000_000)))
        .asRight
    )

  val gnirs = GnirsDaytimeSetup

  val gnirsSpectro: GnirsDynamicConfig =
    gnirs.fpu.replace(GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30))(
      gnirs.grating.replace(GnirsGrating.D32.some)(gnirs.default)
    )

  def gnirsConfig(s: DaytimeCalibrationSet): Option[GnirsDynamicConfig] =
    gnirs.setup.getOption(s.setup)

  test("A new GNIRS row starts as a single 20 s dark through the acquisition mirror"):
    val first = newRow(Nil, Instrument.Gnirs)
    assertEquals(first.map(_.kind), CalibrationSetType.Dark.some)
    assertEquals(first.flatMap(_.explicitCount).map(_.value), 1.some)
    assertEquals(first.map(_.setup.instrument), Instrument.Gnirs.some)
    assertEquals(gnirsConfig(gnirs.defaultSet).map(_.exposure), gnirs.DefaultExposure.some)
    assertEquals(gnirs.default.acquisitionMirror, GnirsAcquisitionMirrorMode.In)

  test("GNIRS takes every kind but a bias"):
    assert(supports(Instrument.Gnirs))
    assert(!kinds(Instrument.Gnirs).contains(CalibrationSetType.Bias))
    assert(kinds(Instrument.Gnirs).contains(CalibrationSetType.DaytimeObserve))

  test("A new GNIRS exposure picks its read mode, which stays editable"):
    val long = gnirs.exposure.replace(TimeSpan.unsafeFromMicroseconds(120_000_000))(gnirs.default)
    assertEquals(long.readMode, GnirsReadMode.VeryFaint)
    val hand = long.copy(readMode = GnirsReadMode.Faint)
    assertEquals(gnirs.exposure.replace(hand.exposure)(hand), hand)

  test("Picking a GNIRS grating takes the mirror out, and clearing it puts it back"):
    val out = gnirs.grating.replace(GnirsGrating.D111.some)(gnirs.default)
    assertEquals(
      out.acquisitionMirror,
      gnirs.defaultDisperser(gnirs.default).copy(grating = GnirsGrating.D111)
    )
    val lxd = gnirs.prism.replace(GnirsPrism.Lxd)(gnirsSpectro)
    assertEquals(
      gnirs.grating.replace(GnirsGrating.D10.some)(lxd).acquisitionMirror,
      GnirsAcquisitionMirrorMode.Out(GnirsPrism.Lxd,
                                     GnirsGrating.D10,
                                     gnirs.defaultDisperser(gnirs.default).wavelength
      )
    )
    assertEquals(gnirs.grating.replace(none)(out).acquisitionMirror, GnirsAcquisitionMirrorMode.In)

  test("The GNIRS decker follows the camera, the prism and the FPU"):
    assertEquals(gnirsSpectro.decker, GnirsDecker.ShortCamLongSlit)
    assertEquals(gnirs.camera.replace(GnirsCamera.LongBlue)(gnirsSpectro).decker,
                 GnirsDecker.LongCamLongSlit
    )
    assertEquals(gnirs.prism.replace(GnirsPrism.Sxd)(gnirsSpectro).decker,
                 GnirsDecker.ShortCamCrossDispersed
    )
    assertEquals(
      gnirs.fpu.replace(GnirsFpu.Other(GnirsFpuOther.Acquisition))(gnirsSpectro).decker,
      GnirsDecker.Acquisition
    )
    assertEquals(
      gnirs.fpu.replace(GnirsFpu.Other(GnirsFpuOther.PupilViewer))(gnirsSpectro).decker,
      GnirsDecker.PupilViewer
    )

  test("A hand-picked GNIRS decker is kept until the camera, prism or FPU changes"):
    val hand = gnirsSpectro.copy(decker = GnirsDecker.PupilViewer)
    assertEquals(gnirs.exposure.replace(TimeSpan.unsafeFromMicroseconds(5_000_000))(hand).decker,
                 GnirsDecker.PupilViewer
    )
    assertEquals(gnirs.grating.replace(GnirsGrating.D111.some)(hand).decker,
                 GnirsDecker.PupilViewer
    )
    assertEquals(gnirs.camera.replace(GnirsCamera.ShortBlue)(hand).decker,
                 GnirsDecker.ShortCamLongSlit
    )

  test("The GNIRS prism and wavelength are only set with the mirror out"):
    assertEquals(gnirs.prism.replace(GnirsPrism.Sxd)(gnirs.default), gnirs.default)
    val w    = Wavelength.unsafeFromIntPicometers(1_650_000)
    assertEquals(gnirs.wavelength.replace(w)(gnirs.default), gnirs.default)
    assertEquals(gnirs.wavelength.getOption(gnirs.wavelength.replace(w)(gnirsSpectro)), w.some)
    val hand = gnirs.default.copy(decker = GnirsDecker.PupilViewer)
    assertEquals(gnirs.prism.replace(GnirsPrism.Sxd)(hand), hand)

  test("Every GNIRS arc and smart kind gets a disperser and a slit, keeping one it has"):
    List(
      CalibrationSetType.Arc,
      CalibrationSetType.SmartFlat,
      CalibrationSetType.SmartArc,
      CalibrationSetType.DayBaseline
    )
      .foreach: kind =>
        val d = gnirsConfig(withKind(gnirs.defaultSet, kind))
        assertEquals(d.map(_.acquisitionMirror), gnirs.defaultDisperser(gnirs.default).some)
        assertEquals(d.map(_.fpu), GnirsFpu.Spectroscopy.Slit(gnirs.DefaultSlit).some)
        assertEquals(d.map(_.decker), GnirsDecker.ShortCamLongSlit.some)
    val lxd  = gnirs.prism.replace(GnirsPrism.Lxd)(gnirsSpectro)
    val kept = gnirsConfig(
      withKind(gnirs.defaultSet.copy(setup = DaytimeCalibrationSetup.Gnirs(lxd)),
               CalibrationSetType.SmartFlat
      )
    )
    assertEquals(kept.map(_.acquisitionMirror), lxd.acquisitionMirror.some)
    assertEquals(kept.map(_.fpu), lxd.fpu.some)
    assert(!gnirs.needsDisperser(CalibrationSetType.Flat))

  test("The GNIRS default disperser sits at the filter's wavelength"):
    def wavelength(f: GnirsFilter): Option[Wavelength] =
      gnirs.wavelength.getOption(
        gnirs.grating.replace(GnirsGrating.D32.some)(gnirs.default.copy(filter = f))
      )
    assertEquals(wavelength(GnirsFilter.Order4), Wavelength.unsafeFromIntPicometers(1_650_000).some)
    assertEquals(wavelength(GnirsFilter.Order3), Wavelength.unsafeFromIntPicometers(2_200_000).some)
    assertEquals(wavelength(GnirsFilter.CrossDispersed),
                 Wavelength.unsafeFromIntPicometers(1_650_000).some
    )

  test("A GNIRS arc swaps an imaging FPU for a slit even with the mirror out, but keeps a pinhole"):
    val out                                                    = gnirs.grating.replace(GnirsGrating.D32.some)(gnirs.default)
    def arc(d: GnirsDynamicConfig): Option[GnirsDynamicConfig] =
      gnirsConfig(
        withKind(gnirs.defaultSet.copy(setup = DaytimeCalibrationSetup.Gnirs(d)),
                 CalibrationSetType.SmartArc
        )
      )
    assertEquals(arc(out).map(_.fpu), GnirsFpu.Spectroscopy.Slit(gnirs.DefaultSlit).some)
    assertEquals(arc(out).map(_.decker), GnirsDecker.ShortCamLongSlit.some)
    val pinhole                                                = gnirs.fpu.replace(GnirsFpu.Other(GnirsFpuOther.Pinhole1))(out)
    assertEquals(arc(pinhole).map(_.fpu), pinhole.fpu.some)

  test("GNIRS offers the IFUs"):
    assert(gnirs.Fpus.contains(GnirsFpu.Spectroscopy.Ifu(GnirsFpuIfu.LowResolution)))

  test("A GNIRS dark gets the default optics and keeps its exposure and read mode"):
    val arc  = withKind(
      gnirs.defaultSet.copy(setup = DaytimeCalibrationSetup.Gnirs(gnirsSpectro)),
      CalibrationSetType.Arc
    )
    val dark = gnirsConfig(withKind(arc, CalibrationSetType.Dark))
    assertEquals(dark, gnirs.default.some)
    assertEquals(gnirsConfig(arc).map(_.acquisitionMirror), gnirsSpectro.acquisitionMirror.some)

  test("A GNIRS smart kind zeroes the exposure, and leaving it restores the default"):
    val smart = withKind(gnirs.defaultSet, CalibrationSetType.SmartFlat)
    assertEquals(gnirsConfig(smart).map(_.exposure), TimeSpan.Zero.some)
    val back  = gnirsConfig(withKind(smart, CalibrationSetType.Flat))
    assertEquals(back.map(_.exposure), gnirs.DefaultExposure.some)
    assertEquals(back.map(_.readMode), GnirsReadMode.Bright.some)

  test("A GNIRS calibration set decodes its setup"):
    val json =
      """{
        "kind": "DARK",
        "count": 1,
        "explicitCount": 1,
        "gcal": null,
        "gmosNorth": null,
        "gmosSouth": null,
        "flamingos2": null,
        "igrins2": null,
        "gnirs": {
          "exposure": { "microseconds": 20000000 },
          "coadds": 1,
          "centralWavelength": { "picometers": 2200000 },
          "filter": "ORDER4",
          "decker": "ACQUISITION",
          "fpuSlit": null,
          "fpuOther": "ACQUISITION",
          "fpuIfu": null,
          "acquisitionMirrorOut": null,
          "camera": "SHORT_BLUE",
          "focusMotorSteps": null,
          "readMode": "BRIGHT"
        }
      }"""
    assertEquals(
      decode[DaytimeCalibrationSet](json).map(_.setup),
      DaytimeCalibrationSetup.Gnirs(gnirs.default).asRight
    )
