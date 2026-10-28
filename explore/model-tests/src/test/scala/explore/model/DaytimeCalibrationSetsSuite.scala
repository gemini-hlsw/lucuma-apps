// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptySet
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.Enumerated
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
      PosInt.unsafeFrom(3),
      StepConfig
        .Gcal(
          StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen100W),
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
      PosInt.unsafeFrom(10),
      none,
      DaytimeCalibrationSetup.GmosNorth(north.withoutOptics(spectro))
    )

  test("Dark to Flat gets the default lamp setup and keeps the count"):
    val s = withKind(dark, CalibrationSetType.Flat)
    assertEquals(s.count, dark.count)
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
    assertEquals(first.map(_.count.value), 1.some)
    assertEquals(first.map(_.setup.instrument), Instrument.GmosSouth.some)
    assertEquals(newRow(Nil, Instrument.Gnirs), none)

  test("A new Flamingos 2 row starts as a single dark with no window cover"):
    val first = newRow(Nil, Instrument.Flamingos2)
    assertEquals(first.map(_.kind), CalibrationSetType.Dark.some)
    assertEquals(first.map(_.count.value), 1.some)
    assertEquals(first.map(_.setup.instrument), Instrument.Flamingos2.some)
    assertEquals(
      first.flatMap(s => DaytimeCalibrationSetup.flamingos2.getOption(s.setup)).map(_.config),
      Flamingos2DaytimeSetup.default.some
    )
    assertEquals(Flamingos2DaytimeSetup.default.windowCover, none)

  test("Flamingos 2 sets may only be darks, flats or arcs"):
    assertEquals(
      kinds(Instrument.Flamingos2),
      List(CalibrationSetType.Dark, CalibrationSetType.Flat, CalibrationSetType.Arc)
    )
    assertEquals(kinds(Instrument.GmosNorth), Enumerated[CalibrationSetType].all)

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

  test("Flamingos 2 flats use the IR diffuser"):
    val s = withKind(Flamingos2DaytimeSetup.defaultSet, CalibrationSetType.Flat)
    assertEquals(s.gcal.map(_.diffuser), GcalDiffuser.Ir.some)
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
