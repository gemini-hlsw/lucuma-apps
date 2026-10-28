// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.DaytimeCalibrationSetup
import monocle.Lens
import monocle.Optional

/**
 * The per-site view of a GMOS Instrument Setup the Daytime Calibration Set editor works through.
 * `grating` hides the grating config: picking a grating when there is none builds one at order One
 * with the Central Wavelength of the filter, or the default; clearing it drops the config.
 */
trait GmosDaytimeSetup[D, G, F, U]:
  protected type GratingConfig

  val instrument: Instrument
  val exposure: Lens[D, TimeSpan]
  val readout: Lens[D, GmosCcdMode]
  val roi: Lens[D, GmosRoi]
  val filter: Lens[D, Option[F]]
  val setup: Optional[DaytimeCalibrationSetup, D]
  protected val gratingConfig: Lens[D, Option[GratingConfig]]
  protected val configGrating: Lens[GratingConfig, G]
  protected val configWavelength: Lens[GratingConfig, Wavelength]
  protected val fpuMask: Lens[D, Option[GmosFpuMask[U]]]
  protected def mkGratingConfig(grating: G, wavelength: Wavelength): GratingConfig
  def filterWavelength(f:                F): Wavelength
  def wrap(d:                            D): DaytimeCalibrationSetup
  def default: D

  lazy val grating: Lens[D, Option[G]] =
    Lens[D, Option[G]](gratingConfig.get(_).map(configGrating.get)): g =>
      d =>
        gratingConfig.replace(
          g.map: grating =>
            gratingConfig
              .get(d)
              .fold(
                mkGratingConfig(
                  grating,
                  filter
                    .get(d)
                    .map(filterWavelength)
                    .getOrElse(DaytimeCalibrationSets.DefaultWavelength)
                )
              )(configGrating.replace(grating))
        )(d)

  lazy val wavelength: Lens[D, Option[Wavelength]] =
    Lens[D, Option[Wavelength]](gratingConfig.get(_).map(configWavelength.get)): w =>
      gratingConfig.modify(c => (c, w).mapN((c, w) => configWavelength.replace(w)(c)))

  lazy val fpu: Lens[D, Option[U]] =
    Lens[D, Option[U]](fpuMask.get(_).flatMap(_.builtinFpu)): u =>
      fpuMask.replace(u.map(GmosFpuMask.Builtin(_)))

  def centralWavelength(d: D): Option[Wavelength] =
    gratingConfig.get(d).map(configWavelength.get).orElse(filter.get(d).map(filterWavelength))

  def withoutOptics(d: D): D =
    fpuMask.replace(none)(filter.replace(none)(gratingConfig.replace(none)(d)))

  def defaultSet: DaytimeCalibrationSet =
    DaytimeCalibrationSet(CalibrationSetType.Bias, PosInt.unsafeFrom(1), none, wrap(default))

object GmosDaytimeSetup:
  import DaytimeCalibrationSets.DefaultReadout

  val north
    : GmosDaytimeSetup[DynamicConfig.GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    new GmosDaytimeSetup[DynamicConfig.GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:
      type GratingConfig = GmosGratingConfig.North
      val instrument       = Instrument.GmosNorth
      val exposure         = DynamicConfig.GmosNorth.exposure
      val readout          = DynamicConfig.GmosNorth.readout
      val roi              = DynamicConfig.GmosNorth.roi
      val filter           = DynamicConfig.GmosNorth.filter
      val setup            =
        DaytimeCalibrationSetup.gmosNorth.andThen(DaytimeCalibrationSetup.GmosNorth.config)
      val gratingConfig    = DynamicConfig.GmosNorth.gratingConfig
      val configGrating    = GmosGratingConfig.North.grating
      val configWavelength = GmosGratingConfig.North.wavelength
      val fpuMask          = DynamicConfig.GmosNorth.fpu

      def mkGratingConfig(
        grating:    GmosNorthGrating,
        wavelength: Wavelength
      ): GmosGratingConfig.North =
        GmosGratingConfig.North(grating, GmosGratingOrder.One, wavelength)

      def filterWavelength(f: GmosNorthFilter): Wavelength = f.wavelength

      def wrap(d: DynamicConfig.GmosNorth): DaytimeCalibrationSetup =
        DaytimeCalibrationSetup.GmosNorth(d)

      def default: DynamicConfig.GmosNorth =
        DynamicConfig.GmosNorth(TimeSpan.Zero,
                                DefaultReadout,
                                GmosDtax.Zero,
                                GmosRoi.FullFrame,
                                none,
                                none,
                                none
        )

  val south
    : GmosDaytimeSetup[DynamicConfig.GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
    new GmosDaytimeSetup[DynamicConfig.GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:
      type GratingConfig = GmosGratingConfig.South
      val instrument       = Instrument.GmosSouth
      val exposure         = DynamicConfig.GmosSouth.exposure
      val readout          = DynamicConfig.GmosSouth.readout
      val roi              = DynamicConfig.GmosSouth.roi
      val filter           = DynamicConfig.GmosSouth.filter
      val setup            =
        DaytimeCalibrationSetup.gmosSouth.andThen(DaytimeCalibrationSetup.GmosSouth.config)
      val gratingConfig    = DynamicConfig.GmosSouth.gratingConfig
      val configGrating    = GmosGratingConfig.South.grating
      val configWavelength = GmosGratingConfig.South.wavelength
      val fpuMask          = DynamicConfig.GmosSouth.fpu

      def mkGratingConfig(
        grating:    GmosSouthGrating,
        wavelength: Wavelength
      ): GmosGratingConfig.South =
        GmosGratingConfig.South(grating, GmosGratingOrder.One, wavelength)

      def filterWavelength(f: GmosSouthFilter): Wavelength = f.wavelength

      def wrap(d: DynamicConfig.GmosSouth): DaytimeCalibrationSetup =
        DaytimeCalibrationSetup.GmosSouth(d)

      def default: DynamicConfig.GmosSouth =
        DynamicConfig.GmosSouth(TimeSpan.Zero,
                                DefaultReadout,
                                GmosDtax.Zero,
                                GmosRoi.FullFrame,
                                none,
                                none,
                                none
        )

  def forInstrument(instrument: Instrument): Option[GmosDaytimeSetup[?, ?, ?, ?]] =
    instrument match
      case Instrument.GmosNorth => north.some
      case Instrument.GmosSouth => south.some
      case _                    => none

/** The Flamingos 2 Instrument Setup the Daytime Calibration Set editor works through. */
object Flamingos2DaytimeSetup:
  val setup: Optional[DaytimeCalibrationSetup, Flamingos2DynamicConfig] =
    DaytimeCalibrationSetup.flamingos2.andThen(DaytimeCalibrationSetup.Flamingos2.config)

  /**
   * Picking another read mode also picks its read count, which stays editable, and raises the
   * exposure to the mode's minimum.
   */
  val readMode: Lens[Flamingos2DynamicConfig, Flamingos2ReadMode] =
    Lens[Flamingos2DynamicConfig, Flamingos2ReadMode](_.readMode): m =>
      d =>
        if m === d.readMode then d
        else
          d.copy(
            readMode = m,
            reads = m.readCount,
            exposure = Order.max(d.exposure, m.minimumExposureTime)
          )

  /**
   * No FPU is imaging, with no mask in the beam, or a custom mask, which is kept. Picking an FPU
   * also picks the decker it needs.
   */
  val fpu: Lens[Flamingos2DynamicConfig, Option[Flamingos2Fpu]] =
    Lens[Flamingos2DynamicConfig, Option[Flamingos2Fpu]](_.fpu.builtinFpu): u =>
      d =>
        (u, d.fpu) match
          case (Some(f), _)                           =>
            d.copy(fpu = Flamingos2FpuMask.Builtin(f), decker = f.decker)
          case (None, Flamingos2FpuMask.Custom(_, _)) => d
          case (None, _)                              =>
            d.copy(fpu = Flamingos2FpuMask.Imaging, decker = Flamingos2Decker.Imaging)

  /**
   * Flamingos 2 can't drop its optics, so a new kind without them gets the default ones. The window
   * cover is the user's to set for any kind, so it is kept.
   */
  def forKind(kind: CalibrationSetType, previous: CalibrationSetType)(
    d: Flamingos2DynamicConfig
  ): Flamingos2DynamicConfig =
    if kind === previous || DaytimeCalibrationSets.usesOptics(kind) then d
    else
      d.copy(
        disperser = default.disperser,
        filter = default.filter,
        lyotWheel = default.lyotWheel,
        fpu = default.fpu,
        decker = default.decker
      )

  val default: Flamingos2DynamicConfig =
    Flamingos2DynamicConfig(
      Flamingos2ReadMode.Bright.recommendedExposureTime,
      none,
      Flamingos2Filter.Y,
      Flamingos2ReadMode.Bright,
      Flamingos2LyotWheel.F16,
      Flamingos2FpuMask.Imaging,
      Flamingos2Decker.Imaging,
      Flamingos2ReadoutMode.Science,
      Flamingos2ReadMode.Bright.readCount,
      none
    )

  val defaultSet: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Dark,
      PosInt.unsafeFrom(1),
      none,
      DaytimeCalibrationSetup.Flamingos2(default)
    )

/**
 * The editing rules of Daytime Calibration Sets: every edit leaves a row the ODB accepts, since
 * rows are saved as they change.
 */
object DaytimeCalibrationSets:
  val DefaultWavelength: Wavelength     = Wavelength.unsafeFromIntPicometers(500_000)
  val DefaultExposure: TimeSpan         = TimeSpan.unsafeFromMicroseconds(1_000_000)
  val DefaultReadout: GmosCcdMode       =
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.Low,
                GmosAmpReadMode.Slow
    )
  val DefaultContinuum: GcalContinuum   = GcalContinuum.QuartzHalogen5W
  val DefaultArcs: NonEmptySet[GcalArc] = NonEmptySet.one(GcalArc.ArArc)
  val DefaultGcalFilter: GcalFilter     = GcalFilter.Nd10
  val DefaultShutter: GcalShutter       = GcalShutter.Open

  val Instruments: List[Instrument] =
    List(Instrument.GmosNorth, Instrument.GmosSouth, Instrument.Flamingos2)

  /** The diffuser the ODB defaults to: visible for GMOS, IR for the infrared instruments. */
  def defaultDiffuser(instrument: Instrument): GcalDiffuser =
    instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth => GcalDiffuser.Visible
      case _                                           => GcalDiffuser.Ir

  def defaultGcal(instrument: Instrument): StepConfig.Gcal =
    StepConfig.Gcal(
      StepConfig.Gcal.Lamp.fromContinuum(DefaultContinuum),
      DefaultGcalFilter,
      defaultDiffuser(instrument),
      DefaultShutter
    )

  /** The read mode and gain of a readout, offered together as one choice. */
  case class ReadMode(readMode: GmosAmpReadMode, gain: GmosAmpGain) derives Eq

  object ReadMode:
    given Enumerated[ReadMode] =
      Enumerated
        .fromNEL(
          NonEmptyList.fromListUnsafe(
            for
              r <- Enumerated[GmosAmpReadMode].all
              g <- Enumerated[GmosAmpGain].all
            yield ReadMode(r, g)
          )
        )
        .withTag(r => s"${r.readMode.tag}-${r.gain.tag}")

    val fromCcdMode: Lens[GmosCcdMode, ReadMode] =
      Lens[GmosCcdMode, ReadMode](m => ReadMode(m.ampReadMode, m.ampGain)): r =>
        _.copy(ampReadMode = r.readMode, ampGain = r.gain)

  /** The x and y binning of a readout, offered together as one choice. */
  case class Binning(x: GmosXBinning, y: GmosYBinning) derives Eq

  object Binning:
    given Enumerated[Binning] =
      Enumerated
        .fromNEL(
          NonEmptyList.fromListUnsafe(
            for
              x <- Enumerated[GmosXBinning].all
              y <- Enumerated[GmosYBinning].all
            yield Binning(x, y)
          )
        )
        .withTag(b => s"${b.x.tag}x${b.y.tag}")

    val fromCcdMode: Lens[GmosCcdMode, Binning] =
      Lens[GmosCcdMode, Binning](m => Binning(m.xBin, m.yBin)): b =>
        _.copy(xBin = b.x, yBin = b.y)

  def supports(instrument: Instrument): Boolean = Instruments.contains_(instrument)

  val Flamingos2Kinds: List[CalibrationSetType] =
    List(CalibrationSetType.Dark, CalibrationSetType.Flat, CalibrationSetType.Arc)

  /** The kinds the ODB accepts for an instrument's calibration sets. */
  def kinds(instrument: Instrument): List[CalibrationSetType] =
    instrument match
      case Instrument.Flamingos2 => Flamingos2Kinds
      case _                     => Enumerated[CalibrationSetType].all

  /** The continuum lamps an instrument can't use: the 100W QH lamp is GHOST only. */
  def excludedContinua(instrument: Instrument): Set[GcalContinuum] =
    instrument match
      case Instrument.Ghost => Set.empty
      case _                => Set(GcalContinuum.QuartzHalogen100W)

  def defaultSet(instrument: Instrument): Option[DaytimeCalibrationSet] =
    instrument match
      case Instrument.Flamingos2 => Flamingos2DaytimeSetup.defaultSet.some
      case i                     => GmosDaytimeSetup.forInstrument(i).map(_.defaultSet)

  /** The GCAL setup a kind needs, keeping the filter and shutter of the current one. */
  def gcalFor(
    instrument: Instrument,
    kind:       CalibrationSetType,
    current:    Option[StepConfig.Gcal]
  ): Option[StepConfig.Gcal] =
    val base = current.getOrElse(defaultGcal(instrument))
    kind match
      case CalibrationSetType.Flat =>
        base
          .copy(lamp =
            base.lamp.continuum.fold(StepConfig.Gcal.Lamp.fromContinuum(DefaultContinuum))(
              StepConfig.Gcal.Lamp.fromContinuum
            )
          )
          .some
      case CalibrationSetType.Arc  =>
        base
          .copy(lamp =
            base.lamp.arcs
              .fold(StepConfig.Gcal.Lamp.fromArcs(DefaultArcs))(StepConfig.Gcal.Lamp.fromArcs)
          )
          .some
      case _                       => none

  def usesOptics(kind: CalibrationSetType): Boolean = kind match
    case CalibrationSetType.Bias | CalibrationSetType.Dark => false
    case _                                                 => true

  def usesExposure(kind: CalibrationSetType): Boolean = kind =!= CalibrationSetType.Bias

  private def setupFor[D, G, F, U](site: GmosDaytimeSetup[D, G, F, U], kind: CalibrationSetType)(
    d: D
  ): D =
    val optics   = if usesOptics(kind) then d else site.withoutOptics(d)
    val exposure =
      if !usesExposure(kind) then TimeSpan.Zero
      else if site.exposure.get(d) === TimeSpan.Zero then DefaultExposure
      else site.exposure.get(d)
    site.exposure.replace(exposure)(optics)

  /** Rewrites a row for a new kind so that it stays valid; the count is kept. */
  def withKind(set: DaytimeCalibrationSet, kind: CalibrationSetType): DaytimeCalibrationSet =
    val setup = set.setup match
      case DaytimeCalibrationSetup.GmosNorth(d)  =>
        DaytimeCalibrationSetup.GmosNorth(setupFor(GmosDaytimeSetup.north, kind)(d))
      case DaytimeCalibrationSetup.GmosSouth(d)  =>
        DaytimeCalibrationSetup.GmosSouth(setupFor(GmosDaytimeSetup.south, kind)(d))
      case DaytimeCalibrationSetup.Flamingos2(d) =>
        DaytimeCalibrationSetup.Flamingos2(Flamingos2DaytimeSetup.forKind(kind, set.kind)(d))
    set.copy(kind = kind, gcal = gcalFor(set.setup.instrument, kind, set.gcal), setup = setup)

  /**
   * A new row copies the last one, or starts as a single bias (a dark for Flamingos 2); none for an
   * unsupported instrument.
   */
  def newRow(
    sets:       List[DaytimeCalibrationSet],
    instrument: Instrument
  ): Option[DaytimeCalibrationSet] =
    sets.lastOption.orElse(defaultSet(instrument))
