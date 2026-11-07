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
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.igrins2 as igrins2
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.DaytimeCalibrationSet
import lucuma.schemas.model.DaytimeCalibrationSetup
import monocle.Focus
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
  def defaultGrating: G

  /** A grating for a kind that needs one, keeping the current one if any. */
  def withGrating(d: D): D =
    if grating.get(d).isDefined then d else grating.replace(defaultGrating.some)(d)

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
    DaytimeCalibrationSet(
      CalibrationSetType.Bias,
      DaytimeCalibrationSets.DefaultCount.some,
      none,
      wrap(default)
    )

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

      val defaultGrating: GmosNorthGrating = GmosNorthGrating.R831_G5302

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

      val defaultGrating: GmosSouthGrating = GmosSouthGrating.R831_G5322

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
    val optics     =
      if kind === previous || DaytimeCalibrationSets.usesOptics(kind) then d
      else
        d.copy(
          disperser = default.disperser,
          filter = default.filter,
          lyotWheel = default.lyotWheel,
          fpu = default.fpu,
          decker = default.decker
        )
    val configured =
      if DaytimeCalibrationSets.needsDisperser(kind) then withDisperser(optics) else optics
    configured.copy(exposure =
      DaytimeCalibrationSets.exposureFor(kind, d.exposure, d.readMode.recommendedExposureTime)
    )

  val DefaultDisperser: Flamingos2Disperser = Flamingos2Disperser.R1200JH

  /** A disperser for a kind that needs one, keeping the current one if any. */
  def withDisperser(d: Flamingos2DynamicConfig): Flamingos2DynamicConfig =
    if d.disperser.isDefined then d else d.copy(disperser = DefaultDisperser.some)

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
      DaytimeCalibrationSets.DefaultCount.some,
      none,
      DaytimeCalibrationSetup.Flamingos2(default)
    )

/** The IGRINS-2 Instrument Setup the Daytime Calibration Set editor works through. */
object Igrins2DaytimeSetup:
  val setup: Optional[DaytimeCalibrationSetup, Igrins2DynamicConfig] =
    DaytimeCalibrationSetup.igrins2.andThen(DaytimeCalibrationSetup.Igrins2.config)

  def forKind(kind: CalibrationSetType)(d: Igrins2DynamicConfig): Igrins2DynamicConfig =
    d.copy(exposure =
      DaytimeCalibrationSets.exposureFor(kind, d.exposure, igrins2.DefaultExposureTime)
    )

  val default: Igrins2DynamicConfig = Igrins2DynamicConfig(igrins2.DefaultExposureTime)

  val defaultSet: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Dark,
      DaytimeCalibrationSets.DefaultCount.some,
      none,
      DaytimeCalibrationSetup.Igrins2(default)
    )

/**
 * The GNIRS Instrument Setup the Daytime Calibration Set editor works through. The decker stays
 * editable but is picked again whenever the camera, prism or FPU changes, and the read mode is
 * picked again whenever the exposure changes.
 */
object GnirsDaytimeSetup:
  val setup: Optional[DaytimeCalibrationSetup, GnirsDynamicConfig] =
    DaytimeCalibrationSetup.gnirs.andThen(DaytimeCalibrationSetup.Gnirs.config)

  val DefaultExposure: TimeSpan = TimeSpan.unsafeFromMicroseconds(20_000_000)

  val DefaultSlit: GnirsFpuSlit = GnirsFpuSlit.LongSlit_0_30

  /**
   * A setup the GNIRS smart GCAL tables cover, at the filter's wavelength: the lookup ignores the
   * filter, so another wavelength would match lamps for light the filter blocks.
   */
  def defaultDisperser(d: GnirsDynamicConfig): GnirsAcquisitionMirrorMode.Out =
    GnirsAcquisitionMirrorMode.Out(
      GnirsPrism.Mirror,
      GnirsGrating.D32,
      GnirsGratingWavelength(d.filter.centralWavelength)
    )

  val Fpus: List[GnirsFpu] =
    Enumerated[GnirsFpuOther].all.map(GnirsFpu.Other(_)) ++
      Enumerated[GnirsFpuSlit].all.map(GnirsFpu.Spectroscopy.Slit(_)) ++
      Enumerated[GnirsFpuIfu].all.map(GnirsFpu.Spectroscopy.Ifu(_))

  given Enumerated[GnirsFpu] =
    Enumerated
      .fromNEL(NonEmptyList.fromListUnsafe(Fpus))
      .withTag(_.fold(_.tag, _.tag, _.tag))

  given Display[GnirsFpu] =
    Display.byShortName(_.fold(s => s"${s.shortName} slit", _.shortName, _.shortName))

  def deckerFor(d: GnirsDynamicConfig): GnirsDecker =
    val prism = GnirsAcquisitionMirrorMode.out.getOption(d.acquisitionMirror).map(_.prism)
    d.fpu match
      case GnirsFpu.Other(GnirsFpuOther.Acquisition) => GnirsDecker.Acquisition
      case GnirsFpu.Other(GnirsFpuOther.PupilViewer) => GnirsDecker.PupilViewer
      case GnirsFpu.Spectroscopy.Ifu(ifu)            => GnirsDecker.forIfu(ifu)
      case _                                         =>
        GnirsDecker.forCameraAndPrism(d.camera, prism.getOrElse(GnirsPrism.Mirror))

  private def withDecker(d: GnirsDynamicConfig): GnirsDynamicConfig =
    d.copy(decker = deckerFor(d))

  val exposure: Lens[GnirsDynamicConfig, TimeSpan] =
    Lens[GnirsDynamicConfig, TimeSpan](_.exposure): t =>
      d =>
        if t === d.exposure then d
        else if t === TimeSpan.Zero then d.copy(exposure = t)
        else d.copy(exposure = t, readMode = GnirsReadMode.forExposureTime(t))

  val camera: Lens[GnirsDynamicConfig, GnirsCamera] =
    Lens[GnirsDynamicConfig, GnirsCamera](_.camera)(c => d => withDecker(d.copy(camera = c)))

  val fpu: Lens[GnirsDynamicConfig, GnirsFpu] =
    Lens[GnirsDynamicConfig, GnirsFpu](_.fpu)(u => d => withDecker(d.copy(fpu = u)))

  private val out: Optional[GnirsDynamicConfig, GnirsAcquisitionMirrorMode.Out] =
    GnirsDynamicConfig.acquisitionMirror.andThen(GnirsAcquisitionMirrorMode.out)

  /** No grating is the acquisition mirror in; picking one takes it out with the default setup. */
  val grating: Lens[GnirsDynamicConfig, Option[GnirsGrating]] =
    Lens[GnirsDynamicConfig, Option[GnirsGrating]](out.getOption(_).map(_.grating)): g =>
      d =>
        val mirror  = g.fold(GnirsAcquisitionMirrorMode.In): grating =>
          out.getOption(d).getOrElse(defaultDisperser(d)).copy(grating = grating)
        val updated = d.copy(acquisitionMirror = mirror)
        if prism.get(updated) === prism.get(d) then updated else withDecker(updated)

  /** Only set with the mirror out. */
  val prism: Lens[GnirsDynamicConfig, GnirsPrism] =
    Lens[GnirsDynamicConfig, GnirsPrism](
      out.getOption(_).fold(GnirsPrism.Mirror)(_.prism)
    )(p => d => if out.isEmpty(d) then d else withDecker(out.modify(_.copy(prism = p))(d)))

  /**
   * With the mirror out and no cross-dispersion, a wavelength outside the new filter's range moves
   * to the filter's, since the smart GCAL lookup ignores the filter.
   */
  val filter: Lens[GnirsDynamicConfig, GnirsFilter] =
    Lens[GnirsDynamicConfig, GnirsFilter](_.filter): f =>
      d =>
        val updated = d.copy(filter = f)
        out
          .getOption(updated)
          .filter(o =>
            o.prism === GnirsPrism.Mirror &&
              !f.spectroscopyRange.exists(_.contains(o.wavelength.value))
          )
          .fold(updated)(_ => wavelength.replace(f.centralWavelength)(updated))

  val wavelength: Optional[GnirsDynamicConfig, Wavelength] =
    Optional[GnirsDynamicConfig, Wavelength](out.getOption(_).map(_.wavelength.value)): w =>
      out.modify(_.copy(wavelength = GnirsGratingWavelength(w)))

  /**
   * The ODB takes no arc with the mirror in, and Smart GCAL's imaging rows are night baseline flats
   * only, so just a Smart Flat may keep the mirror in.
   */
  def needsMirrorOut(kind: CalibrationSetType): Boolean =
    kind === CalibrationSetType.Arc || kind === CalibrationSetType.SmartArc ||
      kind === CalibrationSetType.DayBaseline

  /** Takes the mirror out, and swaps an imaging FPU for the default slit. */
  def withDisperser(d: GnirsDynamicConfig): GnirsDynamicConfig =
    val fpu     = d.fpu match
      case GnirsFpu.Other(GnirsFpuOther.Acquisition | GnirsFpuOther.PupilViewer) =>
        GnirsFpu.Spectroscopy.Slit(DefaultSlit)
      case u                                                                     => u
    val updated =
      d.copy(acquisitionMirror = out.getOption(d).getOrElse(defaultDisperser(d)), fpu = fpu)
    if updated === d then d else withDecker(updated)

  /** Like Flamingos 2, a new kind without optics gets the default ones. */
  def forKind(kind: CalibrationSetType, previous: CalibrationSetType)(
    d: GnirsDynamicConfig
  ): GnirsDynamicConfig =
    val optics     =
      if kind === previous || DaytimeCalibrationSets.usesOptics(kind) then d
      else
        d.copy(
          filter = default.filter,
          decker = default.decker,
          fpu = default.fpu,
          acquisitionMirror = default.acquisitionMirror,
          camera = default.camera
        )
    val configured = if needsMirrorOut(kind) then withDisperser(optics) else optics
    exposure.modify(DaytimeCalibrationSets.exposureFor(kind, _, DefaultExposure))(configured)

  val default: GnirsDynamicConfig =
    GnirsDynamicConfig(
      DefaultExposure,
      PosInt.unsafeFrom(1),
      GnirsFilter.Order4,
      GnirsDecker.Acquisition,
      GnirsFpu.Other(GnirsFpuOther.Acquisition),
      GnirsAcquisitionMirrorMode.In,
      GnirsCamera.ShortBlue,
      GnirsFocus.Best,
      GnirsReadMode.Bright
    )

  val defaultSet: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Dark,
      DaytimeCalibrationSets.DefaultCount.some,
      none,
      DaytimeCalibrationSetup.Gnirs(default)
    )

/**
 * The GHOST Instrument Setup the Daytime Calibration Set editor works through. The fiber agitators
 * stay disabled: a daytime calibration has no target.
 */
object GhostDaytimeSetup:
  val setup: Optional[DaytimeCalibrationSetup, GhostDynamicConfig] =
    DaytimeCalibrationSetup.ghost.andThen(DaytimeCalibrationSetup.Ghost.config)

  val DefaultExposure: TimeSpan = TimeSpan.unsafeFromMicroseconds(10_000_000)

  val DefaultDetector: GhostDetector =
    GhostDetector(
      DefaultExposure,
      DaytimeCalibrationSets.DefaultCount,
      GhostBinning.OneByOne,
      GhostReadMode.Slow
    )

  val red: Lens[GhostDynamicConfig, GhostDetector] =
    Lens[GhostDynamicConfig, GhostDetector](_.red.value)(r => _.copy(red = r.asRed))

  val blue: Lens[GhostDynamicConfig, GhostDetector] =
    Lens[GhostDynamicConfig, GhostDetector](_.blue.value)(b => _.copy(blue = b.asBlue))

  val exposureTime: Lens[GhostDetector, TimeSpan]  = Focus[GhostDetector](_.exposureTime)
  val exposureCount: Lens[GhostDetector, PosInt]   = Focus[GhostDetector](_.exposureCount)
  val binning: Lens[GhostDetector, GhostBinning]   = Focus[GhostDetector](_.binning)
  val readMode: Lens[GhostDetector, GhostReadMode] = Focus[GhostDetector](_.readMode)

  /**
   * Smart GCAL supplies a smart row's exposures, and only for 1x1 binning, so a smart kind starts
   * there; the binning stays editable.
   */
  def forKind(kind: CalibrationSetType)(d: GhostDynamicConfig): GhostDynamicConfig =
    def detector(g: GhostDetector): GhostDetector =
      val exposed =
        g.copy(exposureTime =
          DaytimeCalibrationSets.exposureFor(kind, g.exposureTime, DefaultExposure)
        )
      if kind.isSmart then
        exposed.copy(exposureCount = DaytimeCalibrationSets.DefaultCount,
                     binning = GhostBinning.OneByOne
        )
      else exposed
    red.modify(detector).andThen(blue.modify(detector))(d)

  val default: GhostDynamicConfig =
    GhostDynamicConfig(DefaultDetector.asRed, DefaultDetector.asBlue)

  val defaultSet: DaytimeCalibrationSet =
    DaytimeCalibrationSet(
      CalibrationSetType.Dark,
      DaytimeCalibrationSets.DefaultCount.some,
      none,
      DaytimeCalibrationSetup.Ghost(default)
    )

/**
 * The editing rules of Daytime Calibration Sets: every edit leaves a row the ODB accepts, since
 * rows are saved as they change.
 */
object DaytimeCalibrationSets:
  val DefaultWavelength: Wavelength     = Wavelength.unsafeFromIntPicometers(500_000)
  val DefaultExposure: TimeSpan         = TimeSpan.unsafeFromMicroseconds(1_000_000)
  val DefaultCount: PosInt              = PosInt.unsafeFrom(1)
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
    List(
      Instrument.GmosNorth,
      Instrument.GmosSouth,
      Instrument.Flamingos2,
      Instrument.Igrins2,
      Instrument.Gnirs,
      Instrument.Ghost
    )

  /** The diffuser the ODB defaults to: visible for GMOS, IR for the infrared instruments. */
  def defaultDiffuser(instrument: Instrument): GcalDiffuser =
    instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth | Instrument.Ghost => GcalDiffuser.Visible
      case _                                                              => GcalDiffuser.Ir

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

  val GnirsKinds: List[CalibrationSetType] =
    Enumerated[CalibrationSetType].all.filterNot(_ === CalibrationSetType.Bias)

  val GhostKinds: List[CalibrationSetType] =
    Enumerated[CalibrationSetType].all.filterNot(_ === CalibrationSetType.DaytimeObserve)

  /** Flamingos 2 and IGRINS-2 take neither a bias nor a daytime observe. */
  val NoBiasKinds: List[CalibrationSetType] =
    Enumerated[CalibrationSetType].all.filterNot(k =>
      k === CalibrationSetType.Bias || k === CalibrationSetType.DaytimeObserve
    )

  /** The kinds the ODB accepts for an instrument's calibration sets. */
  def kinds(instrument: Instrument): List[CalibrationSetType] =
    instrument match
      case Instrument.Flamingos2 | Instrument.Igrins2 => NoBiasKinds
      case Instrument.Gnirs                           => GnirsKinds
      case Instrument.Ghost                           => GhostKinds
      case _                                          => Enumerated[CalibrationSetType].all

  /** The continuum lamps an instrument can't use: the 100W QH lamp is GHOST only. */
  def excludedContinua(instrument: Instrument): Set[GcalContinuum] =
    instrument match
      case Instrument.Ghost => Set.empty
      case _                => Set(GcalContinuum.QuartzHalogen100W)

  def defaultSet(instrument: Instrument): Option[DaytimeCalibrationSet] =
    instrument match
      case Instrument.Flamingos2 => Flamingos2DaytimeSetup.defaultSet.some
      case Instrument.Igrins2    => Igrins2DaytimeSetup.defaultSet.some
      case Instrument.Gnirs      => GnirsDaytimeSetup.defaultSet.some
      case Instrument.Ghost      => GhostDaytimeSetup.defaultSet.some
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

  /** Only a manual flat or arc carries its own GCAL setup; the lookup supplies a smart kind's. */
  def usesGcalSetup(kind: CalibrationSetType): Boolean = kind.usesGcal && !kind.isSmart

  /**
   * A bias takes no exposure and a smart kind gets its exposure time from the Smart GCAL lookup.
   */
  def usesExposure(kind: CalibrationSetType): Boolean =
    kind =!= CalibrationSetType.Bias && !kind.isSmart

  /** A smart arc on an undispersed setup can never resolve, so the ODB refuses it. */
  def needsDisperser(kind: CalibrationSetType): Boolean = kind === CalibrationSetType.SmartArc

  private def setupFor[D, G, F, U](site: GmosDaytimeSetup[D, G, F, U], kind: CalibrationSetType)(
    d: D
  ): D =
    val optics =
      if needsDisperser(kind) then site.withGrating(d)
      else if usesOptics(kind) then d
      else site.withoutOptics(d)
    site.exposure.modify(exposureFor(kind, _, DefaultExposure))(optics)

  /** No exposure for a kind that takes none; a kind that takes one starts at the default. */
  def exposureFor(
    kind:    CalibrationSetType,
    current: TimeSpan,
    default: TimeSpan
  ): TimeSpan =
    if !usesExposure(kind) then TimeSpan.Zero
    else if current === TimeSpan.Zero then default
    else current

  /**
   * Rewrites a row for a new kind so that it stays valid. A manual row keeps its count; a smart row
   * has none of its own, so leaving a smart kind starts the count over at one rather than keeping
   * the count the lookup resolved.
   */
  def withKind(set: DaytimeCalibrationSet, kind: CalibrationSetType): DaytimeCalibrationSet =
    val count =
      if kind.isSmart then none
      else if set.kind.isSmart then DefaultCount.some
      else set.explicitCount.orElse(DefaultCount.some)
    val setup = set.setup match
      case DaytimeCalibrationSetup.GmosNorth(d)  =>
        DaytimeCalibrationSetup.GmosNorth(setupFor(GmosDaytimeSetup.north, kind)(d))
      case DaytimeCalibrationSetup.GmosSouth(d)  =>
        DaytimeCalibrationSetup.GmosSouth(setupFor(GmosDaytimeSetup.south, kind)(d))
      case DaytimeCalibrationSetup.Flamingos2(d) =>
        DaytimeCalibrationSetup.Flamingos2(Flamingos2DaytimeSetup.forKind(kind, set.kind)(d))
      case DaytimeCalibrationSetup.Igrins2(d)    =>
        DaytimeCalibrationSetup.Igrins2(Igrins2DaytimeSetup.forKind(kind)(d))
      case DaytimeCalibrationSetup.Gnirs(d)      =>
        DaytimeCalibrationSetup.Gnirs(GnirsDaytimeSetup.forKind(kind, set.kind)(d))
      case DaytimeCalibrationSetup.Ghost(d)      =>
        DaytimeCalibrationSetup.Ghost(GhostDaytimeSetup.forKind(kind)(d))
    set.copy(
      kind = kind,
      explicitCount = count,
      count = if kind === set.kind then set.count else DaytimeCalibrationSet.Count.Pending,
      gcal = gcalFor(set.setup.instrument, kind, set.gcal),
      setup = setup
    )

  /**
   * A new row copies the last one, or starts as a single bias (a dark for Flamingos 2, IGRINS-2,
   * GNIRS and GHOST); none for an unsupported instrument.
   */
  def newRow(
    sets:       List[DaytimeCalibrationSet],
    instrument: Instrument
  ): Option[DaytimeCalibrationSet] =
    sets.lastOption.orElse(defaultSet(instrument))
