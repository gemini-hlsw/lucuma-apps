// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.derived.*
import cats.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.*
import lucuma.core.enums.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import monocle.Getter

sealed trait ItcInstrumentConfig derives Eq:
  def instrument: Instrument

  def instrumentLabel: String = instrument.longName

  type Grating
  val grating: Grating
  def gratingDisplay: Display[Grating]
  def gratingStr: String = gratingDisplay.shortName(grating)
  def filterStr: String

  type FPU
  val fpu: FPU

  type Filter
  val filter: Filter

  val site: Site

  def hasFilter: Boolean

  val mode: ScienceMode

  type Override
  def modeOverrides: Option[Override] = None

  // Used by the modes tables to replace the exposure time mode in the ITC config when the user changes it in the UI.
  // GHOST NOTE: For GHOST, this will set both ETMs (one for each arm) to the same value.
  def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig

object ItcInstrumentConfig:
  // GMOS suporta a total wavelength range of 360-1030 nm
  // https://www.gemini.edu/instrumentation/gmos
  // the center is 360 + (1030 - 360) / 2 = 695
  val GmosFallbackCW: CentralWavelength =
    CentralWavelength(Wavelength.fromIntNanometers(695).get)

  // Used when decoding the spectroscopy and imaging modes tables since there is not yet an ETM
  val PlaceholderEtm: ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(SignalToNoise.Max, Wavelength.Min)

  case class GmosNorthSpectroscopy(
    grating:                    GmosNorthGrating,
    fpu:                        GmosNorthFpu,
    filter:                     Option[GmosNorthFilter],
    exposureTimeMode:           ExposureTimeMode,
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GmosNorthGrating
    type Filter   = Option[GmosNorthFilter]
    type FPU      = GmosNorthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy

    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.fold("none")(_.shortName)
    val instrument                       = Instrument.GmosNorth
    val site                             = Site.GN
    val hasFilter                        = filter.isDefined
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GmosSouthSpectroscopy(
    grating:                    GmosSouthGrating,
    fpu:                        GmosSouthFpu,
    filter:                     Option[GmosSouthFilter],
    exposureTimeMode:           ExposureTimeMode,
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GmosSouthGrating
    type Filter   = Option[GmosSouthFilter]
    type FPU      = GmosSouthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.fold("none")(_.shortName)
    val instrument                       = Instrument.GmosSouth
    val site                             = Site.GS
    val hasFilter                        = filter.isDefined
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GmosNorthImaging(
    filter:                     GmosNorthFilter,
    exposureTimeMode:           ExposureTimeMode,
    override val modeOverrides: Option[InstrumentOverrides.GmosImaging]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = Unit
    type Filter   = GmosNorthFilter
    type FPU      = Unit
    type Override = InstrumentOverrides.GmosImaging

    val gratingDisplay: Display[Grating] = Display.byShortName(_ => "")
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.GmosNorth
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Imaging
    val grating: Grating                 = ()
    val fpu: FPU                         = ()

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GmosSouthImaging(
    filter:                     GmosSouthFilter,
    exposureTimeMode:           ExposureTimeMode,
    override val modeOverrides: Option[InstrumentOverrides.GmosImaging]
  ) extends ItcInstrumentConfig derives Eq {

    type Grating  = Unit
    type Filter   = GmosSouthFilter
    type FPU      = Unit
    type Override = InstrumentOverrides.GmosImaging
    val gratingDisplay: Display[Grating] = Display.byShortName(_ => "")
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.GmosSouth
    val site                             = Site.GS
    val hasFilter                        = true
    val mode                             = ScienceMode.Imaging
    val grating: Grating                 = ()
    val fpu: FPU                         = ()

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class Flamingos2Spectroscopy(
    grating:          Flamingos2Disperser,
    filter:           Flamingos2Filter,
    fpu:              Flamingos2Fpu,
    exposureTimeMode: ExposureTimeMode
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = Flamingos2Disperser
    type Filter   = Flamingos2Filter
    type FPU      = Flamingos2Fpu
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.Flamingos2
    val site                             = Site.GS
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GhostIfu(
    resolutionMode:   GhostResolutionMode,
    binning:          GhostBinning,
    exposureTimeMode: ExposureTimeMode
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = Unit
    type Filter   = Unit
    type FPU      = Unit
    type Override = Unit

    val binningStr = binning match
      case GhostBinning.OneByOne   => "1x1"
      case GhostBinning.OneByTwo   => "1x2"
      case GhostBinning.OneByFour  => "1x4"
      case GhostBinning.OneByEight => "1x8"
      case GhostBinning.TwoByTwo   => "2x2"
      case GhostBinning.TwoByFour  => "2x4"
      case GhostBinning.TwoByEight => "2x8"
      case GhostBinning.FourByFour => "4x4"

    val modeStr = resolutionMode match
      case GhostResolutionMode.High     => "HR"
      case GhostResolutionMode.Standard => "SR"

    val grating                          = ()
    val filter                           = ()
    val fpu: Unit                        = ()
    val gratingDisplay: Display[Grating] = Display.byShortName(_ => "Echelle")
    val filterStr: String                = "none"
    val instrument                       = Instrument.Ghost
    override def instrumentLabel: String = s"${instrument.longName} $modeStr $binningStr"
    val site                             = Site.GS
    val hasFilter                        = false
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  // Igrins2 is a static instrument with no configurable grating, filter, or FPU.
  case class Igrins2Spectroscopy(exposureTimeMode: ExposureTimeMode) extends ItcInstrumentConfig
      derives Eq {
    type Grating  = NonEmptyString
    type Filter   = Option[NonEmptyString]
    type FPU      = NonEmptyString
    type Override = Unit
    val grating: NonEmptyString          = "HK".refined
    val filter: Option[NonEmptyString]   = none
    val fpu: NonEmptyString              = "0.33\"".refined
    val gratingDisplay: Display[Grating] = Display.byShortName(_.value)
    val filterStr: String                = "none"
    val instrument                       = Instrument.Igrins2
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GpiSpectroscopy(
    grating:          GpiDisperser,
    filter:           GpiFilter,
    exposureTimeMode: ExposureTimeMode
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GpiDisperser
    type Filter   = GpiFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val fpu                              = ()
    val instrument                       = Instrument.Gpi
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  case class GnirsSpectroscopy(
    grating:          GnirsDisperser,
    filter:           GnirsFilter,
    exposureTimeMode: ExposureTimeMode
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GnirsDisperser
    type Filter   = GnirsFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val fpu                              = ()
    val instrument                       = Instrument.Gnirs
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  // Used for Instruments not fully defined
  case class GenericSpectroscopy(
    i:                Instrument,
    grating:          String,
    filter:           NonEmptyString,
    exposureTimeMode: ExposureTimeMode
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = String
    type Filter   = NonEmptyString
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(identity)
    val filterStr: String                = filter.value
    val fpu                              = ()
    val instrument                       = i
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

    def setSingleExposureTimeMode(etm: ExposureTimeMode): ItcInstrumentConfig =
      copy(exposureTimeMode = etm)
  }

  val instrument: Getter[ItcInstrumentConfig, Instrument] =
    Getter[ItcInstrumentConfig, Instrument](_.instrument)

  def grating: Getter[ItcInstrumentConfig, ItcInstrumentConfig#Grating] =
    Getter[ItcInstrumentConfig, ItcInstrumentConfig#Grating](_.grating)

  def filter: Getter[ItcInstrumentConfig, ItcInstrumentConfig#Filter] =
    Getter[ItcInstrumentConfig, ItcInstrumentConfig#Filter](_.filter)
