// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.derived.*
import cats.kernel.Order
import clue.data.syntax.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.Flamingos2ImagingInput
import lucuma.schemas.ObservationDB.Types.Flamingos2LongSlitInput
import lucuma.schemas.ObservationDB.Types.GhostIfuInput
import lucuma.schemas.ObservationDB.Types.GmosNorthImagingInput
import lucuma.schemas.ObservationDB.Types.GmosNorthLongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosSouthImagingInput
import lucuma.schemas.ObservationDB.Types.GmosSouthLongSlitInput
import lucuma.schemas.ObservationDB.Types.GnirsSpectroscopyInput
import lucuma.schemas.ObservationDB.Types.Igrins2LongSlitInput
import lucuma.schemas.ObservationDB.Types.ObservingModeInput
import lucuma.schemas.ObservationDB.Types.VisitorInput
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.display.given
import monocle.Lens

// Observing mode with explicit values merged over defaults. Used for grouping observations by configuration.
enum ObservingModeSummary derives Order:
  // TODO: Update for acquisition customization?
  case GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi,
    exposureTimeMode:  ExposureTimeMode
  )                                                        extends ObservingModeSummary
  // TODO: Update for acquisition customization?
  case GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi,
    exposureTimeMode:  ExposureTimeMode
  )                                                        extends ObservingModeSummary
  // TODO: Update for acquisition customization?
  case Flamingos2LongSlit(
    grating:          Flamingos2Disperser,
    filter:           Flamingos2Filter,
    fpu:              Flamingos2Fpu,
    exposureTimeMode: ExposureTimeMode
  )                                                        extends ObservingModeSummary
  case GmosNorthImaging(
    variant:     ImagingVariant,
    filters:     NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  )                                                        extends ObservingModeSummary
  case GmosSouthImaging(
    variant:     ImagingVariant,
    filters:     NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  )                                                        extends ObservingModeSummary
  case Igrins2LongSlit(exposureTimeMode: ExposureTimeMode) extends ObservingModeSummary
  case Flamingos2Imaging(
    variant: ImagingVariant,
    filters: NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter]
  )                                                        extends ObservingModeSummary
  case GnirsSpectroscopy(
    filter:            GnirsFilter,
    fpu:               GnirsFpu.Spectroscopy,
    prism:             GnirsPrism,
    grating:           GnirsGrating,
    camera:            GnirsCamera,
    centralWavelength: CentralWavelength,
    exposureTimeMode:  ExposureTimeMode
  )                                                        extends ObservingModeSummary
  case GhostIfu(
    resolutionMode: GhostResolutionMode,
    stepCount:      PosInt,
    red:            ObservingMode.GhostIfu.GhostDetector,
    blue:           ObservingMode.GhostIfu.GhostDetector
  )                                                        extends ObservingModeSummary
  case Visitor(
    mode:              VisitorObservingModeType,
    centralWavelength: CentralWavelength,
    agsDiameter:       Angle,
    name:              Option[NonEmptyString]
  )                                                        extends ObservingModeSummary

  def obsModeType: ObservingModeType = this match
    case GmosNorthLongSlit(_, _, _, _, _, _, _) => ObservingModeType.GmosNorthLongSlit
    case GmosSouthLongSlit(_, _, _, _, _, _, _) => ObservingModeType.GmosSouthLongSlit
    case Flamingos2LongSlit(_, _, _, _)         => ObservingModeType.Flamingos2LongSlit
    case Flamingos2Imaging(_, _)                => ObservingModeType.Flamingos2Imaging
    case GmosNorthImaging(_, _, _, _)           => ObservingModeType.GmosNorthImaging
    case GmosSouthImaging(_, _, _, _)           => ObservingModeType.GmosSouthImaging
    case Igrins2LongSlit(_)                     => ObservingModeType.Igrins2LongSlit
    case GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Slit(_)) => ObservingModeType.GnirsLongSlit
    case GnirsSpectroscopy(fpu = GnirsFpu.Spectroscopy.Ifu(_))  => ObservingModeType.GnirsIfu
    case GhostIfu(_, _, _, _)                   => ObservingModeType.GhostIfu
    case Visitor(mode, _, _, _)                 => mode

  def toInput: ObservingModeInput = this match
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, etm) =>
      ObservingModeInput.GmosNorthLongSlit(
        GmosNorthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign,
          exposureTimeMode = etm.toInput.assign
        )
      )
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, etm) =>
      ObservingModeInput.GmosSouthLongSlit(
        GmosSouthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign,
          exposureTimeMode = etm.toInput.assign
        )
      )
    case Flamingos2LongSlit(disperser, filter, fpu, etm)                                   =>
      ObservingModeInput.Flamingos2LongSlit(
        Flamingos2LongSlitInput(
          disperser = disperser.assign,
          filter = filter.assign,
          fpu = fpu.assign,
          exposureTimeMode = etm.toInput.assign
        )
      )
    case GmosNorthImaging(variant, filters, ampReadMode, roi)                              =>
      ObservingModeInput.GmosNorthImaging(
        GmosNorthImagingInput(
          variant = variant.toInput.assign,
          filters = filters.toList.map(_.toInput).assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )
    case GmosSouthImaging(variant, filters, ampReadMode, roi)                              =>
      ObservingModeInput.GmosSouthImaging(
        GmosSouthImagingInput(
          // no etm yet
          variant = variant.toInput.assign,
          filters = filters.toList.map(_.toInput).assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        )
      )
    case Flamingos2Imaging(variant, filters)                                               =>
      ObservingModeInput.Flamingos2Imaging(
        Flamingos2ImagingInput(
          variant = variant.toInput.assign,
          filters = filters.toList.map(_.toInput).assign
        )
      )
    case Igrins2LongSlit(etm)                                                              =>
      ObservingModeInput.Igrins2LongSlit(
        Igrins2LongSlitInput(exposureTimeMode = etm.toInput.assign)
      )
    case GnirsSpectroscopy(filter, fpu, prism, grating, camera, centralWavelength, etm)        =>
      ObservingModeInput.GnirsSpectroscopy(
        GnirsSpectroscopyInput(
          filter = filter.assign,
          fpuSlit = GnirsFpu.Spectroscopy.slit.getOption(fpu).orUnassign,
          fpuIfu = GnirsFpu.Spectroscopy.ifu.getOption(fpu).orUnassign,
          prism = prism.assign,
          grating = grating.assign,
          camera = camera.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          exposureTimeMode = etm.toInput.assign
        )
      )
    case GhostIfu(resolutionMode, stepCount, red, blue)                                    =>
      ObservingModeInput.GhostIfu(
        GhostIfuInput(
          stepCount = stepCount.assign,
          resolutionMode = resolutionMode.assign,
          red = red.toInput.assign,
          blue = blue.toInput.assign
        )
      )
    case Visitor(mode, centralWavelength, agsDiameter, name)                               =>
      ObservingModeInput.Visitor(
        VisitorInput(
          mode = mode.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          agsDiameter = agsDiameter.toInput.assign,
          name = name.orUnassign
        )
      )

  // For the dropdown for selecting an existing configuration. The line breaks will be a single space in the dropdown,
  // but will be rendered with line breaks in the dropdown panel.
  def dropdownEntry: String =
    def formatSn(sn: SignalToNoise): String        =
      f"S/N = ${sn.toBigDecimal}%.0f"
    def formatAt(wv: Wavelength): String           =
      f"at ${wv.toNanometers}%.1f nm"
    def formatCount(time: TimeSpan, count: PosInt) =
      s"$count x ${time.toSeconds.toInt}s"

    extension (etm: ExposureTimeMode)
      def formatSpec: String    = etm match
        case ExposureTimeMode.SignalToNoiseMode(value, at)      =>
          s"${formatSn(value)} ${formatAt(at)}"
        case ExposureTimeMode.TimeAndCountMode(time, count, at) =>
          s"${formatCount(time, count)} ${formatAt(at)}"
      def formatImaging: String = etm match
        case ExposureTimeMode.SignalToNoiseMode(value, _)      =>
          formatSn(value)
        case ExposureTimeMode.TimeAndCountMode(time, count, _) =>
          formatCount(time, count)

    this match
      case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, etm) =>
        val cwvStr    = formatAt(centralWavelength.value)
        val filterStr = filter.fold("None")(_.shortName)
        s"GMOS-N Longslit\n${grating.shortName} $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName} (${etm.formatSpec})"
      case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, etm) =>
        val cwvStr    = formatAt(centralWavelength.value)
        val filterStr = filter.fold("None")(_.shortName)
        s"GMOS-S Longslit\n${grating.shortName} $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName} (${etm.formatSpec})"
      case Flamingos2LongSlit(grating, filter, fpu, etm)                                     =>
        s"Flamingos2 Longslit\n${grating.shortName} ${filter.shortName} ${fpu.shortName} (${etm.formatSpec})"
      case Flamingos2Imaging(variant, filters)                                               =>
        val filterStr =
          filters
            .map(i => s"${i.filter.shortName} (${i.exposureTimeMode.formatImaging})")
            .toList
            .mkString("\n")
        s"Flamingos2 Imaging ${variant.variantType.name}\n$filterStr"
      case GmosNorthImaging(variant, filters, ampReadMode, roi)                              =>
        val filterStr =
          filters
            .map(i => s"${i.filter.shortName} (${i.exposureTimeMode.formatImaging})")
            .toList
            .mkString("\n")
        s"GMOS-N Imaging ${variant.variantType.name}\n$filterStr\n${ampReadMode.shortName} ${roi.shortName}"
      case GmosSouthImaging(variant, filters, ampReadMode, roi)                              =>
        val filterStr = filters
          .map(i => s"${i.filter.shortName} (${i.exposureTimeMode.formatImaging})")
          .toList
          .mkString("\n")
        s"GMOS-S Imaging ${variant.variantType.name}\n$filterStr\n${ampReadMode.shortName} ${roi.shortName}"
      case Igrins2LongSlit(etm)                                                              =>
        s"IGRINS-2 Longslit (${etm.formatSpec})"
      case GnirsSpectroscopy(_, fpu, prism, grating, camera, centralWavelength, etm)             =>
        val prismSummary: String      = prism match
          case GnirsPrism.Mirror => ""
          case p                 => s" ${p.shortName}"
        val wavelengthSummary: String =
          f"${centralWavelength.value.toMicrometers.value}%.2fµm"
        val fpuSummary: String        = fpu match
          case GnirsFpu.Spectroscopy.Slit(s) => s"${s.shortName} slit"
          case GnirsFpu.Spectroscopy.Ifu(i)  => i.shortName
        s"GNIRS Spectroscopy\n${camera.shortName} ${grating.longName} @ $wavelengthSummary$prismSummary $fpuSummary (${etm.formatSpec})"
      case GhostIfu(resolutionMode, steps, red, blue)                                        =>
        extension (detector: ObservingMode.GhostIfu.GhostDetector)
          def summary: String =
            val tAndC        = detector.timeAndCount
            val tAndCSummary =
              s"(${tAndC.count} x ${tAndC.time.toSeconds.toInt}s/step ${formatAt(tAndC.at)})"
            s"${detector.readMode.shortName} ${detector.binning.shortName} $tAndCSummary"
        s"GHOST IFU ${resolutionMode.shortName}\n${steps.value} steps\nBlue: ${blue.summary}\nRed: ${red.summary}"
      case Visitor(VisitorObservingModeType.VisitorNorth, _, _, Some(name))                  =>
        s"Gemini North Visitor: ${name.value}"
      case Visitor(VisitorObservingModeType.VisitorSouth, _, _, Some(name))                  =>
        s"Gemini South Visitor: ${name.value}"
      case Visitor(mode, _, _, _)                                                            =>
        mode.shortName

object ObservingModeSummary:
  // These are needed for derviving ObservingModeSummary's Order instance, but actual order is immaterial
  private given Order[ExposureTimeMode.SignalToNoiseMode] = Order.by(sn => (sn.value, sn.at))
  private given Order[ExposureTimeMode.TimeAndCountMode]  =
    Order.by(tc => (tc.time, tc.count, tc.at))

  extension (etm: ExposureTimeMode)
    private def withMininumAt: ExposureTimeMode = ExposureTimeMode.at.replace(Wavelength.Min)(etm)

  // To make comparisons equal and grouping work, we'll set the wavelangthAt of all the filters to the minimum wavelength.
  // The @ wavelength is not actually used for imaging, but some reaason observations have differing values for it.
  def normalizeImagingFiltersAt[I, F](
    filters: NonEmptyList[I],
    lens:    Lens[I, ExposureTimeMode]
  ): NonEmptyList[I] =
    filters.map(i => lens.modify(_.withMininumAt)(i))

  def fromObservingMode(observingMode: ObservingMode): ObservingModeSummary =
    observingMode match
      case n: ObservingMode.GmosNorthLongSlit  =>
        GmosNorthLongSlit(
          n.grating,
          n.filter,
          n.fpu,
          n.centralWavelength,
          n.ampReadMode,
          n.roi,
          n.exposureTimeMode
        )
      case s: ObservingMode.GmosSouthLongSlit  =>
        GmosSouthLongSlit(
          s.grating,
          s.filter,
          s.fpu,
          s.centralWavelength,
          s.ampReadMode,
          s.roi,
          s.exposureTimeMode
        )
      case f: ObservingMode.Flamingos2LongSlit =>
        Flamingos2LongSlit(f.disperser, f.filter, f.fpu, f.exposureTimeMode)
      case f: ObservingMode.Flamingos2Imaging  =>
        Flamingos2Imaging(f.variant, f.filters)
      case n: ObservingMode.GmosNorthImaging   =>
        GmosNorthImaging(
          n.variant,
          normalizeImagingFiltersAt(n.filters,
                                    ObservingMode.GmosNorthImaging.ImagingFilter.exposureTimeMode
          ),
          n.ampReadMode,
          n.roi
        )
      case s: ObservingMode.GmosSouthImaging   =>
        GmosSouthImaging(
          s.variant,
          normalizeImagingFiltersAt(s.filters,
                                    ObservingMode.GmosSouthImaging.ImagingFilter.exposureTimeMode
          ),
          s.ampReadMode,
          s.roi
        )
      case i: ObservingMode.Igrins2LongSlit    =>
        Igrins2LongSlit(i.exposureTimeMode)
      case g: ObservingMode.GnirsSpectroscopy      =>
        GnirsSpectroscopy(g.filter,
                      g.fpu,
                      g.prism,
                      g.grating,
                      g.camera,
                      g.centralWavelength,
                      g.exposureTimeMode
        )
      case g: ObservingMode.GhostIfu           =>
        GhostIfu(g.resolutionMode, g.stepCount, g.red, g.blue)
      case v: ObservingMode.Visitor            =>
        Visitor(v.mode, v.centralWavelength, v.agsDiameter, v.name)

  // TODO Can we unify this logic with the one in explore/model/src/main/scala/explore/model/display.scala
  // and/or observe/web/client-model/src/main/scala/observe/ui/model/ObsSummary.scala?
  // I believe this is used by the ObsBadge and only the ObsBadge
  given Display[ObservingModeSummary] = Display.byShortName:
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, _) =>
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-N Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi, _) =>
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-S Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case Flamingos2LongSlit(grating, filter, fpu, _)                                     =>
      s"Flamingos2 Longslit ${grating.shortName} ${filter.shortName} ${fpu.shortName}"
    case GmosNorthImaging(variant, filters, ampReadMode, roi)                            =>
      val filterStr = filters.map(_.filter.shortName).toList.mkString(", ")
      s"GMOS-N Imaging ${variant.variantType.name} $filterStr ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthImaging(variant, filters, ampReadMode, roi)                            =>
      val filterStr = filters.map(_.filter.shortName).toList.mkString(", ")
      s"GMOS-S Imaging ${variant.variantType.name} $filterStr ${ampReadMode.shortName} ${roi.shortName}"
    case Flamingos2Imaging(variant, filters)                                             =>
      val filterStr = filters.map(_.filter.shortName).toList.mkString(", ")
      s"Flamingos2 Imaging ${variant.variantType.name} $filterStr"
    case Igrins2LongSlit(_)                                                              =>
      s"IGRINS-2 Longslit"
    case GnirsSpectroscopy(_, fpu, prism, grating, camera, centralWavelength, _)             =>
      val prismSummary: String      = prism match
        case GnirsPrism.Mirror => ""
        case p                 => s" ${p.shortName}"
      val wavelengthSummary: String =
        f"${centralWavelength.value.toMicrometers.value}%.2fµm"
      val fpuSummary: String        = fpu match
        case GnirsFpu.Spectroscopy.Slit(s) => s"${s.shortName} slit"
        case GnirsFpu.Spectroscopy.Ifu(i)  => i.shortName
      s"GNIRS Spectroscopy ${camera.shortName} ${grating.longName} @ $wavelengthSummary$prismSummary $fpuSummary"
    case GhostIfu(resolutionMode, _, _, _)                                               =>
      s"GHOST IFU ${resolutionMode.shortName}"
    case Visitor(VisitorObservingModeType.VisitorNorth, _, _, Some(name))                =>
      s"Gemini North Visitor: ${name.value}"
    case Visitor(VisitorObservingModeType.VisitorSouth, _, _, Some(name))                =>
      s"Gemini South Visitor: ${name.value}"
    case Visitor(mode, _, _, _)                                                          =>
      mode.shortName

  object GmosNorthImaging:
    given Order[GmosNorthImaging] =
      Order.by(x => (x.variant.variantType, x.filters.map(_.filter), x.ampReadMode, x.roi))

  object GmosSouthImaging:
    given Order[GmosSouthImaging] =
      Order.by(x => (x.variant.variantType, x.filters.map(_.filter), x.ampReadMode, x.roi))

  object Flamingos2Imaging:
    given Order[Flamingos2Imaging] =
      Order.by(x => x.filters.map(_.filter))

  object Visitor:
    given Order[Visitor] =
      Order.by(x => (x.mode, x.centralWavelength.value, x.agsDiameter.toMicroarcseconds, x.name))
