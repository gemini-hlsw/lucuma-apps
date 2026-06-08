// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import observe.model.ObserveStage
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.EpicsCodex.*
import observe.server.EpicsUtil.*
import observe.server.ObserveFailure
import observe.server.Progress
import observe.server.ProgressUtil
import observe.server.failUnlessM
import observe.server.gnirs.GnirsController.GnirsConfig
import org.typelevel.log4cats.Logger

trait GnirsEncoders {
  // GNIRS read modes encode to (lowNoise, digitalAvgs), both taken from lucuma-core:
  // GnirsReadMode.lowNoiseReads (number of low-noise/Fowler reads) and
  // GnirsReadMode.digitalAverages (on-controller digital averages).
  val readModeEncoder: EncodeEpicsValue[GnirsReadMode, (Int, Int)] =
    EncodeEpicsValue(rm => (rm.lowNoiseReads, rm.digitalAverages))

  // Bias voltage for the detector well depth (volts), taken from lucuma-core so it cannot drift
  // from the model (Shallow = 300mV, Deep = 600mV).
  given EncodeEpicsValue[GnirsWellDepth, Double] =
    EncodeEpicsValue(_.bias_level.toVolts.value.toDouble)

  given EncodeEpicsValue[GnirsCamera, String] = EncodeEpicsValue {
    case GnirsCamera.ShortRed  => "ShortRed"
    case GnirsCamera.ShortBlue => "ShortBlue"
    case GnirsCamera.LongRed   => "LongRed"
    case GnirsCamera.LongBlue  => "LongBlue"
  }

  given EncodeEpicsValue[GnirsDecker, String] = EncodeEpicsValue {
    case GnirsDecker.Acquisition           => "Acq"
    case GnirsDecker.PupilViewer           => "PV"
    case GnirsDecker.ShortCamLongSlit      => "SCLong"
    case GnirsDecker.ShortCamCrossDispersed => "SCXD"
    case GnirsDecker.LongCamLongSlit       => "LCLong"
    case GnirsDecker.LongCamCrossDispersed => "LCXD"
  }
}

object GnirsControllerEpics extends GnirsEncoders {

  val DefaultTimeout: TimeSpan = 60.secondTimeSpan
  val ReadoutTimeout: TimeSpan = 120.secondTimeSpan
  val ConfigTimeout: TimeSpan  = 240.secondTimeSpan

  // Short camera name used to build grating/prism EPICS strings.
  private def cameraStr(c: GnirsCamera): String = c match {
    case GnirsCamera.ShortBlue => "SB"
    case GnirsCamera.LongBlue  => "LB"
    case GnirsCamera.ShortRed  => "SR"
    case GnirsCamera.LongRed   => "LR"
  }

  private def gratingStr(g: GnirsGrating): String = g match {
    case GnirsGrating.D10  => "10"
    case GnirsGrating.D32  => "32"
    case GnirsGrating.D111 => "111"
  }

  // GNIRS has two physical filter wheels but the ODB exposes a single logical filter. This maps the
  // logical filter to (filter1, filter2) wheel positions. Reconciled against seqexec's OCS-derived
  // resolution (Gnirs.getFilter1/getFilter2): this reproduces that mapping for all 14 filters. The
  // pupil-viewer override (FW1 = PupilViewer) is applied in setOtherCCParams, since in OCS it is
  // driven by the FPU/decker rather than the filter.
  private def filterWheels(f: GnirsFilter): (String, String) = f match {
    case GnirsFilter.CrossDispersed => ("Open", "XD")
    case GnirsFilter.Order6         => ("Open", "X")
    case GnirsFilter.Order5         => ("Open", "J")
    case GnirsFilter.Order4         => ("Open", "H")
    case GnirsFilter.Order3         => ("Open", "K")
    case GnirsFilter.Order2         => ("Open", "L")
    case GnirsFilter.Order1         => ("Open", "M")
    case GnirsFilter.H2             => ("Open", "H2")
    case GnirsFilter.HNd100x        => ("ND100X", "H")
    case GnirsFilter.H2Nd100x       => ("ND100X", "H2")
    case GnirsFilter.PAH            => ("Open", "PAH")
    case GnirsFilter.Y              => ("Y-MK", "Open")
    case GnirsFilter.J              => ("J-MK", "Open")
    case GnirsFilter.K              => ("K-MK", "Open")
  }

  private def wavelengthNm(w: Wavelength): Double =
    w.toNanometers.value.value.toDouble

  private def focusSteps(v: GnirsFocusMotorStepsValue): Int =
    GnirsFocusMotorStepsValue.Value.get(v).value

  def apply[F[_]: Async](
    epicsSys: => GnirsEpics[F]
  )(using L: Logger[F]): GnirsController[F] =
    new GnirsController[F] {

      private val warnOnDhs: F[Unit] =
        epicsSys.dhsConnected.flatMap(L.warn("GNIRS is not connected to DHS").unlessA)

      private val warnOnArray: F[Unit] =
        epicsSys.arrayActive.flatMap(L.warn("GNIRS detector array is not active").unlessA)

      private val checkDhs: F[Unit] =
        failUnlessM(epicsSys.dhsConnected, ObserveFailure.Execution("GNIRS is not connected to DHS"))

      private val checkArray: F[Unit] =
        failUnlessM(epicsSys.arrayActive,
                    ObserveFailure.Execution("GNIRS detector array is not active")
        )

      private def setAcqMirror(v: String): F[Option[F[Unit]]] =
        smartSetParamF(v, epicsSys.acqMirror.map(removePartName), epicsSys.configCCCmd.setAcqMirror(v))

      private def setSpectrography(
        camera: GnirsCamera,
        out:    GnirsAcquisitionMirrorMode.Out
      ): List[F[Option[F[Unit]]]] = {
        val gratingValue: String = (out.prism, out.grating, camera) match {
          case (GnirsPrism.Sxd, GnirsGrating.D10, GnirsCamera.LongBlue)  => "10/mmLBSX"
          case (GnirsPrism.Lxd, GnirsGrating.D10, GnirsCamera.LongBlue)  => "10/mmLBLX"
          case (GnirsPrism.Lxd, GnirsGrating.D111, GnirsCamera.LongBlue) => "111/mmLBLX"
          case (_, g, c)                                                 => s"${gratingStr(g)}/mm${cameraStr(c)}"
        }

        val prismValue: String = out.prism match {
          case GnirsPrism.Mirror => "MIR"
          case GnirsPrism.Sxd    => s"${cameraStr(camera)}+SXD"
          case GnirsPrism.Lxd    => s"${cameraStr(camera)}+LXD"
        }

        val gratingMode: String = "WAVELENGTH"

        List(
          smartSetParamF(gratingValue, epicsSys.grating.map(removePartName), epicsSys.configCCCmd.setGrating(gratingValue)),
          smartSetParamF(gratingMode, epicsSys.gratingMode, epicsSys.configCCCmd.setGratingMode(gratingMode)),
          smartSetParamF(prismValue, epicsSys.prism.map(removePartName), epicsSys.configCCCmd.setPrism(prismValue))
        )
      }

      private def setDarkCCParams: List[F[Option[F[Unit]]]] =
        List(
          smartSetParamF("Closed", epicsSys.cover.map(removePartName), epicsSys.configCCCmd.setCover("Closed")),
          smartSetParamF("Dark", epicsSys.filter1.map(removePartName), epicsSys.configCCCmd.setFilter1("Dark"))
        )

      private def slitWidthValue(dc: GnirsDynamicConfig): Option[String] =
        dc.fpu match {
          case Left(slit)  =>
            (slit match {
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_10  => "0.10arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_15  => "0.15arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_20  => "0.20arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_30  => "0.30arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_45  => "0.45arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_0_675 => "0.68arcsec"
              case lucuma.core.enums.GnirsFpuSlit.LongSlit_1_00  => "1.00arcsec"
            }).some
          case Right(other) =>
            (other match {
              case lucuma.core.enums.GnirsFpuOther.Acquisition => "Acq"
              case lucuma.core.enums.GnirsFpuOther.PupilViewer => "PV"
              case lucuma.core.enums.GnirsFpuOther.Pinhole1    => "SmPinholes"
              case lucuma.core.enums.GnirsFpuOther.Pinhole3    => "LgPinholes"
            }).some
        }

      private def setOtherCCParams(
        dc: GnirsDynamicConfig
      ): List[F[Option[F[Unit]]]] = {
        val (filterWheel1, filter2Value): (String, String) = filterWheels(dc.filter)
        // The pupil-viewer optic lives in filter wheel 1, so selecting it overrides FW1. Matches
        // seqexec Gnirs.getFilter1: slit == PupilViewer || decker == PupilViewer => Filter1.PupilViewer.
        val pupilViewer: Boolean                           =
          dc.fpu.toOption.contains(GnirsFpuOther.PupilViewer) ||
            dc.decker === GnirsDecker.PupilViewer
        val filter1Value: String                           = if (pupilViewer) "PupilViewer" else filterWheel1
        val cameraValue: String                            = encode(dc.camera)
        val deckerValue: String                            = encode(dc.decker)
        val wavelengthToleranceNm: Double                  = 0.0001

        val acqMirrorAndSpectrography: List[F[Option[F[Unit]]]] = dc.acquisitionMirror match {
          case GnirsAcquisitionMirrorMode.In       => List(setAcqMirror("In"))
          case out @ GnirsAcquisitionMirrorMode.Out(_, _, _) =>
            setAcqMirror("Out") :: setSpectrography(dc.camera, out)
        }

        val focusParam: F[Option[F[Unit]]] = dc.focus match {
          case GnirsFocus.Best      => (epicsSys.configCCCmd.setFocusBest("best focus").some).pure[F]
          case GnirsFocus.Custom(v) =>
            val steps: Int = focusSteps(v.value)
            smartSetParamF(steps, epicsSys.focusEng, epicsSys.configCCCmd.setFocus(steps))
        }

        val common: List[F[Option[F[Unit]]]] = List(
          smartSetParamF("Open", epicsSys.cover.map(removePartName), epicsSys.configCCCmd.setCover("Open")),
          smartSetParamF(filter1Value, epicsSys.filter1.map(removePartName), epicsSys.configCCCmd.setFilter1(filter1Value)),
          smartSetParamF(filter2Value, epicsSys.filter2.map(removePartName), epicsSys.configCCCmd.setFilter2(filter2Value)),
          smartSetParamF(cameraValue, epicsSys.camera.map(removePartName), epicsSys.configCCCmd.setCamera(cameraValue)),
          smartSetParamF(deckerValue, epicsSys.decker.map(removePartName), epicsSys.configCCCmd.setDecker(deckerValue)),
          focusParam,
          smartSetDoubleParamF(wavelengthToleranceNm)(
            wavelengthNm(dc.centralWavelength),
            epicsSys.centralWavelength,
            epicsSys.configCCCmd.setCentralWavelength(wavelengthNm(dc.centralWavelength))
          )
        )

        val slit: F[Option[F[Unit]]] = slitWidthValue(dc)
          .map(sl =>
            smartSetParamF(sl, epicsSys.slitWidth.map(removePartName), epicsSys.configCCCmd.setSlitWidth(sl))
          )
          .getOrElse(none[F[Unit]].pure[F])

        common ::: acqMirrorAndSpectrography ::: List(slit)
      }

      private def setCCParams(config: GnirsConfig): F[Unit] = {
        val params: List[F[Option[F[Unit]]]] =
          if (config.stepType === CoreStepType.Dark) setDarkCCParams
          else setOtherCCParams(config.dynamicConfig)
        executeIfNeeded(params, epicsSys.configCCCmd.post(ConfigTimeout))
      }

      private def setDCParams(config: GnirsConfig): F[Unit] = {
        val dc: GnirsDynamicConfig = config.dynamicConfig
        val expTimeToleranceSec: Double = 0.0001
        // Old Seqexec uses an absolute tolerance of 0.05V, ~16.7% relative for a 0.3V bias.
        val biasToleranceVolts: Double = 0.15

        val (lowNoise, digitalAvgs): (Int, Int) = readModeEncoder.encode(dc.readMode)
        val biasVolts: Double                   = encode(config.staticConfig.wellDepth)

        val params: List[F[Option[F[Unit]]]] = List(
          smartSetDoubleParamF(expTimeToleranceSec)(
            dc.exposure.toSeconds.toDouble,
            epicsSys.exposureTime,
            epicsSys.configDCCmd.setExposureTime(dc.exposure.toSeconds.toDouble)
          ),
          smartSetParamF(dc.coadds.value, epicsSys.numCoadds, epicsSys.configDCCmd.setCoadds(dc.coadds.value)),
          // The instrument reports the negative of the bias that was set.
          smartSetDoubleParamF(biasToleranceVolts)(
            -biasVolts,
            epicsSys.detBias,
            epicsSys.configDCCmd.setDetBias(biasVolts)
          ),
          smartSetParamF(lowNoise, epicsSys.lowNoise, epicsSys.configDCCmd.setLowNoise(lowNoise)),
          smartSetParamF(digitalAvgs, epicsSys.digitalAvgs, epicsSys.configDCCmd.setDigitalAvgs(digitalAvgs))
        )

        executeIfNeeded(params, epicsSys.configDCCmd.post(DefaultTimeout))
      }

      override def applyConfig(config: GnirsConfig): F[Unit] =
        L.debug("Starting GNIRS configuration") *>
          L.debug(s"GNIRS configuration: ${config.show}") *>
          warnOnDhs *>
          warnOnArray *>
          setDCParams(config) *>
          setCCParams(config) *>
          L.debug("Completed GNIRS configuration")

      override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ObserveCommandResult] =
        L.debug(s"Start GNIRS observe, file id $fileId") *>
          checkDhs *>
          checkArray *>
          epicsSys.observeCmd.setLabel(fileId.value) *>
          epicsSys.observeCmd
            .post(expTime +| ReadoutTimeout)
            .flatTap(_ => L.debug("Completed GNIRS observe"))

      override def endObserve: F[Unit] =
        L.debug("Send endObserve to GNIRS") *>
          epicsSys.endObserveCmd.mark *>
          epicsSys.endObserveCmd.post(DefaultTimeout) *>
          L.debug("endObserve sent to GNIRS")

      override def stopObserve: F[Unit] =
        L.debug("Stop GNIRS exposure") *>
          epicsSys.stopCmd.mark *>
          epicsSys.stopCmd.post(DefaultTimeout) *>
          L.debug("GNIRS stop observe command sent")

      override def abortObserve: F[Unit] =
        L.debug("Abort GNIRS exposure") *>
          epicsSys.abortCmd.mark *>
          epicsSys.abortCmd.post(DefaultTimeout) *>
          L.debug("GNIRS abort observe command sent")

      override def observeProgress(total: TimeSpan): Stream[F, Progress] =
        ProgressUtil.obsCountdownWithObsStage[F](
          total,
          TimeSpan.Zero,
          (epicsSys.dcIsPreparing, epicsSys.dcIsAcquiring, epicsSys.dcIsReadingOut)
            .mapN(ObserveStage.fromBooleans)
        )
    }
}
