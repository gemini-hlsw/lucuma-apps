// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import clue.data.*
import clue.data.syntax.*
import crystal.react.View
import explore.common.Aligner
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.enums.GnirsDecker
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Program
import lucuma.core.model.sequence.gnirs
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger

case class GnirsLongSlitPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GnirsLongSlit, GnirsLongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  isStaffOrAdmin:  Boolean,
  units:           WavelengthUnits
) extends ReactFnProps[GnirsLongSlitPanel](
      GnirsLongSlitPanel.component
    )
    with GnirsSpectroscopyPanelProps[GnirsFpuSlit]:

  def mode: ObservingMode              = observingMode.get
  def isCustomized: Boolean            = observingMode.get.isCustomized
  def initialFilter: GnirsFilter       = observingMode.get.initialFilter
  def initialPrism: GnirsPrism         = observingMode.get.initialPrism
  def initialGrating: GnirsGrating     = observingMode.get.initialGrating
  def initialCamera: GnirsCamera       = observingMode.get.initialCamera
  def initialFpu: GnirsFpuSlit         = observingMode.get.initialFpu
  def defaultDecker: GnirsDecker       = observingMode.get.defaultDecker
  def defaultWellDepth: GnirsWellDepth = observingMode.get.defaultWellDepth
  def defaultFaintSkyOffset: Offset    = GnirsAcquisitionMode.Faint.DefaultSlitSkyOffset

  def initialCentralWavelengths: NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig] =
    observingMode.get.initialCentralWavelengths

  def acquisitionAligner
    : Aligner[ObservingMode.GnirsSpectroscopyAcquisition, GnirsSpectroscopyAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GnirsLongSlit.acquisition,
      forceAssign(GnirsLongSlitInput.acquisition.modify)(GnirsSpectroscopyAcquisitionInput())
    )

  def revertCustomizations(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): Callback =
    observingMode.view(_.toInput).mod(_.revertCustomizations)

  def filterView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsFilter] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.filter, GnirsLongSlitInput.filter.modify)
      .view(_.assign)

  def deckerView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsDecker]] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.explicitDecker, GnirsLongSlitInput.explicitDecker.modify)
      .view(_.orUnassign)

  def fpuView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsFpuSlit] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.fpu, GnirsLongSlitInput.fpu.modify)
      .view(_.assign)

  def prismView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsPrism] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.prism, GnirsLongSlitInput.prism.modify)
      .view(_.assign)

  def gratingView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsGrating] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.grating, GnirsLongSlitInput.grating.modify)
      .view(_.assign)

  def cameraView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsCamera] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.camera, GnirsLongSlitInput.camera.modify)
      .view(_.assign)

  def readModeView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsReadMode]] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.explicitReadMode,
            GnirsLongSlitInput.explicitReadMode.modify
      )
      .view(_.orUnassign)

  def wellDepthView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsWellDepth]] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.explicitWellDepth,
            GnirsLongSlitInput.explicitWellDepth.modify
      )
      .view(_.orUnassign)

  def focusMotorStepsView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsFocusMotorStepsValue]] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.explicitFocusMotorSteps,
            GnirsLongSlitInput.explicitFocusMotorSteps.modify
      )
      .view(_.map(_.value.value).orUnassign)

  def centralWavelengthsView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[
    NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig]
  ] =
    observingMode
      .zoom(ObservingMode.GnirsLongSlit.centralWavelengths,
            GnirsLongSlitInput.centralWavelengths.modify
      )
      .view(_.toList.map(_.toInput).assign)

  def telescopeConfigsEditor(
    prism:      GnirsPrism,
    camera:     GnirsCamera,
    wavelength: Wavelength
  )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
    SlitTelescopeConfigsEditor(
      explicitValue = observingMode
        .zoom(ObservingMode.GnirsLongSlit.explicitTelescopeConfigs,
              GnirsLongSlitInput.explicitTelescopeConfigs.modify
        )
        .view(_.map(_.toInput).orUnassign),
      defaultValue = observingMode.get.defaultTelescopeConfigs,
      defaultForPreset =
        gnirs.defaultSlitTelescopeConfigs(_, prism, camera, GnirsGratingWavelength(wavelength)),
      helpId = "configuration/slit-spatial-offsets.md".refined,
      presetsReadonly = !permissions.isFullEdit,
      editingReadonly = !permissions.isFullEdit
    )

object GnirsLongSlitPanel extends GnirsSpectroscopyPanelBuilder[GnirsFpuSlit, GnirsLongSlitPanel]
