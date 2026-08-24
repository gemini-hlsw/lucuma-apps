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
import explore.config.offsets.IfuTelescopeConfigsEditor
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
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger

case class GnirsIfuPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GnirsIfu, GnirsIfuInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  isStaffOrAdmin:  Boolean,
  units:           WavelengthUnits
) extends ReactFnProps[GnirsIfuPanel](
      GnirsIfuPanel.component
    )
    with GnirsSpectroscopyPanelProps[GnirsFpuIfu]:

  def mode: ObservingMode              = observingMode.get
  def isCustomized: Boolean            = observingMode.get.isCustomized
  def initialFilter: GnirsFilter       = observingMode.get.initialFilter
  def initialPrism: GnirsPrism         = observingMode.get.initialPrism
  def initialGrating: GnirsGrating     = observingMode.get.initialGrating
  def initialCamera: GnirsCamera       = observingMode.get.initialCamera
  def initialFpu: GnirsFpuIfu          = observingMode.get.initialFpu
  def defaultDecker: GnirsDecker       = observingMode.get.defaultDecker
  def defaultWellDepth: GnirsWellDepth = observingMode.get.defaultWellDepth
  def defaultFaintSkyOffset: Offset    = GnirsAcquisitionMode.Faint.DefaultIfuSkyOffset

  def initialCentralWavelengths: NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig] =
    observingMode.get.initialCentralWavelengths

  def acquisitionAligner
    : Aligner[ObservingMode.GnirsSpectroscopyAcquisition, GnirsSpectroscopyAcquisitionInput] =
    observingMode.zoom(
      ObservingMode.GnirsIfu.acquisition,
      forceAssign(GnirsIfuInput.acquisition.modify)(GnirsSpectroscopyAcquisitionInput())
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
    observingMode.zoom(ObservingMode.GnirsIfu.filter, GnirsIfuInput.filter.modify).view(_.assign)

  def deckerView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsDecker]] =
    observingMode
      .zoom(ObservingMode.GnirsIfu.explicitDecker, GnirsIfuInput.explicitDecker.modify)
      .view(_.orUnassign)

  def fpuView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsFpuIfu] =
    observingMode.zoom(ObservingMode.GnirsIfu.fpu, GnirsIfuInput.fpu.modify).view(_.assign)

  def prismView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsPrism] =
    observingMode.zoom(ObservingMode.GnirsIfu.prism, GnirsIfuInput.prism.modify).view(_.assign)

  def gratingView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsGrating] =
    observingMode
      .zoom(ObservingMode.GnirsIfu.grating, GnirsIfuInput.grating.modify)
      .view(_.assign)

  def cameraView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[GnirsCamera] =
    observingMode.zoom(ObservingMode.GnirsIfu.camera, GnirsIfuInput.camera.modify).view(_.assign)

  def readModeView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsReadMode]] =
    observingMode
      .zoom(ObservingMode.GnirsIfu.explicitReadMode, GnirsIfuInput.explicitReadMode.modify)
      .view(_.orUnassign)

  def wellDepthView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsWellDepth]] =
    observingMode
      .zoom(ObservingMode.GnirsIfu.explicitWellDepth, GnirsIfuInput.explicitWellDepth.modify)
      .view(_.orUnassign)

  def focusMotorStepsView(using
    MonadError[IO, Throwable],
    Effect.Dispatch[IO],
    Logger[IO]
  ): View[Option[GnirsFocusMotorStepsValue]] =
    observingMode
      .zoom(ObservingMode.GnirsIfu.explicitFocusMotorSteps,
            GnirsIfuInput.explicitFocusMotorSteps.modify
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
      .zoom(ObservingMode.GnirsIfu.centralWavelengths, GnirsIfuInput.centralWavelengths.modify)
      .view(_.toList.map(_.toInput).assign)

  def telescopeConfigsEditor(
    prism:      GnirsPrism,
    camera:     GnirsCamera,
    wavelength: Wavelength
  )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
    val fpu = observingMode.get.fpu
    IfuTelescopeConfigsEditor(
      telescopeConfigs = observingMode
        .zoom(ObservingMode.GnirsIfu.telescopeConfigs, GnirsIfuInput.telescopeConfigs.modify)
        .view(_.toList.map(_.toInput).assign),
      presets = gnirs.gnirsIfuTelescopeConfigPresets(fpu),
      defaultConfigs = gnirs.defaultIfuTelescopeConfigs(fpu),
      helpId = "configuration/ifu-spatial-offsets.md".refined,
      presetsReadonly = !permissions.isFullEdit,
      editingReadonly = !permissions.isFullEdit
    )

object GnirsIfuPanel extends GnirsSpectroscopyPanelBuilder[GnirsFpuIfu, GnirsIfuPanel]
