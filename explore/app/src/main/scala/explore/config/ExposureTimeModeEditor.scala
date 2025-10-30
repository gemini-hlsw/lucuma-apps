// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import japgolly.scalajs.react.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps

final case class ExposureTimeModeEditor(
  instrument:       Option[Instrument],
  wavelength:       Option[Wavelength],
  exposureTimeMode: View[ExposureTimeMode],
  scienceMode:      ScienceMode,
  readonly:         Boolean,
  units:            WavelengthUnits,
  calibrationRole:  Option[CalibrationRole],
  forceCount:       Option[PosInt] = None
) extends ReactFnProps(ExposureTimeModeEditor)

object ExposureTimeModeEditor
    extends ReactFnComponent[ExposureTimeModeEditor](props =>
      for
        oEtm <- useStateView(props.exposureTimeMode.get.some)
        _    <- useEffectWithDeps(props.exposureTimeMode.get)(etm => oEtm.set(etm.some))
      yield
        // ExposureTimeModeEditorOptional only ever sets the value, it never updates to None
        val oEtmWithOnMod = oEtm.withOnMod(_.foldMap(props.exposureTimeMode.set))
        ExposureTimeModeEditorOptional(
          props.instrument,
          props.wavelength,
          oEtmWithOnMod,
          props.scienceMode,
          props.readonly,
          props.units,
          props.calibrationRole,
          props.forceCount
        )
    )
