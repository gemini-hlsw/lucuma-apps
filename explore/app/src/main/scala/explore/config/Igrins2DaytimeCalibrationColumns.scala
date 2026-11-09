// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.config.DaytimeCalibrationSetsTable.*
import explore.model.Igrins2DaytimeSetup
import explore.model.inputCodecs
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.schemas.model.DaytimeCalibrationSet

private[config] object Igrins2DaytimeCalibrationColumns:
  def apply(
    setsView: View[List[DaytimeCalibrationSet]],
    editable: Boolean
  ): List[ColDef.Type] =
    val setup: Int => View[Igrins2DynamicConfig] =
      setupView(setsView, Igrins2DaytimeSetup.setup, Igrins2DaytimeSetup.default)

    columns(
      Instrument.Igrins2,
      setsView,
      editable,
      List(
        exposureColumn(editable, inputCodecs.Igrins2CalibrationExposure)(
          setup(_).zoom(Igrins2DynamicConfig.exposure)
        )
      )
    )
