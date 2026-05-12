// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Site
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

final case class AlienVisitorConfigEditor(
  exposureTimeMode: View[Option[ExposureTimeMode]],
  calibrationRole:  Option[CalibrationRole],
  readonly:         Boolean,
  units:            WavelengthUnits
) extends ReactFnProps(AlienVisitorConfigEditor)

object AlienVisitorConfigEditor
    extends ReactFnComponent[AlienVisitorConfigEditor](props =>
      for
        site <- useStateView[Site](Site.GN)
        cw   <- useStateView[Wavelength](
                  BasicConfiguration.Visitor.defaultCentralWavelength(
                    VisitorObservingModeType.VisitorNorth
                  )
                )
        fov  <- useStateView[Angle](Angle.Angle0)
      yield
        val mode: VisitorObservingModeType = site.get match
          case Site.GN => VisitorObservingModeType.VisitorNorth
          case Site.GS => VisitorObservingModeType.VisitorSouth

        val defaultTimeAndCount: ExposureTimeMode.TimeAndCountMode =
          ExposureTimeMode.TimeAndCountMode(
            TimeSpan.unsafeFromMicroseconds(0L),
            PosInt.unsafeFrom(1),
            cw.get
          )

        val timeAndCountView: View[ExposureTimeMode.TimeAndCountMode] =
          props.exposureTimeMode.zoom {
            case Some(t: ExposureTimeMode.TimeAndCountMode) => t
            case _                                          => defaultTimeAndCount
          } { f => opt =>
            val current = opt match
              case Some(t: ExposureTimeMode.TimeAndCountMode) => t
              case _                                          => defaultTimeAndCount
            f(current).some
          }

        val header: VdomNode =
          React.Fragment(
            FormLabel(htmlFor = "visitor-basic-site".refined)("Site"),
            EnumDropdownView(
              id = "visitor-basic-site".refined,
              value = site,
              disabled = props.readonly
            )
          )

        VisitorConfigFields(
          header = header,
          centralWavelength = cw,
          scienceFov = fov,
          timeAndCount = timeAndCountView.some,
          instrument = mode.instrument,
          calibrationRole = props.calibrationRole,
          units = props.units,
          disabled = props.readonly,
          idPrefix = "visitor-basic".refined
        )
    )
