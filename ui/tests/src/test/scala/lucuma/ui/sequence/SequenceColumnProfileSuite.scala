// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.schemas.model.BasicConfiguration
import munit.FunSuite

final class SequenceColumnProfileSuite extends FunSuite:
  import SequenceColumns.*

  private val daytimeMode: BasicConfiguration =
    BasicConfiguration.DaytimeCalibration(Instrument.Flamingos2)

  private val science: SequenceColumnProfile     =
    SequenceColumnProfile(SequenceKind.Science, hasItc = true)
  private val calibration: SequenceColumnProfile =
    SequenceColumnProfile(SequenceKind.Calibration, hasItc = false)

  test("science observations have ITC"):
    assertEquals(SequenceColumnProfile.fromObservation(none, none), science)

  test("daytime roles are calibrations without ITC"):
    assertEquals(
      SequenceColumnProfile.fromObservation(CalibrationRole.DaytimeCalibration.some, none),
      calibration
    )
    assertEquals(
      SequenceColumnProfile.fromObservation(CalibrationRole.DaytimePinhole.some, none),
      calibration
    )

  test("a daytime observing mode is a calibration"):
    assertEquals(
      SequenceColumnProfile.fromObservation(none, daytimeMode.some).kind,
      SequenceKind.Calibration
    )

  test("twilight is science without ITC"):
    assertEquals(
      SequenceColumnProfile.fromObservation(CalibrationRole.Twilight.some, none),
      SequenceColumnProfile(SequenceKind.Science, hasItc = false)
    )

  test("science hides the window cover"):
    assertEquals(hiddenColumns(science), Set(WindowCoverColumnId))

  test("calibrations hide offsets"):
    assertEquals(
      hiddenColumns(calibration.copy(hasItc = true)),
      Set(PColumnId, QColumnId)
    )

  test("no ITC hides S/N and peak in any kind"):
    assertEquals(
      hiddenColumns(science.copy(hasItc = false)),
      Set(WindowCoverColumnId, SNColumnId, PeakColumnId)
    )
    assertEquals(
      hiddenColumns(calibration),
      Set(PColumnId, QColumnId, SNColumnId, PeakColumnId)
    )
