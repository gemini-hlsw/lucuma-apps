// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import lucuma.core.enums.CalibrationRole
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.syntax.calibration.*

enum SequenceKind derives Eq:
  case Science, Calibration

/** What a sequence has, from which the table decides which columns to show. */
case class SequenceColumnProfile(kind: SequenceKind, hasItc: Boolean) derives Eq

object SequenceColumnProfile:
  def fromObservation(
    calibrationRole: Option[CalibrationRole],
    observingMode:   Option[BasicConfiguration]
  ): SequenceColumnProfile =
    SequenceColumnProfile(
      if calibrationRole.isDaytime(observingMode) then SequenceKind.Calibration
      else SequenceKind.Science,
      calibrationRole.needsITC
    )
