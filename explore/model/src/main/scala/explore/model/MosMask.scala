// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Instrument
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference

/**
 * A MOS mask any program uploaded, as offered to staff for a daytime calibration set.
 */
case class MosMask(
  id:               Attachment.Id,
  name:             NonEmptyString,
  programId:        Program.Id,
  programReference: Option[ProgramReference]
) derives Eq:
  def programLabel: String = programReference.fold(programId.show)(_.label)
  def label: String        = s"${name.value} · $programLabel"

/** The MOS masks of each instrument, loaded on demand and kept for the session. */
type MosMasks = Map[Instrument, Pot[List[MosMask]]]
