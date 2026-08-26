// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import explore.model.ArchiveDuplication
import explore.model.ArchiveMatch
import explore.model.Observation
import lucuma.core.model.Program

trait OdbArchiveApi[F[_]]:
  // The Archive Duplication header for every observation in a program.
  def programArchiveDuplications(
    programId: Program.Id
  ): F[Map[Observation.Id, ArchiveDuplication]]

  // One observation's header.
  def observationArchiveDuplication(obsId: Observation.Id): F[Option[ArchiveDuplication]]

  def observationArchiveMatches(obsId: Observation.Id): F[List[ArchiveMatch]]

  // Runs the Archive Duplication Search for one observation and returns its new result.
  def refreshArchiveDuplication(obsId: Observation.Id): F[ArchiveDuplication]
