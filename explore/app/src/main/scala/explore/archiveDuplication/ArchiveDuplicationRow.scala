// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.archiveDuplication

import cats.syntax.all.*
import explore.model.ArchiveDuplicationEntry
import explore.model.ArchiveMatch
import explore.model.Observation

/**
 * A row of the Archive Duplication Search table: one observation, or one of the Archive Matches
 * nested beneath it. A status row stands in for an observation's matches while they are being
 * fetched or when the fetch failed — it is also what makes a collapsed row expandable before its
 * matches are known.
 */
enum ArchiveDuplicationRow:
  case ObsRow(entry: ArchiveDuplicationEntry)
  case MatchRow(obsId: Observation.Id, archiveMatch: ArchiveMatch)
  case StatusRow(obsId: Observation.Id, message: String, loading: Boolean)

  def fold[A](fObs: ObsRow => A, fMatch: MatchRow => A, fStatus: StatusRow => A): A =
    this match
      case r: ObsRow    => fObs(r)
      case r: MatchRow  => fMatch(r)
      case r: StatusRow => fStatus(r)

  def observationId: Observation.Id =
    fold(_.entry.id, _.obsId, _.obsId)

  def optEntry: Option[ArchiveDuplicationEntry] =
    fold(_.entry.some, _ => none, _ => none)

  def optMatch: Option[ArchiveMatch] =
    fold(_ => none, _.archiveMatch.some, _ => none)

  def isObsRow: Boolean =
    fold(_ => true, _ => false, _ => false)

  def rowId: String =
    fold(
      _.entry.id.toString,
      r => s"${r.obsId}-${r.archiveMatch.name}",
      r => s"${r.obsId}-status"
    )
