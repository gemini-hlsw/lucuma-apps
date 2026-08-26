// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.archiveDuplication

import cats.syntax.all.*
import crystal.Pot
import explore.model.ArchiveDuplicationEntry
import explore.model.ArchiveMatch
import explore.model.Observation
import lucuma.react.table.Expandable

/**
 * A row of the Archive Duplication Search table: one observation, or one of the Archive Matches
 * nested beneath it.
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

object ArchiveDuplicationRow:
  /**
   * The nested rows beneath one observation. A row with no matches has none; otherwise a status row
   * stands in until the matches are fetched, which is also what makes a collapsed row expandable.
   */
  def subRowsFor(
    entry:      ArchiveDuplicationEntry,
    matchCache: Map[Observation.Id, Pot[List[ArchiveMatch]]]
  ): List[Expandable[ArchiveDuplicationRow]] =
    if !entry.hasMatches then Nil
    else
      matchCache.get(entry.id) match
        case Some(Pot.Ready(found)) =>
          found.map(m => Expandable(MatchRow(entry.id, m)))
        case Some(Pot.Error(t))     =>
          List(Expandable(StatusRow(entry.id, s"Could not load matches: ${t.getMessage}", false)))
        case _                      =>
          List(Expandable(StatusRow(entry.id, "Loading matches…", true)))
