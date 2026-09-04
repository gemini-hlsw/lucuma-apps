// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ProposalStatus
import lucuma.core.math.Coordinates
import lucuma.schemas.model.enums.ArchiveDuplicationState

enum MatchCountCell derives Eq:
  case Loading
  case Searching
  case NotChecked
  case NotApplicable
  case Counted(count: NonNegInt, saturated: Boolean, stale: Boolean)
  case SearchFailed(
    count:     NonNegInt,
    saturated: Boolean,
    error:     Option[NonEmptyString],
    stale:     Boolean
  )
  case CallFailed(message: String)

object MatchCountCell:
  def fromPot(pot: Pot[ArchiveDuplication], headersLoaded: Boolean): MatchCountCell =
    pot match
      case Pot.Pending      => if headersLoaded then Searching else Loading
      case Pot.Error(t)     => CallFailed(Option(t.getMessage).getOrElse("Unknown error"))
      case Pot.Ready(dupli) =>
        dupli.state match
          case ArchiveDuplicationState.NotChecked    => NotChecked
          case ArchiveDuplicationState.NotApplicable => NotApplicable
          case ArchiveDuplicationState.Checked       =>
            Counted(dupli.matchCount, dupli.saturated, dupli.stale)
          case ArchiveDuplicationState.Error         =>
            SearchFailed(dupli.matchCount, dupli.saturated, dupli.error, dupli.stale)

/** One observation's row in the Archive Duplication Search tile. */
case class ArchiveDuplicationEntry(
  observation:    Observation,
  basePosition:   Option[Coordinates],
  duplication:    Pot[ArchiveDuplication],
  matchCountCell: MatchCountCell
) derives Eq:
  val id: Observation.Id       = observation.id
  lazy val matchCount: Int     = duplication.toOption.foldMap(_.matchCount.value)
  lazy val hasMatches: Boolean = matchCount > 0

/**
 * Why the sweep button cannot run, or how much work it has. One ordered decision, so the button's
 * enabled state and its tooltip can never disagree.
 */
enum SweepState derives Eq:
  case NotAllowed(reason: String)
  case Loading
  case InFlight
  case UpToDate
  case Ready(count: Int)

  def disabled: Boolean =
    this match
      case Ready(_) => false
      case _        => true

  def tooltip: String =
    this match
      case NotAllowed(reason) => reason
      case Loading            => "Loading the stored Search results…"
      case InFlight           => "A Search is already running."
      case UpToDate           => "Every observation has an up-to-date result."
      case Ready(count)       =>
        s"Run the Archive Duplication Search for $count observation(s) that have never been " +
          "checked, failed, or have changed since they were checked"

/**
 * Everything the Archive Duplication Search tile decides about a program.
 */
case class ProgramArchiveDuplications(
  observations:   ObservationList,
  targets:        TargetList,
  duplications:   Map[Observation.Id, Pot[ArchiveDuplication]],
  headersLoaded:  Boolean,
  readonly:       Boolean,
  proposalStatus: ProposalStatus
):
  // Calibrations are the observatory's, not the proposal's, so they are not part of a
  // duplication check.
  private lazy val candidates: List[Observation] =
    observations.values.toList.filterNot(_.isCalibration)

  private def basePositionOf(obs: Observation): Option[Coordinates] =
    obs.explicitBase.orElse:
      NonEmptyList
        .fromList:
          obs.scienceTargetIds.toList
            .flatMap(targets.get)
            .flatMap(_.target.asSidereal.map(_.tracking.baseCoordinates))
        .map(Coordinates.centerOf)

  private def headerOf(obsId: Observation.Id): Pot[ArchiveDuplication] =
    duplications.getOrElse(
      obsId,
      if headersLoaded then ProgramArchiveDuplications.MissingHeader else Pot.pending
    )

  private lazy val allEntries: List[ArchiveDuplicationEntry] =
    candidates.map: obs =>
      val header = headerOf(obs.id)
      ArchiveDuplicationEntry(
        obs,
        basePositionOf(obs),
        header,
        MatchCountCell.fromPot(header, headersLoaded)
      )

  private lazy val (notApplicableEntries, applicableEntries) =
    allEntries.partition(_.duplication.toOption.exists(_.isNotApplicable))

  /** The rows the table shows: an observation the archive cannot be asked about carries none. */
  lazy val entries: List[ArchiveDuplicationEntry] = applicableEntries

  /** Observations the archive cannot be asked about, and so are kept out of the table. */
  lazy val notApplicable: List[Observation] = notApplicableEntries.map(_.observation)

  /** How many observations have Archive Matches. Shown in the Tile title. */
  lazy val withMatchesCount: Int =
    entries.count(_.hasMatches)

  /** The observations the sweep asks about: never checked, last attempt failed, or stale. */
  lazy val sweepObservations: List[Observation.Id] =
    allEntries.filter(_.duplication.toOption.exists(_.needsSearch)).map(_.id)

  lazy val searchInFlight: Boolean =
    headersLoaded && duplications.values.exists(_.isPending)

  /** The sweep button's whole state: whether it can run, and what it would do. */
  lazy val sweepState: SweepState =
    disabledReason.fold(
      if !headersLoaded then SweepState.Loading
      else if searchInFlight then SweepState.InFlight
      else
        NonEmptyList
          .fromList(sweepObservations)
          .fold(SweepState.UpToDate)(obs => SweepState.Ready(obs.length))
    )(SweepState.NotAllowed(_))

  /** Why the Search controls cannot run: the ODB rejects the refresh mutation in these cases. */
  lazy val disabledReason: Option[String] =
    if readonly then "You do not have permission to edit this program.".some
    else if proposalStatus =!= ProposalStatus.NotSubmitted then
      "The proposal has been submitted; its Archive Duplication Search is frozen.".some
    else none

object ProgramArchiveDuplications:
  private val MissingHeader: Pot[ArchiveDuplication] =
    Pot.error(
      new RuntimeException("The Archive Duplication query reported nothing for this observation")
    )
