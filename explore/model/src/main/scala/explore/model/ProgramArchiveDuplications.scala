// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

/**
 * What the Match Count cell shows — the Match Count itself is just a number, so the cell needs its
 * own type. `Pot.Pending` is a Search in flight, `Pot.Error` is a *call* that failed, and a ready
 * header whose state is ERROR is a successful call reporting a *Search* that failed: three
 * different things, displayed differently.
 */
enum MatchCountCell derives Eq:
  case InFlight
  case NotChecked
  case NotApplicable
  case Counted(count: NonNegInt, saturated: Boolean)
  case SearchFailed(count: NonNegInt, saturated: Boolean, error: Option[NonEmptyString])
  case CallFailed(message: String)

object MatchCountCell:
  def fromPot(pot: Pot[ArchiveDuplication]): MatchCountCell =
    pot match
      case Pot.Pending      => InFlight
      case Pot.Error(t)     => CallFailed(Option(t.getMessage).getOrElse("Unknown error"))
      case Pot.Ready(dupli) =>
        dupli.state match
          case ArchiveDuplicationState.NotChecked    => NotChecked
          case ArchiveDuplicationState.NotApplicable => NotApplicable
          case ArchiveDuplicationState.Checked       => Counted(dupli.matchCount, dupli.saturated)
          case ArchiveDuplicationState.Error         =>
            SearchFailed(dupli.matchCount, dupli.saturated, dupli.error)

/** One observation's row in the Archive Duplication Search tile. */
case class ArchiveDuplicationEntry(
  observation:  Observation,
  basePosition: Option[Coordinates],
  duplication:  Pot[ArchiveDuplication]
) derives Eq:
  val id: Observation.Id                  = observation.id
  lazy val matchCountCell: MatchCountCell  = MatchCountCell.fromPot(duplication)
  lazy val matchCount: Int                 = duplication.toOption.foldMap(_.matchCount.value)
  lazy val hasMatches: Boolean             = matchCount > 0
  lazy val searchArea: Option[SearchArea]  = duplication.toOption.flatMap(_.searchArea)

/**
 * Everything the Archive Duplication Search tile decides about a program, in one pure place: which
 * observations get a row, which the sweep asks about, and whether the Search controls are live.
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

  // Until the header query lands nothing is known about any observation; after it lands, an
  // observation it did not report on is a broken result rather than a Search in flight.
  private def headerOf(obsId: Observation.Id): Pot[ArchiveDuplication] =
    duplications.getOrElse(
      obsId,
      if headersLoaded then
        Pot.error(
          new RuntimeException("The Archive Duplication query reported nothing for this observation")
        )
      else Pot.pending
    )

  private lazy val allEntries: List[ArchiveDuplicationEntry] =
    candidates.map: obs =>
      ArchiveDuplicationEntry(obs, basePositionOf(obs), headerOf(obs.id))

  /** The rows the table shows: an observation the archive cannot be asked about carries none. */
  lazy val entries: List[ArchiveDuplicationEntry] =
    allEntries.filterNot(_.duplication.toOption.exists(_.isNotApplicable))

  /** Observations the archive cannot be asked about, and so are kept out of the table. */
  lazy val notApplicable: List[Observation] =
    allEntries.filter(_.duplication.toOption.exists(_.isNotApplicable)).map(_.observation)

  /** How many observations have Archive Matches. Shown in the Tile title. */
  lazy val withMatchesCount: Int =
    entries.count(_.hasMatches)

  /** The observations the sweep asks about: never checked, or last attempt failed. */
  lazy val sweepObservations: List[Observation.Id] =
    allEntries.filter(_.duplication.toOption.exists(_.needsSearch)).map(_.id)

  lazy val searchInFlight: Boolean =
    headersLoaded && duplications.values.exists(_.isPending)

  /**
   * The ODB rejects the refresh mutation once the proposal is submitted, so that is the test — not
   * the user's role alone.
   */
  lazy val controlsEnabled: Boolean =
    !readonly && proposalStatus === ProposalStatus.NotSubmitted

  lazy val disabledReason: Option[String] =
    if readonly then "You do not have permission to edit this program.".some
    else if proposalStatus =!= ProposalStatus.NotSubmitted then
      "The proposal has been submitted; its Archive Duplication Search is frozen.".some
    else none
