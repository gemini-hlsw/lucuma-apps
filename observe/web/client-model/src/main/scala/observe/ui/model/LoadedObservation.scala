// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import crystal.Pot
import crystal.Pot.Pending
import crystal.Pot.Ready
import crystal.syntax.*
import lucuma.core.model.Visit
import lucuma.core.util.NewType
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.sequence.SequenceData
import monocle.Focus
import monocle.Lens
import observe.model.Observation

object LoadedObservations extends NewType[Map[Observation.Id, Pot[LoadedObservation]]]:
  val empty: LoadedObservations = LoadedObservations(Map.empty)

  extension (loadedObss: LoadedObservations)
    def modify(
      f: Map[Observation.Id, Pot[LoadedObservation]] => Map[Observation.Id, Pot[LoadedObservation]]
    ): LoadedObservations =
      LoadedObservations(f(loadedObss.value))

    def readyObsIds: Set[Observation.Id] =
      loadedObss.value
        .collect:
          case (obsId, obsPot) if obsPot.isReady => obsId
        .toSet
type LoadedObservations = LoadedObservations.Type

case class LoadedObservation private (
  isRefreshing: Boolean = false, // Waiting for the server. Used when resyncing after disconnect.
  sequenceData: Pot[SequenceData] = Pot.pending,
  visits:       Pot[Option[ExecutionVisits]] = Pot.pending
) derives Eq:
  private def potFromEither[A](e: Either[Throwable, A]): Pot[A] =
    e.toTry.toPot

  private def potFromEitherOption[A](e: Either[Throwable, Option[A]]): Pot[A] =
    e.toTry.toPot.flatMap(_.toPot)

  def withSequenceData(config: Either[Throwable, SequenceData]): LoadedObservation =
    copy(sequenceData = potFromEither(config))

  def withVisits(visits: Either[Throwable, Option[ExecutionVisits]]): LoadedObservation =
    copy(visits = potFromEitherOption(visits.map(_.some)))

  def addVisits(addedVisits: Either[Throwable, Option[ExecutionVisits]]): LoadedObservation =
    copy(visits = visits match
      case Ready(existing) =>
        potFromEitherOption(addedVisits.map(_.some)) match
          case Ready(Some(added)) => Ready(existing.fold(added)(_.extendWith(added)).some)
          case Ready(None)        => Ready(existing)
          case Pending            => visits
          case error              => error
      case _               => potFromEitherOption(addedVisits.map(_.some)))

  def reset: LoadedObservation =
    copy(sequenceData = Pot.pending, visits = Pot.pending)

  lazy val lastVisitId: Option[Visit.Id] =
    visits.toOption.flatten.map:
      case ExecutionVisits.GmosNorth(visits)  => visits.last.id
      case ExecutionVisits.GmosSouth(visits)  => visits.last.id
      case ExecutionVisits.Flamingos2(visits) => visits.last.id

object LoadedObservation:
  def apply(): LoadedObservation = new LoadedObservation()

  val isRefreshing: Lens[LoadedObservation, Boolean]                = Focus[LoadedObservation](_.isRefreshing)
  val sequenceData: Lens[LoadedObservation, Pot[SequenceData]]      =
    Focus[LoadedObservation](_.sequenceData)
  val visits: Lens[LoadedObservation, Pot[Option[ExecutionVisits]]] =
    Focus[LoadedObservation](_.visits)
