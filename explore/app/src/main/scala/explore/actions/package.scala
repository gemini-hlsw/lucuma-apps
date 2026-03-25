// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.actions

import cats.syntax.all.*
import explore.model.Observation
import explore.model.ProgramSummaries

import scala.collection.mutable

private def obsListGetter(
  obsList: List[Observation.Id]
): ProgramSummaries => Option[List[Observation]] =
  programSummaries => obsList.map(programSummaries.observations.get(_)).sequence

private def obsListSetter(obsList: List[Observation.Id])(
  otwol: Option[List[Observation]]
): ProgramSummaries => ProgramSummaries =
  programSummaries =>
    otwol.fold {
      obsList.foldLeft(programSummaries) { case (ps, obsId) => ps.removeObs(obsId) }
    } {
      _.foldLeft(programSummaries)((ps, obsSumm) => ps.upsertObs(obsSumm))
    }

object ObservationCloneNotifier:
  private val arrived = mutable.Map.empty[Observation.Id, Observation]

  def tryGetAll(ids: List[Observation.Id]): Option[List[Observation]] =
    val found = ids.flatMap(id => arrived.get(id).map(id -> _))
    if found.length == ids.length then
      ids.foreach(arrived.remove)
      found.map(_._2).some
    else
      none

  def notify(id: Observation.Id, obs: Observation): Unit =
    arrived(id) = obs
