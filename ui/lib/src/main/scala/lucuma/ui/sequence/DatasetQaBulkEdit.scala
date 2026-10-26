// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import japgolly.scalajs.react.Callback
import lucuma.schemas.model.Dataset
import lucuma.schemas.model.enums.StepExecutionState

import scala.collection.immutable.HashSet

/**
 * `OnlyUnset` leaves datasets that already have a QA state alone, which the server enforces too in
 * case the client's data is stale. `Overwrite` replaces whatever QA state they have.
 */
enum QaBulkMode:
  case OnlyUnset, Overwrite

case class DatasetQaBulkEdit(
  datasetIdsInFlight: HashSet[Dataset.Id],
  onChange:           QaBulkMode => NonEmptyList[Dataset.Id] => EditableQaFields => Callback
)

object DatasetQaBulkEdit:
  def datasetCount(count: Int): String =
    s"$count dataset" + (if count == 1 then "" else "s")

  private val TargetStates: Set[StepExecutionState] =
    Set(StepExecutionState.Completed, StepExecutionState.Stopped)

  /**
   * The datasets a bulk edit can target: those of completed or stopped steps. The editor then
   * narrows them down to the ones with no QA state, unless the user chooses to overwrite.
   */
  def targets(steps: List[(StepExecutionState, List[Dataset])]): List[Dataset] =
    steps.flatMap: (executionState, datasets) =>
      if TargetStates.contains(executionState) then datasets
      else Nil
