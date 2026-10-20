// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import japgolly.scalajs.react.Callback
import lucuma.schemas.model.Dataset
import lucuma.schemas.model.enums.StepExecutionState

import scala.collection.immutable.HashSet

case class DatasetQaBulkEdit(
  datasetIdsInFlight: HashSet[Dataset.Id],
  onChange:           NonEmptyList[Dataset.Id] => EditableQaFields => Callback
)

object DatasetQaBulkEdit:
  def datasetCount(count: Int): String =
    s"$count dataset" + (if count == 1 then "" else "s")

  private val TargetStates: Set[StepExecutionState] =
    Set(StepExecutionState.Completed, StepExecutionState.Stopped)

  /**
   * The datasets a bulk edit targets: those of completed or stopped steps that have no QA state
   * yet. Bulk editing never overwrites a QA state; corrections are made dataset by dataset.
   */
  def targets(steps: List[(StepExecutionState, List[Dataset])]): List[Dataset] =
    steps.flatMap: (executionState, datasets) =>
      if TargetStates.contains(executionState) then datasets.filter(_.qaState.isEmpty)
      else Nil
