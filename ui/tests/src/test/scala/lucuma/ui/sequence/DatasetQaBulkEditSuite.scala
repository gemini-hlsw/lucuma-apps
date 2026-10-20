// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Dataset.Filename
import lucuma.core.util.Gid
import lucuma.schemas.model.Dataset
import lucuma.schemas.model.enums.StepExecutionState

import java.time.LocalDate

class DatasetQaBulkEditSuite extends munit.FunSuite:

  private def dataset(index: Int, qaState: Option[DatasetQaState]): Dataset =
    Dataset(
      Gid[Dataset.Id].fromLong.getOption(index.toLong).get,
      PosShort.unsafeFrom(index.toShort),
      Filename.from(Site.GN, LocalDate.of(2026, 10, 20), PosInt.unsafeFrom(index)).get,
      qaState,
      none,
      none,
      isWritten = true
    )

  private val unset1: Dataset  = dataset(1, None)
  private val unset2: Dataset  = dataset(2, None)
  private val passed3: Dataset = dataset(3, Some(DatasetQaState.Pass))

  test("targets the unset datasets of completed and stopped steps"):
    val steps = List(
      StepExecutionState.Completed -> List(unset1, passed3),
      StepExecutionState.Stopped   -> List(unset2)
    )
    assertEquals(DatasetQaBulkEdit.targets(steps), List(unset1, unset2))

  test("skips steps that are not completed or stopped"):
    val steps = List(
      StepExecutionState.Ongoing    -> List(unset1),
      StepExecutionState.Aborted    -> List(unset2),
      StepExecutionState.Abandoned  -> List(unset2),
      StepExecutionState.NotStarted -> List(unset2)
    )
    assertEquals(DatasetQaBulkEdit.targets(steps), Nil)

  test("nothing to target when every dataset has a QA state"):
    assertEquals(
      DatasetQaBulkEdit.targets(List(StepExecutionState.Completed -> List(passed3))),
      Nil
    )
