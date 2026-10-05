// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.syntax.all.*
import explore.model.Execution
import lucuma.core.enums.ExecutionState
import lucuma.core.util.Enumerated

/**
 * What a Duplicate takes from its source's Materialized Sequences.
 */
enum SequenceCopy(val tag: String) derives Enumerated:
  case GenerateNew  extends SequenceCopy("generate_new")
  case PendingSteps extends SequenceCopy("pending_steps")
  case AllSteps     extends SequenceCopy("all_steps")

object SequenceCopy:

  /**
   * The choices to offer when duplicating, empty when there is nothing to ask. Pending and all
   * steps are the same when nothing has started, so only all steps is offered then.
   */
  def choices(execution: Execution): List[SequenceCopy] =
    if !execution.hasMaterializedSequence then Nil
    else if execution.executionState === ExecutionState.NotStarted then List(GenerateNew, AllSteps)
    else List(GenerateNew, PendingSteps, AllSteps)
