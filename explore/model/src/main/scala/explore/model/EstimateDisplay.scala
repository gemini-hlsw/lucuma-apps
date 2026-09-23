// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.TimeSpan

/**
 * Which estimates an observation badge shows. The Original Estimate only exists once execution has
 * started, and once completed the Remaining Estimate carries no information.
 */
enum EstimateDisplay derives Eq:
  case RemainingOnly
  case RemainingAndOriginal(original: TimeSpan)
  case OriginalOnly(original: TimeSpan)

object EstimateDisplay:
  def apply(original: Option[TimeSpan], state: ObservationWorkflowState): EstimateDisplay =
    original match
      case None                                                    => RemainingOnly
      case Some(o) if state === ObservationWorkflowState.Completed => OriginalOnly(o)
      case Some(o)                                                 => RemainingAndOriginal(o)
