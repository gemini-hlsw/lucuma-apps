// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.enums

import lucuma.core.util.Enumerated

/** How the queue dispatcher treats an observation that cannot start because of busy subsystems. */
enum QueueOrderingMode(val tag: String) derives Enumerated:
  /** Nothing below a blocked observation starts. */
  case Strict extends QueueOrderingMode("strict")

  /** A blocked observation is skipped, and the ones below it may start. */
  case JumpAhead extends QueueOrderingMode("jump_ahead")

object QueueOrderingMode:
  val Default: QueueOrderingMode = JumpAhead
