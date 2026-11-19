// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.enums

import lucuma.core.util.Enumerated

/** State of the queue dispatcher. */
enum QueueDispatchState(val tag: String) derives Enumerated:
  /** Never run, or finished by itself. */
  case Idle extends QueueDispatchState("idle")

  /** The dispatcher is active and starts observations as subsystems become available. */
  case Running extends QueueDispatchState("running")

  /** Stopped by a queue-level control or at a pause: nothing starts until the queue is run. */
  case Halted extends QueueDispatchState("halted")
