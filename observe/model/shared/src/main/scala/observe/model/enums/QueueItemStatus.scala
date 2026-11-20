// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.enums

import lucuma.core.util.Enumerated

/** Execution status of an observation in the daytime queue. */
enum QueueItemStatus(val tag: String) derives Enumerated:
  /** The dispatcher may start it. */
  case Waiting extends QueueItemStatus("waiting")

  /** Currently executing. */
  case Running extends QueueItemStatus("running")

  /** Held before finishing. The dispatcher skips it until Run Queue sets it back to Waiting. */
  case Stopped extends QueueItemStatus("stopped")

  /** Finished successfully. */
  case Completed extends QueueItemStatus("completed")

  /** Finished with an error. */
  case Failed extends QueueItemStatus("failed")

  /** Aborted by the operator. */
  case Aborted extends QueueItemStatus("aborted")
