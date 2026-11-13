// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.acm

import lucuma.core.util.Enumerated

/**
 * State of a CAR, and of the BUSY field of the TaskControl and assemblyControl records. Those were
 * first developed when GEM5 was in use, where the error state is called ERR, and the name was not
 * updated in their BUSY field.
 *
 * The difference does not matter when decoding, because EPICS enumerations are sent as the index of
 * the selected value, not as its name, and the order of the states is the same in both cases (see
 * the `ToJavaType` instance for `Enumerated` in `navigate.epics`). The order of the cases here must
 * match it.
 *
 * The BUSY field of the TaskControl and assemblyControl records is an output that only takes the
 * values IDLE, BUSY and ERR, so it is never PAUSED. The documents of those records list the values
 * in a different order, but the enumeration read from the EPICS channel is IDLE, PAUSED, BUSY, ERR,
 * which is the order used here.
 */
enum CarState(val tag: String) extends Product with Serializable derives Enumerated {
  case IDLE   extends CarState("idle")
  case PAUSED extends CarState("paused")
  case BUSY   extends CarState("busy")
  case ERROR  extends CarState("error")
}
