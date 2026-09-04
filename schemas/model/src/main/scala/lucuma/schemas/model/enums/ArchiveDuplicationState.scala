// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.enums

import lucuma.core.util.Enumerated

// The outcome of the most recent Archive Duplication Search attempt for an observation.
enum ArchiveDuplicationState(val tag: String, val label: String) derives Enumerated:
  case NotChecked    extends ArchiveDuplicationState("NOT_CHECKED", "Not checked")
  case NotApplicable extends ArchiveDuplicationState("NOT_APPLICABLE", "Not applicable")
  case Checked       extends ArchiveDuplicationState("CHECKED", "Checked")
  case Error         extends ArchiveDuplicationState("ERROR", "Search failed")
