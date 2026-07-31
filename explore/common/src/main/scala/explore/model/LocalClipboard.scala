// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.model.Program

enum LocalClipboard(val isEmpty: Boolean, val isObservations: Boolean, val isTargets: Boolean)
    derives Encoder,
      Decoder:
  case Empty extends LocalClipboard(true, false, false)
  case CopiedObservations(programId: Program.Id, oids: ObsIdSet)
      extends LocalClipboard(false, true, false)
  case CopiedTargets(programId: Program.Id, tids: TargetIdSet)
      extends LocalClipboard(false, false, true)
