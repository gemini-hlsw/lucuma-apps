// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import observe.model.Subsystem
import observe.model.dhs.ImageFileId
import observe.server.engine.Result.RetVal

enum Response extends RetVal:
  case Configured(subsystem: Subsystem) extends Response
  case Observed(fileId: ImageFileId)    extends Response
  case Aborted(fileId: ImageFileId)     extends Response
  case Ignored                          extends Response
