// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.derived.*
import cats.kernel.Order

enum ActionType derives Order:
  case Observe
  case Undefined // Used in tests
  case Configure(sys: Subsystem)
  case OdbEvent
