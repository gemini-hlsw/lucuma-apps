// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.enums

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*

enum ClientMode derives Eq:
  case ReadOnly, CanOperate

  def canOperate: Boolean = this === CanOperate
