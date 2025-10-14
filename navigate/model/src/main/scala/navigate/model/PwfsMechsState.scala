// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter

case class PwfsMechsState(
  filter:    Option[PwfsFilter],
  fieldStop: Option[PwfsFieldStop]
) derives Eq
