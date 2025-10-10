// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import navigate.server.acm.Encoder

sealed trait OffsetIndexSelection {
  def toEPICSValue: String
}

object OffsetIndexSelection {
  case object All            extends OffsetIndexSelection {
    override def toEPICSValue: String = "all"
  }
  case class Index(idx: Int) extends OffsetIndexSelection {
    override def toEPICSValue: String = idx.toString
  }

  given Encoder[OffsetIndexSelection, String] = _.toEPICSValue
}
