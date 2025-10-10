// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

sealed trait AcWindow(val tag: String)

object AcWindow {
  case object Full                                 extends AcWindow("full")
  case class Square100(centerX: Int, centerY: Int) extends AcWindow("100x100")
  case class Square200(centerX: Int, centerY: Int) extends AcWindow("200x200")
}
