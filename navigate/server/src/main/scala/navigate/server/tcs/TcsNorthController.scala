// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import navigate.server.ApplyCommandResult

trait TcsNorthController[F[_]] extends TcsBaseController[F] {

  /**
   * Make the Altair AOWFS probe follow the guide star, or stop doing it. It is not allowed if the
   * probe has no target of its own, or if Altair has not been configured.
   */
  def altairFollow(enable: Boolean): F[ApplyCommandResult]

  /** Park Altair */
  def altairPark: F[ApplyCommandResult]

}
