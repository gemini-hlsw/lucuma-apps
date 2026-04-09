// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import observe.model.Subsystem

trait System[F[_]] {
  val resource: Subsystem

  /**
   * Called to configure a system
   */
  def configure: F[ConfigResult[F]]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

//Placeholder for config response
final case class ConfigResult[F[_]](sys: System[F])
