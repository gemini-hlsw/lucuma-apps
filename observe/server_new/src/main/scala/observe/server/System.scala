// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.Instrument
import observe.model.enums.Resource

trait System[F[_]] {
  val resource: Resource | Instrument

  /**
   * Called to configure a system
   */
  def configure: F[ConfigResult[F]]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

//Placeholder for config response
final case class ConfigResult[F[_]](sys: System[F])
