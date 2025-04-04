// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.MonadThrow
import cats.effect.IO
import japgolly.scalajs.react.React
import japgolly.scalajs.react.feature.Context
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import observe.model.Observer
import observe.model.Operator
import observe.model.SubsystemEnabled

trait ConfigApi[F[_]: MonadThrow]:
  def setImageQuality(iq:    ImageQuality.Preset): F[Unit]    = NotAuthorized
  def setCloudExtinction(ce: CloudExtinction.Preset): F[Unit] = NotAuthorized
  def setWaterVapor(wv:      WaterVapor): F[Unit]             = NotAuthorized
  def setSkyBackground(sb:   SkyBackground): F[Unit]          = NotAuthorized

  def setOperator(operator: Option[Operator]): F[Unit] = NotAuthorized
  def setObserver(obsId:    Observation.Id, observer: Option[Observer]): F[Unit] = NotAuthorized

  def setTcsEnabled(obsId:  Observation.Id, enabled: SubsystemEnabled): F[Unit] = NotAuthorized
  def setGcalEnabled(obsId: Observation.Id, enabled: SubsystemEnabled): F[Unit] = NotAuthorized
  def setInstrumentEnabled(obsId: Observation.Id, enabled: SubsystemEnabled): F[Unit] =
    NotAuthorized
  def setDhsEnabled(obsId: Observation.Id, enabled: SubsystemEnabled): F[Unit] = NotAuthorized

  def isBlocked: Boolean = false

object ConfigApi:
  // Default value is NotAuthorized implementations
  val ctx: Context[ConfigApi[IO]] = React.createContext(new ConfigApi[IO] {})
