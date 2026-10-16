// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import crystal.react.View
import crystal.react.syntax.all.*
import observe.model.Observation
import observe.ui.model.ObservationRequests

extension (requests: View[Map[Observation.Id, ObservationRequests]])
  def modObservation(
    obsId: Observation.Id
  )(f: ObservationRequests => ObservationRequests): IO[Unit] =
    requests
      .mod: r =>
        r + (obsId -> f(r.getOrElse(obsId, ObservationRequests.Idle)))
      .to[IO]
