// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Eq
import cats.derived.*
import fs2.concurrent.SignallingRef

enum LoadStage(val label: String) derives Eq:
  case Observations          extends LoadStage("Observations")
  case Groups                extends LoadStage("Groups")
  case ProgramDetails        extends LoadStage("Program details")
  case Targets               extends LoadStage("Targets")
  case Attachments           extends LoadStage("Attachments")
  case Programs              extends LoadStage("Program list")
  case ConfigurationRequests extends LoadStage("Configuration requests")
  case Preparing             extends LoadStage("Preparing program")

// A stage stays as Done just long enough for its row to fade out.
enum LoadStageState derives Eq:
  case InFlight, Done

type LoadProgress          = Map[LoadStage, LoadStageState]
type LoadProgressRef[F[_]] = SignallingRef[F, LoadProgress]
