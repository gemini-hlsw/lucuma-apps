// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Eq
import cats.derived.*
import fs2.concurrent.SignallingRef

enum LoadStep(val label: String) derives Eq:
  case Observations          extends LoadStep("Observations")
  case Groups                extends LoadStep("Groups")
  case ProgramDetails        extends LoadStep("Program details")
  case Targets               extends LoadStep("Targets")
  case Attachments           extends LoadStep("Attachments")
  case Programs              extends LoadStep("Program list")
  case ConfigurationRequests extends LoadStep("Configuration requests")
  case Preparing             extends LoadStep("Preparing program")

// A step stays as Done just long enough for its row to fade out.
enum LoadStepState derives Eq:
  case InFlight(page: Int)
  case Done

type LoadProgress          = Map[LoadStep, LoadStepState]
type LoadProgressRef[F[_]] = SignallingRef[F, LoadProgress]
