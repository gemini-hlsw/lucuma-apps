// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.*
import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.*
import lucuma.core.model.sequence.Step
import monocle.Lens
import monocle.syntax.all.*
import observe.model.enums.*

// TODO Move this class to the server project. It is not shared anymore.
// Implies a small change to store the paused step id in SequenceView.
enum ObserveStep(
  val id:              Step.Id,
  val configStatus:    Map[Subsystem, ActionStatus]
) derives Eq,
      Encoder.AsObject,
      Decoder:
  case Standard(
    override val id:              Step.Id,
    override val configStatus:    Map[Subsystem, ActionStatus],
    val observeStatus:            ActionStatus
  ) extends ObserveStep(
        id,
        configStatus
      )

  case NodAndShuffle(
    override val id:              Step.Id,
    override val configStatus:    Map[Subsystem, ActionStatus],
    val nsStatus:                 NodAndShuffleStatus,
    val pendingObserveCmd:        Option[PendingObserveCmd]
  ) extends ObserveStep(
        id,
        configStatus
      )

object ObserveStep:
  // Derivation doesn't generate instances for subtypes.
  given Eq[Standard]      = Eq.by: x =>
    (x.id, x.configStatus)

  given Eq[NodAndShuffle] = Eq.by: x =>
    (
      x.id,
     x.configStatus,
     x.nsStatus,
     x.pendingObserveCmd
    )

  def configStatus: Lens[ObserveStep, Map[Subsystem, ActionStatus]] =
    Lens[ObserveStep, Map[Subsystem, ActionStatus]] {
      case s: Standard      => s.configStatus
      case s: NodAndShuffle => s.configStatus
    } { n =>
      {
        case s: Standard      => s.focus(_.configStatus).replace(n)
        case s: NodAndShuffle => s.focus(_.configStatus).replace(n)
      }
    }

  extension (s: ObserveStep) {
    def isObservePaused: Boolean =
      s match
        case x: Standard      => x.observeStatus === ActionStatus.Paused
        case x: NodAndShuffle => x.nsStatus.observing === ActionStatus.Paused

  }
