// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import navigate.model.NavigateCommand
import navigate.model.NavigateCommand.*

/**
 * Rules about which commands cannot run at the same time. To add a restriction, add a [[Rule]] to
 * `rules`.
 */
object CommandConflicts {

  /**
   * Decides if the `incoming` command conflicts with a `running` one. If it does, it returns the
   * reason. Rules are not required to be symmetric.
   */
  type Rule = (NavigateCommand, NavigateCommand) => Option[String]

  /**
   * Commands that map to the same key block each other, including themselves. Commands not covered
   * by `key` are not affected by the rule.
   */
  def exclusive[K: Eq](reason: K => String)(key: PartialFunction[NavigateCommand, K]): Rule =
    (running, incoming) =>
      (key.lift(running), key.lift(incoming)).tupled.collect {
        case (a, b) if a === b => reason(a)
      }

  /** Same as `exclusive`, for a single group of commands. */
  def exclusiveGroup(reason: String)(in: PartialFunction[NavigateCommand, Unit]): Rule =
    exclusive[Unit](_ => reason)(in)

  val pwfs1: Rule =
    exclusiveGroup("PWFS1 observation commands cannot run at the same time") {
      case _: Pwfs1Observe | _: Pwfs1CircularBuffer | _: Pwfs1QlMode | Pwfs1StopObserve => ()
      case WfsSky(GuideProbe.PWFS1, _)                                                  => ()
    }

  val pwfs2: Rule =
    exclusiveGroup("PWFS2 observation commands cannot run at the same time") {
      case _: Pwfs2Observe | _: Pwfs2CircularBuffer | _: Pwfs2QlMode | Pwfs2StopObserve => ()
      case WfsSky(GuideProbe.PWFS2, _)                                                  => ()
    }

  val oiwfs: Rule =
    exclusiveGroup("OIWFS observation commands cannot run at the same time") {
      case _: OiwfsObserve | _: OiwfsCircularBuffer | _: OiwfsQlMode | OiwfsStopObserve => ()
      case WfsSky(GuideProbe.GmosOIWFS | GuideProbe.Flamingos2OIWFS, _)                 => ()
    }

  val ac: Rule =
    exclusiveGroup("AC observation commands cannot run at the same time") {
      case _: AcObserve | AcStopObserve => ()
    }

  val rules: List[Rule] = List(pwfs1, pwfs2, oiwfs, ac)

  /** The reason why `incoming` cannot run while `running` is in progress, if there is one. */
  def between(running: NavigateCommand, incoming: NavigateCommand): Option[String] =
    rules.collectFirstSome(_(running, incoming))

}
