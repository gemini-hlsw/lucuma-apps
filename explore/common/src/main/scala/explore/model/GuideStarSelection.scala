// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.GuideStarId
import lucuma.core.math.Angle

sealed trait GuideStarSelection derives Eq

object GuideStarSelection:
  // Automatic selection, store the index
  case class AgsSelection(index: Option[(Int, AgsAnalysis.Usable)]) extends GuideStarSelection
      derives Eq

  // Remote selection, as stored in the ODB
  case class RemoteGSSelection(selection: GuideStarId) extends GuideStarSelection derives Eq

  // Fully developed override, include analysis and location on list of candidates
  case class AgsOverride(
    selection:       GuideStarId,
    selectedGSIndex: Int,
    analysis:        AgsAnalysis.Usable
  ) extends GuideStarSelection derives Eq

  val Default: GuideStarSelection = AgsSelection(None)

  extension (gs: GuideStarSelection)
    def fold[A](f: AgsSelection => A, g: RemoteGSSelection => A, h: AgsOverride => A): A =
      gs match
        case a @ AgsSelection(_)      => f(a)
        case a @ RemoteGSSelection(_) => g(a)
        case a @ AgsOverride(_, _, _) => h(a)

    // Drops a stale analysis, keeping a manual pick as a selection for `pick` to re-resolve. Going
    // straight to AgsSelection would lose the pick.
    def resetKeepingSelection: GuideStarSelection = gs match
      case AgsOverride(selection, _, _) => RemoteGSSelection(selection)
      case r @ RemoteGSSelection(_)     => r
      case AgsSelection(_)              => Default

    def idx: Option[Int] = fold(_.index.map(_._1), _ => none, _.selectedGSIndex.some)

    def selectedId: Option[GuideStarId] =
      fold(_.index.map(_._2.target.id), _.selection.some, _.selection.some)

    def analysis: Option[AgsAnalysis.Usable] = gs match
      case AgsSelection(Some((_, a))) => a.some
      case AgsOverride(_, _, a)       => a.some
      case _                          => none

    def selectedAngle: Option[Angle] = analysis.map(_.posAngle)

// The state of a guide star the user picked by hand, as reported in the AGS overlay.
enum ManualGuideStar derives Eq:
  case Selected, Unusable, Ambiguous, NotFound, LookupFailed

extension (r: List[AgsAnalysis.Usable])
  // The star the ODB selects when it selects one, else the best candidate.
  def select(
    selection:          Option[GuideStarId],
    observationTargets: Int
  ): GuideStarSelection =
    selection.fold(GuideStarSelection.AgsSelection(r.headOption.tupleLeft(0))):
      pick(_, observationTargets)

  def pick(i: Int): GuideStarSelection =
    r.lift(i)
      .fold[GuideStarSelection](GuideStarSelection.AgsSelection(none))(a =>
        GuideStarSelection.AgsOverride(a.target.id, i, a)
      )

  // The first result, in ranking order, the selection picks; kept as remote when there is none or
  // the selection is ambiguous for the number of targets the observation has.
  def pick(selection: GuideStarId, observationTargets: Int): GuideStarSelection =
    r.zipWithIndex
      .collectFirst { case (a, i) if selection.selects(a.target.id) => i }
      .filterNot(_ => selection.isAmbiguousWith(observationTargets))
      .fold[GuideStarSelection](GuideStarSelection.RemoteGSSelection(selection))(i =>
        GuideStarSelection.AgsOverride(selection, i, r(i))
      )

extension (selection: GuideStarId)
  // As in the ODB, the id-less selection picks nothing when the observation has several targets,
  // whether or not each of them has a position.
  def isAmbiguousWith(observationTargets: Int): Boolean =
    selection.isObservationTarget && selection.observationTargetId.isEmpty && observationTargets > 1
