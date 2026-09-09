// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.enums.Partner
import lucuma.core.enums.ProposalSummaryGenerationState
import lucuma.core.util.Timestamp

/**
 * Where the program's Proposal Summary regeneration stands, aggregated by the ODB over the
 * per-partner renders. A Generating always reaches a terminal state, so the client needs no timers
 * of its own.
 */
case class ProposalSummaryGeneration(
  state:       ProposalSummaryGenerationState,
  requestedAt: Option[Timestamp],
  failures:    List[ProposalSummaryGeneration.Failure]
) derives Eq:
  def isGenerating: Boolean = state === ProposalSummaryGenerationState.Generating

  def failureFor(partner: Option[Partner]): Option[ProposalSummaryGeneration.Failure] =
    failures.find(_.partner === partner)

  // Failures with no summary to hang them on, as after a first-ever render fails.
  def failuresWithout(partners: Set[Option[Partner]]): List[ProposalSummaryGeneration.Failure] =
    failures.filterNot(f => partners.contains(f.partner))

object ProposalSummaryGeneration:
  val Idle: ProposalSummaryGeneration =
    ProposalSummaryGeneration(ProposalSummaryGenerationState.Idle, none, Nil)

  case class Failure(partner: Option[Partner], message: String) derives Eq
  object Failure:
    given Decoder[Failure] = deriveDecoder

  given Decoder[ProposalSummaryGeneration] = deriveDecoder
