// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.core.model.PartnerLeads
import lucuma.core.model.ProgramUser
import monocle.Focus
import monocle.Lens

/**
 * The share of requested time for a partner, and the partner lead: the investigator the partner
 * contacts during ITAC. The lead is maintained by `lucuma.core.model.PartnerLeads`.
 */
case class PartnerSplit(
  partner: Partner,
  percent: IntPercent,
  lead:    Option[ProgramUser.Id] = None
) derives Eq

object PartnerSplit:
  val partner: Lens[PartnerSplit, Partner]             = Focus[PartnerSplit](_.partner)
  val percent: Lens[PartnerSplit, IntPercent]          = Focus[PartnerSplit](_.percent)
  val lead: Lens[PartnerSplit, Option[ProgramUser.Id]] = Focus[PartnerSplit](_.lead)

  private val leadId: Decoder[ProgramUser.Id] =
    Decoder.instance(_.downField("id").as[ProgramUser.Id])

  /** The partners from which time is requested. */
  def requested(splits: List[PartnerSplit]): Set[Partner] =
    splits.filter(_.percent.value > 0).map(_.partner).toSet

  /** The leads currently assigned. */
  def leads(splits: List[PartnerSplit]): Map[Partner, ProgramUser.Id] =
    splits.flatMap(s => s.lead.map(s.partner -> _)).toMap

  /**
   * Assigns the leads implied by the splits and the team, keeping each current choice that remains
   * eligible. The ODB applies the same rules whenever the splits or the team change, so this is
   * what it will report back.
   */
  def withLeads(
    splits: List[PartnerSplit],
    users:  List[explore.model.ProgramUser]
  ): List[PartnerSplit] =
    val leads =
      PartnerLeads.normalize(requested(splits), users.map(_.leadCandidate), this.leads(splits))
    splits.map(s => s.copy(lead = leads.get(s.partner)))

  given Decoder[PartnerSplit] = c =>
    for
      partner <- c.downField("partner").as[Partner]
      percent <- c.downField("percent").as[IntPercent]
      lead    <- c.downField("lead").as(using Decoder.decodeOption(using leadId))
    yield PartnerSplit(partner, percent, lead)
