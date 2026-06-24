// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.CallProperties.GeminiCallProperties
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.Partner
import lucuma.core.model.CallForProposals
import lucuma.core.model.PartnerLink
import lucuma.core.model.Semester
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp
import lucuma.odb.json.time.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens

import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class CallPartner(
  partner:            Partner,
  submissionDeadline: Option[Timestamp]
) derives Eq,
      Decoder

case class CallForProposal(
  id:             CallForProposals.Id,
  semester:       Semester,
  title:          NonEmptyString,
  partners:       List[CallPartner],
  active:         DateInterval,
  callProperties: CallProperties
) derives Eq:

  // Gemini-specific properties, present only for Gemini calls.
  def gemini: Option[GeminiCallProperties] =
    CallProperties.gemini.getOption(callProperties)

  def middleDate: LocalDate =
    active.start.plusDays(ChronoUnit.DAYS.between(active.start, active.end) / 2)

  def deadline(piPartner: Option[PartnerLink]): Either[String, Timestamp] =
    // piPartner is only None if there is no pi, which should never happen
    piPartner.fold("No PI for this program.".asLeft):
      _.fold(
        "Select PI's partner to show CfP deadline.".asLeft,
        gemini
          .flatMap(_.nonPartnerDeadline)
          .fold("Non-partner PI not allowed for this CfP.".asLeft)(_.asRight),
        partner =>
          partners
            .find(_.partner === partner)
            .fold("PI partner not valid for this CfP.".asLeft)(
              _.submissionDeadline
                .fold("PI partner deadline not set for this CfP.".asLeft)(_.asRight)
            ),
        _ => "Exchange partner PIs are not yet supported.".asLeft
      )

object CallForProposal:
  val id: Lens[CallForProposal, CallForProposals.Id] =
    Focus[CallForProposal](_.id)

  // The observatory-specific properties are read from the mutually-exclusive
  // `gemini` / `keck` / `subaru` blocks of the same `CallForProposals` object.
  given Decoder[CallForProposal] = c =>
    for {
      id             <- c.downField("id").as[CallForProposals.Id]
      semester       <- c.downField("semester").as[Semester]
      title          <- c.downField("title").as[NonEmptyString]
      partners       <- c.downField("partners").as[List[CallPartner]]
      active         <- c.downField("active").as[DateInterval]
      callProperties <- c.as[CallProperties]
    } yield CallForProposal(id, semester, title, partners, active, callProperties)
