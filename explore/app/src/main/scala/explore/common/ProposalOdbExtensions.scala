// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.Input
import clue.data.Unassign
import clue.data.syntax.*
import explore.model.PartnerSplit
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalType.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.AeonMultiFacilityInput
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.GeminiProposalTypeInput
import lucuma.schemas.ObservationDB.Types.KeckProposalTypeInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.PartnerSplitInput
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import lucuma.schemas.ObservationDB.Types.QueueInput
import lucuma.schemas.ObservationDB.Types.SubaruProposalTypeInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput
import lucuma.schemas.ObservationDB.Types.TimeSpanInput

trait ProposalOdbExtensions:
  // This is on import lucuma.schemas.odb.input.* but it is not picked up for some reason
  extension (ts: TimeSpan)
    def toInput: TimeSpanInput = TimeSpanInput.Microseconds(ts.toMicroseconds)

  // The ODB rejects a time request that carries both partner splits and an
  // exchange partner, so the splits are explicitly nulled out (which clears them)
  // whenever the request belongs to an exchange partner community. Null is also
  // how empty splits are expressed: a list, when given, must sum to 100.
  private def splitsInput(
    partnerSplits:   List[PartnerSplit],
    exchangePartner: Option[ExchangePartner]
  ): Input[List[PartnerSplitInput]] =
    if (exchangePartner.isDefined || partnerSplits.isEmpty) Unassign
    else partnerSplits.map(_.toInput).assign

  // The ODB models AEON membership as the presence of the AeonMultiFacility
  // object. Explore only tracks membership, so the required instruments are
  // left untouched.
  private def aeonMultiFacilityInput(aeon: Boolean): Input[AeonMultiFacilityInput] =
    if (aeon) AeonMultiFacilityInput().assign else Unassign

  extension (proposalType: GeminiProposalType)
    def toInput: GeminiProposalTypeInput =
      proposalType match
        case GeminiProposalType.DemoScience(_, tooActivationCeiling, minPercentTime)        =>
          GeminiProposalTypeInput.DemoScience(
            DemoScienceInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.DirectorsTime(_, tooActivationCeiling, minPercentTime)      =>
          GeminiProposalTypeInput.DirectorsTime(
            DirectorsTimeInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.FastTurnaround(_,
                                               tooActivationCeiling,
                                               minPercentTime,
                                               reviewer,
                                               mentor
            ) =>
          GeminiProposalTypeInput.FastTurnaround(
            FastTurnaroundInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign,
              reviewerId = reviewer.orUnassign,
              mentorId = mentor.orUnassign
            )
          )
        case GeminiProposalType.LargeProgram(
              _,
              tooActivationCeiling,
              minPercentTime,
              minPercentTotalTime,
              totalTime,
              aeonMultiFacility,
              jwstSynergy
            ) =>
          GeminiProposalTypeInput.LargeProgram(
            LargeProgramInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign,
              minPercentTotalTime = minPercentTotalTime.assign,
              totalTime = totalTime.toInput.assign,
              aeonMultiFacility = aeonMultiFacilityInput(aeonMultiFacility),
              jwstSynergy = jwstSynergy.assign
            )
          )
        case GeminiProposalType.Classical(
              _,
              minPercentTime,
              partnerSplits,
              exchangePartner,
              aeonMultiFacility,
              jwstSynergy,
              usLongTerm
            ) =>
          GeminiProposalTypeInput.Classical(
            ClassicalInput(
              minPercentTime = minPercentTime.assign,
              partnerSplits = splitsInput(partnerSplits, exchangePartner),
              exchangePartner = exchangePartner.orUnassign,
              aeonMultiFacility = aeonMultiFacilityInput(aeonMultiFacility),
              jwstSynergy = jwstSynergy.assign,
              usLongTerm = usLongTerm.assign
            )
          )
        case GeminiProposalType.Queue(
              _,
              tooActivationCeiling,
              minPercentTime,
              partnerSplits,
              exchangePartner,
              aeonMultiFacility,
              jwstSynergy,
              usLongTerm,
              considerForBand3
            ) =>
          GeminiProposalTypeInput.Queue(
            QueueInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign,
              partnerSplits = splitsInput(partnerSplits, exchangePartner),
              exchangePartner = exchangePartner.orUnassign,
              aeonMultiFacility = aeonMultiFacilityInput(aeonMultiFacility),
              jwstSynergy = jwstSynergy.assign,
              usLongTerm = usLongTerm.assign,
              considerForBand3 = considerForBand3.assign
            )
          )
        case GeminiProposalType.SystemVerification(_, tooActivationCeiling, minPercentTime) =>
          GeminiProposalTypeInput.SystemVerification(
            SystemVerificationInput(
              explicitTooActivationCeiling = tooActivationCeiling.explicit.orUnassign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.PoorWeather(scienceSubtype)                                 =>
          GeminiProposalTypeInput.PoorWeather(PoorWeatherInput())

  extension (proposalType: KeckProposalType)
    def toInput: KeckProposalTypeInput =
      KeckProposalTypeInput(
        minPercentTime = proposalType.minPercentTime.assign,
        partnerSplits =
          if (proposalType.partnerSplits.nonEmpty) proposalType.partnerSplits.map(_.toInput).assign
          else Unassign
      )

  extension (proposalType: SubaruProposalType)
    def toInput: SubaruProposalTypeInput =
      SubaruProposalTypeInput(
        minPercentTime = proposalType.minPercentTime.assign,
        partnerSplits =
          if (proposalType.partnerSplits.nonEmpty) proposalType.partnerSplits.map(_.toInput).assign
          else Unassign
      )

  // Used to reset the proposal type when the call changes
  extension (cfpType: GeminiCallForProposalsType)
    def defaultType(reviewerId: Option[ProgramUser.Id]): GeminiProposalType = cfpType match
      case GeminiCallForProposalsType.DemoScience        => GeminiProposalType.DemoScience.Default
      case GeminiCallForProposalsType.DirectorsTime      => GeminiProposalType.DirectorsTime.Default
      case GeminiCallForProposalsType.FastTurnaround     =>
        GeminiProposalType.FastTurnaround.defaultWithReviewer(reviewerId)
      case GeminiCallForProposalsType.LargeProgram       => GeminiProposalType.LargeProgram.Default
      case GeminiCallForProposalsType.PoorWeather        => GeminiProposalType.PoorWeather.Default
      case GeminiCallForProposalsType.RegularSemester    => GeminiProposalType.Queue.Default
      case GeminiCallForProposalsType.SystemVerification =>
        GeminiProposalType.SystemVerification.Default

  extension (split: PartnerSplit)
    def toInput: PartnerSplitInput =
      PartnerSplitInput(partner = split.partner, percent = split.percent)

  extension (proposal: Proposal)
    def toInput: ProposalPropertiesInput =
      val base = ProposalPropertiesInput(
        callId = proposal.call.map(_.id).orUnassign,
        category = proposal.category.orUnassign
      )
      proposal.proposalType match
        case Some(g: GeminiProposalType) => base.copy(gemini = g.toInput.assign)
        case Some(k: KeckProposalType)   => base.copy(keck = k.toInput.assign)
        case Some(s: SubaruProposalType) => base.copy(subaru = s.toInput.assign)
        case None                        => base

object ProposalOdbExtensions extends ProposalOdbExtensions
