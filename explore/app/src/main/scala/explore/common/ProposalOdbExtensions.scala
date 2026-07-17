// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.Unassign
import clue.data.syntax.*
import explore.model.PartnerSplit
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalType.*
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.util.TimeSpan
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

  extension (proposalType: GeminiProposalType)
    def toInput: GeminiProposalTypeInput =
      proposalType match
        case GeminiProposalType.DemoScience(_, toOActivation, minPercentTime)        =>
          GeminiProposalTypeInput.DemoScience(
            DemoScienceInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.DirectorsTime(_, toOActivation, minPercentTime)      =>
          GeminiProposalTypeInput.DirectorsTime(
            DirectorsTimeInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.FastTurnaround(_,
                                               toOActivation,
                                               minPercentTime,
                                               reviewer,
                                               mentor
            ) =>
          GeminiProposalTypeInput.FastTurnaround(
            FastTurnaroundInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              reviewerId = reviewer.orUnassign,
              mentorId = mentor.orUnassign
            )
          )
        case GeminiProposalType.LargeProgram(
              _,
              toOActivation,
              minPercentTime,
              minPercentTotalTime,
              totalTime,
              aeonMultiFacility,
              jwstSynergy
            ) =>
          GeminiProposalTypeInput.LargeProgram(
            LargeProgramInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              minPercentTotalTime = minPercentTotalTime.assign,
              totalTime = totalTime.toInput.assign,
              aeonMultiFacility = aeonMultiFacility.assign,
              jwstSynergy = jwstSynergy.assign
            )
          )
        case GeminiProposalType.Classical(
              _,
              minPercentTime,
              partnerSplits,
              aeonMultiFacility,
              jwstSynergy,
              usLongTerm
            ) =>
          GeminiProposalTypeInput.Classical(
            ClassicalInput(
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign,
              aeonMultiFacility = aeonMultiFacility.assign,
              jwstSynergy = jwstSynergy.assign,
              usLongTerm = usLongTerm.assign
            )
          )
        case GeminiProposalType.Queue(
              _,
              toOActivation,
              minPercentTime,
              partnerSplits,
              aeonMultiFacility,
              jwstSynergy,
              usLongTerm,
              considerForBand3
            ) =>
          GeminiProposalTypeInput.Queue(
            QueueInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign,
              aeonMultiFacility = aeonMultiFacility.assign,
              jwstSynergy = jwstSynergy.assign,
              usLongTerm = usLongTerm.assign,
              considerForBand3 = considerForBand3.assign
            )
          )
        case GeminiProposalType.SystemVerification(_, toOActivation, minPercentTime) =>
          GeminiProposalTypeInput.SystemVerification(
            SystemVerificationInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case GeminiProposalType.PoorWeather(scienceSubtype)                          =>
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
