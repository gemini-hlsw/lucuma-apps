// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.Input
import clue.data.Unassign
import clue.data.syntax.*
import explore.model.PartnerSplit
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalType
import lucuma.core.enums.CallForProposalsType
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.ClassicalInput
import lucuma.schemas.ObservationDB.Types.DemoScienceInput
import lucuma.schemas.ObservationDB.Types.DirectorsTimeInput
import lucuma.schemas.ObservationDB.Types.FastTurnaroundInput
import lucuma.schemas.ObservationDB.Types.LargeProgramInput
import lucuma.schemas.ObservationDB.Types.PartnerSplitInput
import lucuma.schemas.ObservationDB.Types.PoorWeatherInput
import lucuma.schemas.ObservationDB.Types.ProposalPropertiesInput
import lucuma.schemas.ObservationDB.Types.ProposalTypeInput
import lucuma.schemas.ObservationDB.Types.QueueInput
import lucuma.schemas.ObservationDB.Types.SystemVerificationInput
import lucuma.schemas.ObservationDB.Types.TimeSpanInput

trait ProposalOdbExtensions:
  // This is on import lucuma.schemas.odb.input.* but it is not picked up for some reason
  extension (ts: TimeSpan)
    def toInput: TimeSpanInput = TimeSpanInput.Microseconds(ts.toMicroseconds)

  extension (proposalType: ProposalType)
    def toInput: ProposalTypeInput =
      proposalType match
        case ProposalType.DemoScience(_, toOActivation, minPercentTime)                      =>
          ProposalTypeInput.DemoScience(
            DemoScienceInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case ProposalType.DirectorsTime(_, toOActivation, minPercentTime)                    =>
          ProposalTypeInput.DirectorsTime(
            DirectorsTimeInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case ProposalType.FastTurnaround(_, toOActivation, minPercentTime, reviewer, mentor) =>
          ProposalTypeInput.FastTurnaround(
            FastTurnaroundInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              reviewerId = reviewer.orUnassign,
              mentorId = mentor.orUnassign
            )
          )
        case ProposalType.LargeProgram(
              _,
              toOActivation,
              minPercentTime,
              minPercentTotalTime,
              totalTime
            ) =>
          ProposalTypeInput.LargeProgram(
            LargeProgramInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              minPercentTotalTime = minPercentTotalTime.assign,
              totalTime = totalTime.toInput.assign
            )
          )
        case ProposalType.Classical(_, minPercentTime, partnerSplits)                        =>
          ProposalTypeInput.Classical(
            ClassicalInput(
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign
            )
          )
        case ProposalType.Queue(_, toOActivation, minPercentTime, partnerSplits)             =>
          ProposalTypeInput.Queue(
            QueueInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign,
              partnerSplits =
                if (partnerSplits.nonEmpty) partnerSplits.map(_.toInput).assign else Unassign
            )
          )
        case ProposalType.SystemVerification(_, toOActivation, minPercentTime)               =>
          ProposalTypeInput.SystemVerification(
            SystemVerificationInput(
              toOActivation = toOActivation.assign,
              minPercentTime = minPercentTime.assign
            )
          )
        case ProposalType.PoorWeather(scienceSubtype)                                        =>
          ProposalTypeInput.PoorWeather(PoorWeatherInput())

  // Used to reset the proposal type when the call changes
  extension (cfpType: CallForProposalsType)
    def defaultType(reviewerId: Option[ProgramUser.Id]): ProposalType = cfpType match
      case CallForProposalsType.DemoScience        => ProposalType.DemoScience.Default
      case CallForProposalsType.DirectorsTime      => ProposalType.DirectorsTime.Default
      case CallForProposalsType.FastTurnaround     =>
        ProposalType.FastTurnaround.defaultWithReviewer(reviewerId)
      case CallForProposalsType.LargeProgram       => ProposalType.LargeProgram.Default
      case CallForProposalsType.PoorWeather        => ProposalType.PoorWeather.Default
      case CallForProposalsType.RegularSemester    => ProposalType.Queue.Default
      case CallForProposalsType.SystemVerification => ProposalType.SystemVerification.Default

  extension (split: PartnerSplit)
    def toInput: PartnerSplitInput =
      PartnerSplitInput(partner = split.partner, percent = split.percent)

  extension (proposal: Proposal)
    def toInput: ProposalPropertiesInput =
      ProposalPropertiesInput(
        callId = proposal.call.map(_.id).orUnassign,
        category = proposal.category.orUnassign,
        `type` = proposal.proposalType.map(_.toInput).orUnassign
      )

object ProposalOdbExtensions extends ProposalOdbExtensions
