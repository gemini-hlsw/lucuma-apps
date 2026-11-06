// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.ProposalSubmissionError

/**
 * A reason the proposal cannot be submitted, with the message to show for it. The message is
 * usually the rule's own, but may name the particular case, such as the partner that lacks a lead.
 */
case class ProposalError(error: ProposalSubmissionError, message: String) derives Eq

object ProposalError:
  def apply(error: ProposalSubmissionError): ProposalError =
    ProposalError(error, error.message)
