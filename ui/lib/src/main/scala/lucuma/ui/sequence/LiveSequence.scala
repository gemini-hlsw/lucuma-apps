// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import crystal.Pot
import crystal.react.View
import crystal.react.given
import japgolly.scalajs.react.Reusability
import lucuma.core.enums.Instrument
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.reusability.given

final case class LiveSequence(
  visits:   Pot[View[Option[ExecutionVisits]]],
  sequence: Pot[View[Option[SequenceData]]]
):
  val isReady: Boolean                       = visits.isReady && sequence.isReady
  val sequenceInstrument: Option[Instrument] =
    sequence.toOption.flatMap(_.get).map(_.config.instrument)

object LiveSequence:
  given Reusability[LiveSequence] =
    Reusability.by(x => (x.visits.map(_.get), x.sequence.map(_.get)))
