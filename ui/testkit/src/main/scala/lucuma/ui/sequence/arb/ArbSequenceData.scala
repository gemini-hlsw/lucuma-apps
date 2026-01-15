// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.arb

import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.arb.ArbInstrumentExecutionConfig.given
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.model.arb.ArbModeSignalToNoise
import lucuma.ui.sequence.SequenceData
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import ArbModeSignalToNoise.given

trait ArbSequenceData:
  given Arbitrary[SequenceData] = Arbitrary:
    for
      config <- arbitrary[InstrumentExecutionConfig]
      sn     <- arbitrary[ModeSignalToNoise]
    yield SequenceData(config, sn)

  given Cogen[SequenceData] =
    Cogen[(InstrumentExecutionConfig, ModeSignalToNoise)].contramap: sd =>
      (sd.config, sd.signalToNoise)

object ArbSequenceData extends ArbSequenceData
