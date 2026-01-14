// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.arb

import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.arb.ArbInstrumentExecutionConfig.given
import lucuma.ui.sequence.InstrumentSignalToNoise
import lucuma.ui.sequence.SequenceData
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import ArbInstrumentSignalToNoise.given

trait ArbSequenceData:
  given Arbitrary[SequenceData] = Arbitrary:
    for
      config <- arbitrary[InstrumentExecutionConfig]
      sn     <- arbitrary[InstrumentSignalToNoise]
    yield SequenceData(config, sn)

  given Cogen[SequenceData] =
    Cogen[(InstrumentExecutionConfig, InstrumentSignalToNoise)].contramap: sd =>
      (sd.config, sd.signalToNoise)

object ArbSequenceData extends ArbSequenceData
