// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.arb

import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.arb.ArbSignalToNoise.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.ui.sequence.InstrumentSignalToNoise
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbInstrumentSignalToNoise:
  given Arbitrary[InstrumentSignalToNoise.Spectroscopy] = Arbitrary:
    for
      acquisitionSN <- arbitrary[Option[(SingleSN, TotalSN)]]
      scienceSN     <- arbitrary[Option[(SingleSN, TotalSN)]]
    yield InstrumentSignalToNoise.Spectroscopy(acquisitionSN, scienceSN)

  given Cogen[InstrumentSignalToNoise.Spectroscopy] =
    Cogen[(Option[(SingleSN, TotalSN)], Option[(SingleSN, TotalSN)])].contramap: sn =>
      (sn.acquisition, sn.science)

  given Arbitrary[InstrumentSignalToNoise.GmosNorthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosNorthFilter, (SingleSN, TotalSN)]]
    yield InstrumentSignalToNoise.GmosNorthImaging(scienceSN)

  given Cogen[InstrumentSignalToNoise.GmosNorthImaging] =
    Cogen[List[(lucuma.core.enums.GmosNorthFilter, (SingleSN, TotalSN))]].contramap: sn =>
      sn.science.toList

  given Arbitrary[InstrumentSignalToNoise.GmosSouthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosSouthFilter, (SingleSN, TotalSN)]]
    yield InstrumentSignalToNoise.GmosSouthImaging(scienceSN)

  given Cogen[InstrumentSignalToNoise.GmosSouthImaging] =
    Cogen[List[(lucuma.core.enums.GmosSouthFilter, (SingleSN, TotalSN))]].contramap: sn =>
      sn.science.toList

  given Arbitrary[InstrumentSignalToNoise] = Arbitrary:
    Gen.oneOf(
      Gen.const(InstrumentSignalToNoise.Undefined),
      arbitrary[InstrumentSignalToNoise.Spectroscopy],
      arbitrary[InstrumentSignalToNoise.GmosNorthImaging],
      arbitrary[InstrumentSignalToNoise.GmosSouthImaging]
    )

  given Cogen[InstrumentSignalToNoise] =
    Cogen[Either[
      Unit,
      Either[
        InstrumentSignalToNoise.Spectroscopy,
        Either[
          InstrumentSignalToNoise.GmosNorthImaging,
          InstrumentSignalToNoise.GmosSouthImaging
        ]
      ]
    ]].contramap: isn =>
      isn match
        case InstrumentSignalToNoise.Undefined             => Left(())
        case s: InstrumentSignalToNoise.Spectroscopy       => Right(Left(s))
        case gnm: InstrumentSignalToNoise.GmosNorthImaging => Right(Right(Left(gnm)))
        case gsm: InstrumentSignalToNoise.GmosSouthImaging => Right(Right(Right(gsm)))

object ArbInstrumentSignalToNoise extends ArbInstrumentSignalToNoise
