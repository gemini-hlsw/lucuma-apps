// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbSignalToNoise.given
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.itc.SignalToNoiseAt
import lucuma.schemas.model.ModeSignalToNoise
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbModeSignalToNoise:
  // TODO This exists in the ODB but is not published, we should move it to ITC client maybe?
  given Arbitrary[SignalToNoiseAt] =
    Arbitrary:
      for
        w <- arbitrary[Wavelength]
        s <- arbitrary[SignalToNoise]
        t <- arbitrary[SignalToNoise]
      yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

  given Cogen[SignalToNoiseAt] =
    Cogen[(Wavelength, SignalToNoise, SignalToNoise)].contramap: a =>
      (a.wavelength, a.single.value, a.total.value)

  given Arbitrary[ModeSignalToNoise.Spectroscopy] = Arbitrary:
    for
      acquisitionSN <- arbitrary[Option[SignalToNoiseAt]]
      scienceSN     <- arbitrary[Option[SignalToNoiseAt]]
    yield ModeSignalToNoise.Spectroscopy(acquisitionSN, scienceSN)

  given Cogen[ModeSignalToNoise.Spectroscopy] =
    Cogen[(Option[SignalToNoiseAt], Option[SignalToNoiseAt])].contramap: sn =>
      (sn.acquisition, sn.science)

  given Arbitrary[ModeSignalToNoise.GmosNorthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosNorthFilter, SignalToNoiseAt]]
    yield ModeSignalToNoise.GmosNorthImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GmosNorthImaging] =
    Cogen[List[(lucuma.core.enums.GmosNorthFilter, SignalToNoiseAt)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GmosSouthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosSouthFilter, SignalToNoiseAt]]
    yield ModeSignalToNoise.GmosSouthImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GmosSouthImaging] =
    Cogen[List[(lucuma.core.enums.GmosSouthFilter, SignalToNoiseAt)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise] = Arbitrary:
    Gen.oneOf(
      Gen.const(ModeSignalToNoise.Undefined),
      arbitrary[ModeSignalToNoise.Spectroscopy],
      arbitrary[ModeSignalToNoise.GmosNorthImaging],
      arbitrary[ModeSignalToNoise.GmosSouthImaging]
    )

  given Cogen[ModeSignalToNoise] =
    Cogen[Either[
      Unit,
      Either[
        ModeSignalToNoise.Spectroscopy,
        Either[
          ModeSignalToNoise.GmosNorthImaging,
          ModeSignalToNoise.GmosSouthImaging
        ]
      ]
    ]].contramap: isn =>
      isn match
        case ModeSignalToNoise.Undefined             => Left(())
        case s: ModeSignalToNoise.Spectroscopy       => Right(Left(s))
        case gnm: ModeSignalToNoise.GmosNorthImaging => Right(Right(Left(gnm)))
        case gsm: ModeSignalToNoise.GmosSouthImaging => Right(Right(Right(gsm)))

object ArbModeSignalToNoise extends ArbModeSignalToNoise
