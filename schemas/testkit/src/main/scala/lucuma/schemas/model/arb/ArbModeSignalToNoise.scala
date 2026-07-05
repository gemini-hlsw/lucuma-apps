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

  given Arbitrary[ModeSignalToNoise.Flamingos2Imaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.Flamingos2Filter, SignalToNoiseAt]]
    yield ModeSignalToNoise.Flamingos2Imaging(scienceSN)

  given Cogen[ModeSignalToNoise.Flamingos2Imaging] =
    Cogen[List[(lucuma.core.enums.Flamingos2Filter, SignalToNoiseAt)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GnirsImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GnirsFilter, SignalToNoiseAt]]
    yield ModeSignalToNoise.GnirsImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GnirsImaging] =
    Cogen[List[(lucuma.core.enums.GnirsFilter, SignalToNoiseAt)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GhostIfu] = Arbitrary:
    for
      redSN  <- arbitrary[Option[SignalToNoiseAt]]
      blueSN <- arbitrary[Option[SignalToNoiseAt]]
    yield ModeSignalToNoise.GhostIfu(redSN, blueSN)

  given Cogen[ModeSignalToNoise.GhostIfu] =
    Cogen[(Option[SignalToNoiseAt], Option[SignalToNoiseAt])].contramap: sn =>
      (sn.red, sn.blue)

  given Arbitrary[ModeSignalToNoise] = Arbitrary:
    Gen.oneOf(
      Gen.const(ModeSignalToNoise.Undefined),
      arbitrary[ModeSignalToNoise.Spectroscopy],
      arbitrary[ModeSignalToNoise.GmosNorthImaging],
      arbitrary[ModeSignalToNoise.GmosSouthImaging],
      arbitrary[ModeSignalToNoise.Flamingos2Imaging],
      arbitrary[ModeSignalToNoise.GnirsImaging],
      arbitrary[ModeSignalToNoise.GhostIfu]
    )

  given Cogen[ModeSignalToNoise] =
    Cogen[Either[
      Unit,
      Either[
        ModeSignalToNoise.Spectroscopy,
        Either[
          ModeSignalToNoise.GmosNorthImaging,
          Either[
            ModeSignalToNoise.GmosSouthImaging,
            Either[
              ModeSignalToNoise.Flamingos2Imaging,
              Either[
                ModeSignalToNoise.GnirsImaging,
                ModeSignalToNoise.GhostIfu
              ]
            ]
          ]
        ]
      ]
    ]].contramap: isn =>
      isn match
        case ModeSignalToNoise.Undefined              => Left(())
        case s: ModeSignalToNoise.Spectroscopy        => Right(Left(s))
        case gnm: ModeSignalToNoise.GmosNorthImaging  => Right(Right(Left(gnm)))
        case gsm: ModeSignalToNoise.GmosSouthImaging  => Right(Right(Right(Left(gsm))))
        case f2i: ModeSignalToNoise.Flamingos2Imaging => Right(Right(Right(Right(Left(f2i)))))
        case gnm: ModeSignalToNoise.GnirsImaging      => Right(Right(Right(Right(Right(Left(gnm))))))
        case gst: ModeSignalToNoise.GhostIfu          => Right(Right(Right(Right(Right(Right(gst))))))

object ArbModeSignalToNoise extends ArbModeSignalToNoise
