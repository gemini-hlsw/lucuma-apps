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
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.model.PeakPixel
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

  given Arbitrary[PeakPixel] =
    Arbitrary:
      for
        flux <- Gen.chooseNum(0.0, 1.0e7)
        adu  <- Gen.chooseNum(0, 10000000)
      yield PeakPixel(flux, adu)

  given Cogen[PeakPixel] =
    Cogen[(Double, Int)].contramap(a => (a.flux, a.adu))

  given Arbitrary[ItcResultValues] =
    Arbitrary:
      for
        sn   <- arbitrary[Option[SignalToNoiseAt]]
        peak <- arbitrary[Option[PeakPixel]]
      yield ItcResultValues(sn, peak)

  given Cogen[ItcResultValues] =
    Cogen[(Option[SignalToNoiseAt], Option[PeakPixel])].contramap: a =>
      (a.signalToNoise, a.peakPixel)

  given Arbitrary[ModeSignalToNoise.Spectroscopy] = Arbitrary:
    for
      acquisitionSN <- arbitrary[ItcResultValues]
      scienceSN     <- arbitrary[ItcResultValues]
    yield ModeSignalToNoise.Spectroscopy(acquisitionSN, scienceSN)

  given Cogen[ModeSignalToNoise.Spectroscopy] =
    Cogen[(ItcResultValues, ItcResultValues)].contramap: sn =>
      (sn.acquisition, sn.science)

  given Arbitrary[ModeSignalToNoise.GmosNorthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosNorthFilter, ItcResultValues]]
    yield ModeSignalToNoise.GmosNorthImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GmosNorthImaging] =
    Cogen[List[(lucuma.core.enums.GmosNorthFilter, ItcResultValues)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GmosSouthImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GmosSouthFilter, ItcResultValues]]
    yield ModeSignalToNoise.GmosSouthImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GmosSouthImaging] =
    Cogen[List[(lucuma.core.enums.GmosSouthFilter, ItcResultValues)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.Flamingos2Imaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.Flamingos2Filter, ItcResultValues]]
    yield ModeSignalToNoise.Flamingos2Imaging(scienceSN)

  given Cogen[ModeSignalToNoise.Flamingos2Imaging] =
    Cogen[List[(lucuma.core.enums.Flamingos2Filter, ItcResultValues)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GnirsImaging] = Arbitrary:
    for scienceSN <- arbitrary[Map[lucuma.core.enums.GnirsFilter, ItcResultValues]]
    yield ModeSignalToNoise.GnirsImaging(scienceSN)

  given Cogen[ModeSignalToNoise.GnirsImaging] =
    Cogen[List[(lucuma.core.enums.GnirsFilter, ItcResultValues)]].contramap: sn =>
      sn.science.toList

  given Arbitrary[ModeSignalToNoise.GhostIfu] = Arbitrary:
    for
      redSN  <- arbitrary[ItcResultValues]
      blueSN <- arbitrary[ItcResultValues]
    yield ModeSignalToNoise.GhostIfu(redSN, blueSN)

  given Cogen[ModeSignalToNoise.GhostIfu] =
    Cogen[(ItcResultValues, ItcResultValues)].contramap: sn =>
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
