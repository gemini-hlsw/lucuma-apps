// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import eu.timepit.refined.scalacheck.all.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.itc.ItcGhostDetector
import lucuma.itc.arb.ArbItcGhostDetector.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbBasicConfiguration {
  import ArbAngle.given
  import lucuma.core.model.sequence.gnirs.GnirsFpu
  import lucuma.core.util.arb.ArbEnumerated.given

  given Arbitrary[GnirsFpu.Spectroscopy] = Arbitrary(
    Gen.oneOf(
      arbitrary[GnirsFpuSlit].map(GnirsFpu.Spectroscopy.Slit(_)),
      arbitrary[GnirsFpuIfu].map(GnirsFpu.Spectroscopy.Ifu(_))
    )
  )

  given Cogen[GnirsFpu.Spectroscopy] =
    Cogen[Either[GnirsFpuSlit, GnirsFpuIfu]].contramap:
      case GnirsFpu.Spectroscopy.Slit(s) => Left(s)
      case GnirsFpu.Spectroscopy.Ifu(i)  => Right(i)
  import ArbWavelength.given

  given Arbitrary[BasicConfiguration.GmosNorthLongSlit] =
    Arbitrary[BasicConfiguration.GmosNorthLongSlit](
      for {
        grating <- arbitrary[GmosNorthGrating]
        filter  <- arbitrary[Option[GmosNorthFilter]]
        fpu     <- arbitrary[GmosNorthFpu]
        cw      <- arbitrary[Wavelength]
      } yield BasicConfiguration.GmosNorthLongSlit(
        grating,
        filter,
        fpu,
        CentralWavelength(cw)
      )
    )

  given Arbitrary[BasicConfiguration.GmosSouthLongSlit] =
    Arbitrary[BasicConfiguration.GmosSouthLongSlit](
      for {
        grating <- arbitrary[GmosSouthGrating]
        filter  <- arbitrary[Option[GmosSouthFilter]]
        fpu     <- arbitrary[GmosSouthFpu]
        cw      <- arbitrary[Wavelength]
      } yield BasicConfiguration.GmosSouthLongSlit(
        grating,
        filter,
        fpu,
        CentralWavelength(cw)
      )
    )

  given Arbitrary[BasicConfiguration.GmosNorthImaging] =
    Arbitrary[BasicConfiguration.GmosNorthImaging](
      for {
        filter <- arbitrary[NonEmptyList[GmosNorthFilter]]
      } yield BasicConfiguration.GmosNorthImaging(filter)
    )

  given Arbitrary[BasicConfiguration.GmosSouthImaging] =
    Arbitrary[BasicConfiguration.GmosSouthImaging](
      for {
        filter <- arbitrary[NonEmptyList[GmosSouthFilter]]
      } yield BasicConfiguration.GmosSouthImaging(filter)
    )

  given Arbitrary[BasicConfiguration.Flamingos2LongSlit] =
    Arbitrary[BasicConfiguration.Flamingos2LongSlit](
      for {
        disperser <- arbitrary[Flamingos2Disperser]
        filter    <- arbitrary[Flamingos2Filter]
        fpu       <- arbitrary[Flamingos2Fpu]
      } yield BasicConfiguration.Flamingos2LongSlit(disperser, filter, fpu)
    )

  given Arbitrary[BasicConfiguration.Flamingos2Imaging] =
    Arbitrary[BasicConfiguration.Flamingos2Imaging](
      for {
        filters <- arbitrary[NonEmptyList[Flamingos2Filter]]
      } yield BasicConfiguration.Flamingos2Imaging(filters)
    )

  given Arbitrary[BasicConfiguration.Igrins2LongSlit.type] =
    Arbitrary[BasicConfiguration.Igrins2LongSlit.type](
      Gen.const(BasicConfiguration.Igrins2LongSlit)
    )

  given Arbitrary[BasicConfiguration.GnirsSpectroscopy] =
    Arbitrary[BasicConfiguration.GnirsSpectroscopy](
      for {
        filter  <- arbitrary[GnirsFilter]
        fpu     <- arbitrary[GnirsFpu.Spectroscopy]
        prism   <- arbitrary[GnirsPrism]
        grating <- arbitrary[GnirsGrating]
        camera  <- arbitrary[GnirsCamera]
        cw      <- arbitrary[Wavelength]
      } yield BasicConfiguration.GnirsSpectroscopy(
        filter,
        fpu,
        prism,
        grating,
        camera,
        CentralWavelength(cw)
      )
    )

  given Arbitrary[BasicConfiguration.GhostIfu] =
    Arbitrary[BasicConfiguration.GhostIfu](
      for {
        resolutionMode <- arbitrary[GhostResolutionMode]
        stepCount      <- arbitrary[PosInt]
        snAt           <- arbitrary[Wavelength]
        red            <- arbitrary[ItcGhostDetector]
        blue           <- arbitrary[ItcGhostDetector]
      } yield BasicConfiguration.GhostIfu(resolutionMode, stepCount, snAt, red = red, blue = blue)
    )

  given Arbitrary[BasicConfiguration.Visitor] =
    Arbitrary[BasicConfiguration.Visitor](
      for {
        mode <- arbitrary[VisitorObservingModeType]
        cw   <- arbitrary[Wavelength]
        gsms <- arbitrary[Angle]
      } yield BasicConfiguration.Visitor(mode, CentralWavelength(cw), gsms)
    )

  given Arbitrary[BasicConfiguration.KeckExchange] =
    Arbitrary[BasicConfiguration.KeckExchange](
      for {
        keckInstrument <- arbitrary[KeckInstrument]
        requested      <- arbitrary[TimeSpan]
      } yield BasicConfiguration.KeckExchange(keckInstrument, requested)
    )

  given Arbitrary[BasicConfiguration.SubaruExchange] =
    Arbitrary[BasicConfiguration.SubaruExchange](
      for {
        subaruInstrument <- arbitrary[SubaruInstrument]
        requested        <- arbitrary[TimeSpan]
      } yield BasicConfiguration.SubaruExchange(subaruInstrument, requested)
    )

  given Arbitrary[BasicConfiguration] = Arbitrary[BasicConfiguration](
    Gen.oneOf(
      arbitrary[BasicConfiguration.GmosNorthLongSlit],
      arbitrary[BasicConfiguration.GmosSouthLongSlit],
      arbitrary[BasicConfiguration.GmosNorthImaging],
      arbitrary[BasicConfiguration.GmosSouthImaging],
      arbitrary[BasicConfiguration.GnirsSpectroscopy],
      arbitrary[BasicConfiguration.Flamingos2LongSlit],
      arbitrary[BasicConfiguration.Flamingos2Imaging],
      arbitrary[BasicConfiguration.Igrins2LongSlit.type],
      arbitrary[BasicConfiguration.GhostIfu],
      arbitrary[BasicConfiguration.Visitor],
      arbitrary[BasicConfiguration.KeckExchange],
      arbitrary[BasicConfiguration.SubaruExchange]
    )
  )

  given Cogen[BasicConfiguration.GmosNorthLongSlit] =
    Cogen[
      (GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu)
    ]
      .contramap(o =>
        (
          o.grating,
          o.filter,
          o.fpu
        )
      )

  given Cogen[BasicConfiguration.GmosSouthLongSlit] =
    Cogen[
      (GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu)
    ]
      .contramap(o =>
        (
          o.grating,
          o.filter,
          o.fpu
        )
      )

  given Cogen[BasicConfiguration.Flamingos2LongSlit] =
    Cogen[
      (Flamingos2Disperser, Flamingos2Filter, Flamingos2Fpu)
    ]
      .contramap(o =>
        (
          o.disperser,
          o.filter,
          o.fpu
        )
      )

  given Cogen[BasicConfiguration.Igrins2LongSlit.type] =
    Cogen[Unit].contramap(_ => ())

  given Cogen[BasicConfiguration.GnirsSpectroscopy] =
    Cogen[(GnirsFilter, GnirsFpu.Spectroscopy, GnirsPrism, GnirsGrating, GnirsCamera, Wavelength)]
      .contramap(o => (o.filter, o.fpu, o.prism, o.grating, o.camera, o.centralWavelength.value))

  given Cogen[BasicConfiguration.GmosNorthImaging] =
    Cogen[NonEmptyList[GmosNorthFilter]]
      .contramap(_.filters)

  given Cogen[BasicConfiguration.GmosSouthImaging] =
    Cogen[NonEmptyList[GmosSouthFilter]]
      .contramap(_.filters)

  given Cogen[BasicConfiguration.Flamingos2Imaging] =
    Cogen[NonEmptyList[Flamingos2Filter]]
      .contramap(_.filters)

  given Cogen[BasicConfiguration.GhostIfu] =
    Cogen[(GhostResolutionMode, ItcGhostDetector, ItcGhostDetector)]
      .contramap(o => (o.resolutionMode, o.red, o.blue))

  given Cogen[BasicConfiguration.Visitor] =
    Cogen[(VisitorObservingModeType, Wavelength, Angle)]
      .contramap(o => (o.mode, o.centralWavelength.value, o.agsDiameter))

  given Cogen[BasicConfiguration.KeckExchange] =
    Cogen[(KeckInstrument, TimeSpan)]
      .contramap(o => (o.keckInstrument, o.totalRequestTime))

  given Cogen[BasicConfiguration.SubaruExchange] =
    Cogen[(SubaruInstrument, TimeSpan)]
      .contramap(o => (o.subaruInstrument, o.totalRequestTime))

  given Cogen[BasicConfiguration] =
    Cogen[Either[
      BasicConfiguration.Igrins2LongSlit.type,
      Either[
        BasicConfiguration.Flamingos2LongSlit,
        Either[
          BasicConfiguration.GmosNorthLongSlit,
          Either[
            BasicConfiguration.GmosSouthLongSlit,
            Either[
              BasicConfiguration.GhostIfu,
              Either[
                BasicConfiguration.GnirsSpectroscopy,
                Either[
                  BasicConfiguration.GmosNorthImaging,
                  Either[
                    BasicConfiguration.GmosSouthImaging,
                    Either[
                      BasicConfiguration.Flamingos2Imaging,
                      Either[
                        BasicConfiguration.Visitor,
                        Either[
                          BasicConfiguration.KeckExchange,
                          BasicConfiguration.SubaruExchange
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]]
      .contramap:
        case BasicConfiguration.Igrins2LongSlit       => BasicConfiguration.Igrins2LongSlit.asLeft
        case f: BasicConfiguration.Flamingos2LongSlit => f.asLeft.asRight
        case n: BasicConfiguration.GmosNorthLongSlit  => n.asLeft.asRight.asRight
        case s: BasicConfiguration.GmosSouthLongSlit  => s.asLeft.asRight.asRight.asRight
        case g: BasicConfiguration.GhostIfu           => g.asLeft.asRight.asRight.asRight.asRight
        case i: BasicConfiguration.GnirsSpectroscopy  =>
          i.asLeft.asRight.asRight.asRight.asRight.asRight
        case n: BasicConfiguration.GmosNorthImaging   =>
          n.asLeft.asRight.asRight.asRight.asRight.asRight.asRight
        case s: BasicConfiguration.GmosSouthImaging   =>
          s.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case f: BasicConfiguration.Flamingos2Imaging  =>
          f.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case v: BasicConfiguration.Visitor            =>
          v.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case k: BasicConfiguration.KeckExchange       =>
          k.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case s: BasicConfiguration.SubaruExchange     =>
          s.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight

}

object ArbBasicConfiguration extends ArbBasicConfiguration
