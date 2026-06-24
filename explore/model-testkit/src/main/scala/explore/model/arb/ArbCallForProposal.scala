// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.model.arb.ArbCallCoordinatesLimits.given
import lucuma.core.model.arb.ArbSiteCoordinatesLimits.given
import lucuma.core.util.arb.ArbDateInterval.given
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimestamp.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.CallForProposal
import explore.model.CallPartner
import explore.model.CallProperties
import explore.model.CallProperties.GeminiCallProperties
import explore.model.CallProperties.KeckCallProperties
import explore.model.CallProperties.SubaruCallProperties
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.Partner
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.enums.SubaruInstrument
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CallForProposals
import lucuma.core.model.Semester
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp

trait ArbCallForProposal {
  import ArbEnumerated.given

  given Arbitrary[CallPartner] =
    Arbitrary {
      for {
        partner  <- arbitrary[Partner]
        deadline <- arbitrary[Option[Timestamp]]
      } yield CallPartner(partner, deadline)
    }

  given Cogen[CallPartner] =
    Cogen[(Partner, Option[Timestamp])].contramap(p => (p.partner, p.submissionDeadline))

  given Arbitrary[GeminiCallProperties] =
    Arbitrary {
      for {
        cfpType            <- arbitrary[GeminiCallForProposalsType]
        coordinateLimits   <- arbitrary[CallCoordinatesLimits]
        instruments        <- arbitrary[List[Instrument]]
        proprietaryMonths  <- arbitrary[NonNegInt]
        allowsNonPartnerPi <- arbitrary[Boolean]
        nonPartnerDeadline <- arbitrary[Option[Timestamp]]
        exchangePartners   <- arbitrary[List[ExchangePartner]]
      } yield GeminiCallProperties(cfpType,
                                   coordinateLimits,
                                   instruments,
                                   proprietaryMonths,
                                   allowsNonPartnerPi,
                                   nonPartnerDeadline,
                                   exchangePartners
      )
    }

  given Cogen[GeminiCallProperties] =
    Cogen[
      (GeminiCallForProposalsType,
       CallCoordinatesLimits,
       List[Instrument],
       NonNegInt,
       Boolean,
       Option[Timestamp],
       List[ExchangePartner]
      )
    ].contramap: p =>
      (p.cfpType,
       p.coordinateLimits,
       p.instruments,
       p.proprietaryMonths,
       p.allowsNonPartnerPi,
       p.nonPartnerDeadline,
       p.exchangePartners
      )

  given Arbitrary[KeckCallProperties] =
    Arbitrary {
      for {
        instruments      <- arbitrary[List[KeckInstrument]]
        coordinateLimits <- arbitrary[SiteCoordinatesLimits]
      } yield KeckCallProperties(instruments, coordinateLimits)
    }

  given Cogen[KeckCallProperties] =
    Cogen[(List[KeckInstrument], SiteCoordinatesLimits)]
      .contramap(p => (p.instruments, p.coordinateLimits))

  given Arbitrary[SubaruCallProperties] =
    Arbitrary {
      for {
        cfpType          <- arbitrary[SubaruCallForProposalsType]
        instruments      <- arbitrary[List[SubaruInstrument]]
        coordinateLimits <- arbitrary[SiteCoordinatesLimits]
      } yield SubaruCallProperties(cfpType, instruments, coordinateLimits)
    }

  given Cogen[SubaruCallProperties] =
    Cogen[(SubaruCallForProposalsType, List[SubaruInstrument], SiteCoordinatesLimits)]
      .contramap(p => (p.cfpType, p.instruments, p.coordinateLimits))

  given Arbitrary[CallProperties] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[GeminiCallProperties],
        arbitrary[KeckCallProperties],
        arbitrary[SubaruCallProperties]
      )
    }

  given Cogen[CallProperties] =
    Cogen[Either[GeminiCallProperties, Either[KeckCallProperties, SubaruCallProperties]]].contramap {
      case g: GeminiCallProperties => Left(g)
      case k: KeckCallProperties   => Right(Left(k))
      case s: SubaruCallProperties => Right(Right(s))
    }

  given Arbitrary[CallForProposal] =
    Arbitrary {
      for {
        id             <- arbitrary[CallForProposals.Id]
        semester       <- arbitrary[Semester]
        title          <- arbitrary[NonEmptyString]
        partners       <- arbitrary[List[CallPartner]]
        active         <- arbitrary[DateInterval]
        callProperties <- arbitrary[CallProperties]
      } yield CallForProposal(id, semester, title, partners, active, callProperties)
    }

  given Cogen[CallForProposal] =
    Cogen[
      (CallForProposals.Id,
       Semester,
       NonEmptyString,
       List[CallPartner],
       DateInterval,
       CallProperties
      )
    ].contramap: p =>
      (p.id, p.semester, p.title, p.partners, p.active, p.callProperties)
}

object ArbCallForProposal extends ArbCallForProposal
