// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.refined.given
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.enums.SubaruInstrument
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.util.Timestamp
import lucuma.odb.json.limits.decoder.given
import monocle.Prism
import monocle.macros.GenPrism

// The observatory-specific properties of a Call for Proposals. Exactly one of
// the variants applies to a given call, determined by its `observatory`.
sealed trait CallProperties derives Eq

object CallProperties:
  case class GeminiCallProperties(
    cfpType:            GeminiCallForProposalsType,
    coordinateLimits:   CallCoordinatesLimits,
    instruments:        List[Instrument],
    proprietaryMonths:  NonNegInt,
    allowsNonPartnerPi: Boolean,
    nonPartnerDeadline: Option[Timestamp],
    exchangePartners:   List[ExchangePartner]
  ) extends CallProperties derives Eq

  object GeminiCallProperties:
    given Decoder[GeminiCallProperties] = c =>
      for {
        cfpType            <- c.downField("cfpType").as[GeminiCallForProposalsType]
        coordinateLimits   <- c.downField("coordinateLimits").as[CallCoordinatesLimits]
        instruments        <- c.downField("instruments").as[List[Instrument]]
        proprietaryMonths  <- c.downField("proprietaryMonths").as[NonNegInt]
        allowsNonPartnerPi <- c.downField("allowsNonPartnerPi").as[Boolean]
        nonPartnerDeadline <- c.downField("nonPartnerDeadline").as[Option[Timestamp]]
        exchangePartners   <- c.downField("exchangePartners").as[List[ExchangePartner]]
      } yield GeminiCallProperties(cfpType,
                                   coordinateLimits,
                                   instruments,
                                   proprietaryMonths,
                                   allowsNonPartnerPi,
                                   nonPartnerDeadline,
                                   exchangePartners
      )

  case class KeckCallProperties(
    instruments:      List[KeckInstrument],
    coordinateLimits: SiteCoordinatesLimits
  ) extends CallProperties derives Eq

  object KeckCallProperties:
    given Decoder[KeckCallProperties] = c =>
      for {
        instruments      <- c.downField("instruments").as[List[KeckInstrument]]
        coordinateLimits <- c.downField("coordinateLimits").as[SiteCoordinatesLimits]
      } yield KeckCallProperties(instruments, coordinateLimits)

  case class SubaruCallProperties(
    cfpType:          SubaruCallForProposalsType,
    instruments:      List[SubaruInstrument],
    coordinateLimits: SiteCoordinatesLimits
  ) extends CallProperties derives Eq

  object SubaruCallProperties:
    given Decoder[SubaruCallProperties] = c =>
      for {
        cfpType          <- c.downField("cfpType").as[SubaruCallForProposalsType]
        instruments      <- c.downField("instruments").as[List[SubaruInstrument]]
        coordinateLimits <- c.downField("coordinateLimits").as[SiteCoordinatesLimits]
      } yield SubaruCallProperties(cfpType, instruments, coordinateLimits)

  val gemini: Prism[CallProperties, GeminiCallProperties] =
    GenPrism[CallProperties, GeminiCallProperties]
  val keck: Prism[CallProperties, KeckCallProperties]     =
    GenPrism[CallProperties, KeckCallProperties]
  val subaru: Prism[CallProperties, SubaruCallProperties] =
    GenPrism[CallProperties, SubaruCallProperties]

  // Decodes from the enclosing `CallForProposals` object, which carries the
  // mutually-exclusive `gemini` / `keck` / `subaru` property blocks.
  given Decoder[CallProperties] = c =>
    for {
      gemini <- c.downField("gemini").as[Option[GeminiCallProperties]]
      keck   <- c.downField("keck").as[Option[KeckCallProperties]]
      subaru <- c.downField("subaru").as[Option[SubaruCallProperties]]
      result <- (gemini: Option[CallProperties])
                  .orElse(keck)
                  .orElse(subaru)
                  .toRight(
                    DecodingFailure("CallForProposals has no observatory properties", c.history)
                  )
    } yield result
