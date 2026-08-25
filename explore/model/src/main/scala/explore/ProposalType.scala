// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.refined.*
import lucuma.core.enums.ConsiderForBand3
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

// The proposal type, discriminated by observatory. Exactly one variant applies
// to a given proposal. `GeminiProposalType` is itself a sub-hierarchy of the
// various Gemini science subtypes.
sealed trait ProposalType derives Eq

object ProposalType:
  val geminiProposalType: Prism[ProposalType, GeminiProposalType] =
    GenPrism[ProposalType, GeminiProposalType]
  val keckProposalType: Prism[ProposalType, KeckProposalType]     =
    GenPrism[ProposalType, KeckProposalType]
  val subaruProposalType: Prism[ProposalType, SubaruProposalType] =
    GenPrism[ProposalType, SubaruProposalType]

  // Exchange proposal requesting time at Keck.
  case class KeckProposalType(minPercentTime: IntPercent, partnerSplits: List[PartnerSplit])
      extends ProposalType derives Eq

  object KeckProposalType:
    val minPercentTime: Lens[KeckProposalType, IntPercent]        =
      Focus[KeckProposalType](_.minPercentTime)
    val partnerSplits: Lens[KeckProposalType, List[PartnerSplit]] =
      Focus[KeckProposalType](_.partnerSplits)

    val Default: KeckProposalType =
      KeckProposalType(minPercentTime = IntPercent.unsafeFrom(100), partnerSplits = List.empty)

    given Decoder[KeckProposalType] = c =>
      for {
        minPercentTime <- c.downField("minPercentTime").as[IntPercent]
        partnerSplits  <- c.downField("partnerSplits").as[List[PartnerSplit]]
      } yield KeckProposalType(minPercentTime, partnerSplits)

  // Exchange proposal requesting time at Subaru.
  case class SubaruProposalType(
    minPercentTime: IntPercent,
    partnerSplits:  List[PartnerSplit]
  ) extends ProposalType derives Eq

  object SubaruProposalType:
    val minPercentTime: Lens[SubaruProposalType, IntPercent]        =
      Focus[SubaruProposalType](_.minPercentTime)
    val partnerSplits: Lens[SubaruProposalType, List[PartnerSplit]] =
      Focus[SubaruProposalType](_.partnerSplits)

    val Default: SubaruProposalType =
      SubaruProposalType(
        minPercentTime = IntPercent.unsafeFrom(100),
        partnerSplits = List.empty
      )

    given Decoder[SubaruProposalType] = c =>
      for {
        minPercentTime <- c.downField("minPercentTime").as[IntPercent]
        partnerSplits  <- c.downField("partnerSplits").as[List[PartnerSplit]]
      } yield SubaruProposalType(minPercentTime, partnerSplits)

  // The Gemini proposal type, further discriminated by science subtype.
  sealed trait GeminiProposalType extends ProposalType derives Eq {
    val scienceSubtype: ScienceSubtype
  }

  object GeminiProposalType:
    def toScienceSubtype(s: ScienceSubtype): GeminiProposalType => GeminiProposalType =
      s match
        case ScienceSubtype.Classical => {
          case Queue(_, _, minTime, splits, exchange, aeon, jwst, lt, _) =>
            Classical(ScienceSubtype.Classical, minTime, splits, exchange, aeon, jwst, lt)
          case i                                                         => i
        }
        case ScienceSubtype.Queue     => {
          case Classical(_, minTime, splits, exchange, aeon, jwst, lt) =>
            // On conversion consider for band 3 gets unset.
            Queue(ScienceSubtype.Queue,
                  TooActivationCeiling.Default,
                  minTime,
                  splits,
                  exchange,
                  aeon,
                  jwst,
                  lt,
                  ConsiderForBand3.Unset
            )
          case i                                                       => i
        }
        case _                        => identity

    val tooActivationCeiling: Optional[GeminiProposalType, TooActivationCeiling] =
      Optional[GeminiProposalType, TooActivationCeiling] {
        case d: DemoScience        => d.tooActivationCeiling.some
        case d: DirectorsTime      => d.tooActivationCeiling.some
        case d: FastTurnaround     => d.tooActivationCeiling.some
        case d: LargeProgram       => d.tooActivationCeiling.some
        case d: Queue              => d.tooActivationCeiling.some
        case d: SystemVerification => d.tooActivationCeiling.some
        case _                     => none
      }(a => {
        case d: DemoScience        => d.copy(tooActivationCeiling = a)
        case d: DirectorsTime      => d.copy(tooActivationCeiling = a)
        case d: FastTurnaround     => d.copy(tooActivationCeiling = a)
        case d: LargeProgram       => d.copy(tooActivationCeiling = a)
        case d: Queue              => d.copy(tooActivationCeiling = a)
        case d: SystemVerification => d.copy(tooActivationCeiling = a)
        case i                     => i
      })

    val partnerSplits: Optional[GeminiProposalType, List[PartnerSplit]] =
      Optional[GeminiProposalType, List[PartnerSplit]] {
        case c: Classical => c.partnerSplits.some
        case q: Queue     => q.partnerSplits.some
        case _            => none
      }(a => {
        case c: Classical => c.copy(partnerSplits = a)
        case q: Queue     => q.copy(partnerSplits = a)
        case i            => i
      })

    // Only Queue and Classical proposals can request time on behalf of an
    // exchange partner community, and only when the PI belongs to one.
    val exchangePartner: Optional[GeminiProposalType, Option[ExchangePartner]] =
      Optional[GeminiProposalType, Option[ExchangePartner]] {
        case c: Classical => c.exchangePartner.some
        case q: Queue     => q.exchangePartner.some
        case _            => none
      }(a => {
        case c: Classical => c.copy(exchangePartner = a)
        case q: Queue     => q.copy(exchangePartner = a)
        case i            => i
      })

    // The time request is either assigned to an exchange partner community or
    // apportioned across Gemini partners, never both.
    def withExchangePartner(
      ep: Option[ExchangePartner]
    ): GeminiProposalType => GeminiProposalType =
      gpt =>
        val withEp = exchangePartner.replace(ep)(gpt)
        if ep.isDefined then partnerSplits.replace(List.empty)(withEp) else withEp

    val minPercentTime: Optional[GeminiProposalType, IntPercent] =
      Optional[GeminiProposalType, IntPercent] {
        case c: Classical          => c.minPercentTime.some
        case d: DemoScience        => d.minPercentTime.some
        case d: DirectorsTime      => d.minPercentTime.some
        case d: FastTurnaround     => d.minPercentTime.some
        case d: LargeProgram       => d.minPercentTime.some
        case d: Queue              => d.minPercentTime.some
        case d: SystemVerification => d.minPercentTime.some
        case _                     => none
      }(a => {
        case c: Classical          => c.copy(minPercentTime = a)
        case d: DemoScience        => d.copy(minPercentTime = a)
        case d: DirectorsTime      => d.copy(minPercentTime = a)
        case d: FastTurnaround     => d.copy(minPercentTime = a)
        case d: LargeProgram       => d.copy(minPercentTime = a)
        case d: Queue              => d.copy(minPercentTime = a)
        case d: SystemVerification => d.copy(minPercentTime = a)
        case i                     => i
      })

    val minPercentTotalTime: Optional[GeminiProposalType, IntPercent] =
      Optional[GeminiProposalType, IntPercent] {
        case l: LargeProgram => l.minPercentTotalTime.some
        case _               => none
      }(a => {
        case l: LargeProgram => l.copy(minPercentTotalTime = a)
        case i               => i
      })

    val totalTime: Optional[GeminiProposalType, TimeSpan] =
      Optional[GeminiProposalType, TimeSpan] {
        case l: LargeProgram => l.totalTime.some
        case _               => none
      }(a => {
        case l: LargeProgram => l.copy(totalTime = a)
        case i               => i
      })

    val aeonMultiFacility: Optional[GeminiProposalType, Boolean] =
      Optional[GeminiProposalType, Boolean] {
        case c: Classical    => c.aeonMultiFacility.some
        case l: LargeProgram => l.aeonMultiFacility.some
        case q: Queue        => q.aeonMultiFacility.some
        case _               => none
      }(a => {
        case c: Classical    => c.copy(aeonMultiFacility = a)
        case l: LargeProgram => l.copy(aeonMultiFacility = a)
        case q: Queue        => q.copy(aeonMultiFacility = a)
        case i               => i
      })

    val jwstSynergy: Optional[GeminiProposalType, Boolean] =
      Optional[GeminiProposalType, Boolean] {
        case c: Classical    => c.jwstSynergy.some
        case l: LargeProgram => l.jwstSynergy.some
        case q: Queue        => q.jwstSynergy.some
        case _               => none
      }(a => {
        case c: Classical    => c.copy(jwstSynergy = a)
        case l: LargeProgram => l.copy(jwstSynergy = a)
        case q: Queue        => q.copy(jwstSynergy = a)
        case i               => i
      })

    val usLongTerm: Optional[GeminiProposalType, Boolean] =
      Optional[GeminiProposalType, Boolean] {
        case c: Classical => c.usLongTerm.some
        case q: Queue     => q.usLongTerm.some
        case _            => none
      }(a => {
        case c: Classical => c.copy(usLongTerm = a)
        case q: Queue     => q.copy(usLongTerm = a)
        case i            => i
      })

    val considerForBand3: Optional[GeminiProposalType, ConsiderForBand3] =
      Optional[GeminiProposalType, ConsiderForBand3] {
        case q: Queue => q.considerForBand3.some
        case _        => none
      }(a => {
        case q: Queue => q.copy(considerForBand3 = a)
        case i        => i
      })

    // Define the Classical case class implementing GeminiProposalType
    case class Classical(
      scienceSubtype:    ScienceSubtype,
      minPercentTime:    IntPercent,
      partnerSplits:     List[PartnerSplit],
      exchangePartner:   Option[ExchangePartner],
      aeonMultiFacility: Boolean,
      jwstSynergy:       Boolean,
      usLongTerm:        Boolean
    ) extends GeminiProposalType derives Eq

    object Classical {
      val minPercentTime: Lens[Classical, IntPercent]               = Focus[Classical](_.minPercentTime)
      val exchangePartner: Lens[Classical, Option[ExchangePartner]] =
        Focus[Classical](_.exchangePartner)
      val aeonMultiFacility: Lens[Classical, Boolean]               = Focus[Classical](_.aeonMultiFacility)
      val jwstSynergy: Lens[Classical, Boolean]                     = Focus[Classical](_.jwstSynergy)
      val usLongTerm: Lens[Classical, Boolean]                      = Focus[Classical](_.usLongTerm)

      val Default: Classical =
        Classical(ScienceSubtype.Classical, 100.refined, List.empty, none, false, false, false)
    }

    // Define the DemoScience case class implementing GeminiProposalType
    case class DemoScience(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent
    ) extends GeminiProposalType derives Eq

    object DemoScience {
      val minPercentTime: Lens[DemoScience, IntPercent]                 = Focus[DemoScience](_.minPercentTime)
      val tooActivationCeiling: Lens[DemoScience, TooActivationCeiling] =
        Focus[DemoScience](_.tooActivationCeiling)

      val Default: DemoScience =
        DemoScience(ScienceSubtype.DemoScience, TooActivationCeiling.Default, 100.refined)
    }

    // Define the DirectorsTime case class implementing GeminiProposalType
    case class DirectorsTime(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent
    ) extends GeminiProposalType derives Eq

    object DirectorsTime {
      val minPercentTime: Lens[DirectorsTime, IntPercent]                 = Focus[DirectorsTime](_.minPercentTime)
      val tooActivationCeiling: Lens[DirectorsTime, TooActivationCeiling] =
        Focus[DirectorsTime](_.tooActivationCeiling)

      val Default: DirectorsTime =
        DirectorsTime(ScienceSubtype.DirectorsTime, TooActivationCeiling.Default, 100.refined)
    }

    // Define the FastTurnaround case class implementing GeminiProposalType
    case class FastTurnaround(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent,
      reviewerId:           Option[ProgramUser.Id],
      mentorId:             Option[ProgramUser.Id]
    ) extends GeminiProposalType derives Eq

    object FastTurnaround {
      val minPercentTime: Lens[FastTurnaround, IntPercent]                 = Focus[FastTurnaround](_.minPercentTime)
      val tooActivationCeiling: Lens[FastTurnaround, TooActivationCeiling] =
        Focus[FastTurnaround](_.tooActivationCeiling)
      val reviewerId: Lens[FastTurnaround, Option[ProgramUser.Id]]         =
        Focus[FastTurnaround](_.reviewerId)
      val mentorId: Lens[FastTurnaround, Option[ProgramUser.Id]]           = Focus[FastTurnaround](_.mentorId)

      val Default: FastTurnaround =
        FastTurnaround(ScienceSubtype.FastTurnaround,
                       TooActivationCeiling.Default,
                       100.refined,
                       None,
                       None
        )

      def defaultWithReviewer(id: Option[ProgramUser.Id]): FastTurnaround =
        reviewerId.replace(id)(Default)
    }

    // Define the LargeProgram case class implementing GeminiProposalType
    case class LargeProgram(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent,
      minPercentTotalTime:  IntPercent,
      totalTime:            TimeSpan,
      aeonMultiFacility:    Boolean,
      jwstSynergy:          Boolean
    ) extends GeminiProposalType derives Eq

    object LargeProgram {
      val minPercentTime: Lens[LargeProgram, IntPercent]                 = Focus[LargeProgram](_.minPercentTime)
      val minPercentTotalTime: Lens[LargeProgram, IntPercent]            =
        Focus[LargeProgram](_.minPercentTotalTime)
      val tooActivationCeiling: Lens[LargeProgram, TooActivationCeiling] =
        Focus[LargeProgram](_.tooActivationCeiling)
      val totalTime: Lens[LargeProgram, TimeSpan]                        = Focus[LargeProgram](_.totalTime)
      val aeonMultiFacility: Lens[LargeProgram, Boolean]                 =
        Focus[LargeProgram](_.aeonMultiFacility)
      val jwstSynergy: Lens[LargeProgram, Boolean]                       = Focus[LargeProgram](_.jwstSynergy)

      val Default: LargeProgram =
        LargeProgram(ScienceSubtype.LargeProgram,
                     TooActivationCeiling.Default,
                     100.refined,
                     100.refined,
                     TimeSpan.Zero,
                     false,
                     false
        )
    }

    // Define the PoorWeather case class implementing GeminiProposalType
    case class PoorWeather(
      scienceSubtype: ScienceSubtype
    ) extends GeminiProposalType derives Eq

    object PoorWeather {
      val Default: PoorWeather = PoorWeather(ScienceSubtype.PoorWeather)
    }

    // Define the Queue case class implementing GeminiProposalType
    case class Queue(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent,
      partnerSplits:        List[PartnerSplit],
      exchangePartner:      Option[ExchangePartner],
      aeonMultiFacility:    Boolean,
      jwstSynergy:          Boolean,
      usLongTerm:           Boolean,
      considerForBand3:     ConsiderForBand3
    ) extends GeminiProposalType derives Eq

    object Queue {
      val minPercentTime: Lens[Queue, IntPercent]                 = Focus[Queue](_.minPercentTime)
      val tooActivationCeiling: Lens[Queue, TooActivationCeiling] =
        Focus[Queue](_.tooActivationCeiling)
      val exchangePartner: Lens[Queue, Option[ExchangePartner]]   = Focus[Queue](_.exchangePartner)
      val aeonMultiFacility: Lens[Queue, Boolean]                 = Focus[Queue](_.aeonMultiFacility)
      val jwstSynergy: Lens[Queue, Boolean]                       = Focus[Queue](_.jwstSynergy)
      val usLongTerm: Lens[Queue, Boolean]                        = Focus[Queue](_.usLongTerm)
      val considerForBand3: Lens[Queue, ConsiderForBand3]         = Focus[Queue](_.considerForBand3)

      val Default: Queue =
        Queue(ScienceSubtype.Queue,
              TooActivationCeiling.Default,
              100.refined,
              List.empty,
              none,
              false,
              false,
              false,
              ConsiderForBand3.Unset
        )
    }

    // Define the SystemVerification case class implementing GeminiProposalType
    case class SystemVerification(
      scienceSubtype:       ScienceSubtype,
      tooActivationCeiling: TooActivationCeiling,
      minPercentTime:       IntPercent
    ) extends GeminiProposalType

    object SystemVerification {
      val minPercentTime: Lens[SystemVerification, IntPercent]                 =
        Focus[SystemVerification](_.minPercentTime)
      val tooActivationCeiling: Lens[SystemVerification, TooActivationCeiling] =
        Focus[SystemVerification](_.tooActivationCeiling)

      val Default: SystemVerification =
        SystemVerification(ScienceSubtype.SystemVerification,
                           TooActivationCeiling.Default,
                           100.refined
        )
    }

    val classical: Prism[GeminiProposalType, Classical]                   =
      GenPrism[GeminiProposalType, Classical]
    val directorsTime: Prism[GeminiProposalType, DirectorsTime]           =
      GenPrism[GeminiProposalType, DirectorsTime]
    val demoScience: Prism[GeminiProposalType, DemoScience]               =
      GenPrism[GeminiProposalType, DemoScience]
    val fastTurnaround: Prism[GeminiProposalType, FastTurnaround]         =
      GenPrism[GeminiProposalType, FastTurnaround]
    val largeProgram: Prism[GeminiProposalType, LargeProgram]             =
      GenPrism[GeminiProposalType, LargeProgram]
    val poorWeather: Prism[GeminiProposalType, PoorWeather]               =
      GenPrism[GeminiProposalType, PoorWeather]
    val queue: Prism[GeminiProposalType, Queue]                           =
      GenPrism[GeminiProposalType, Queue]
    val systemVerification: Prism[GeminiProposalType, SystemVerification] =
      GenPrism[GeminiProposalType, SystemVerification]

    // The ODB models AEON membership as a nullable `AeonMultiFacility` object;
    // Explore only tracks membership, so presence maps to `true`.
    private def aeonMultiFacilityDecoder(c: ACursor): Decoder.Result[Boolean] =
      c.downField("aeonMultiFacility").as[Option[Json]].map(_.isDefined)

    given Decoder[GeminiProposalType] = {

      def toProposalType(tpe: ScienceSubtype, c: ACursor): Decoder.Result[GeminiProposalType] =
        tpe match
          case ScienceSubtype.Classical          =>
            for {
              minPercentTime    <- c.downField("minPercentTime").as[IntPercent]
              partnerSplits     <- c.downField("partnerSplits").as[List[PartnerSplit]]
              exchangePartner   <- c.downField("exchangePartner").as[Option[ExchangePartner]]
              aeonMultiFacility <- aeonMultiFacilityDecoder(c)
              jwstSynergy       <- c.downField("jwstSynergy").as[Boolean]
              usLongTerm        <- c.downField("usLongTerm").as[Boolean]
            } yield Classical(tpe,
                              minPercentTime,
                              partnerSplits,
                              exchangePartner,
                              aeonMultiFacility,
                              jwstSynergy,
                              usLongTerm
            )
          case ScienceSubtype.DemoScience        =>
            for {
              ceiling        <- c.as[TooActivationCeiling]
              minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            } yield DemoScience(tpe, ceiling, minPercentTime)
          case ScienceSubtype.DirectorsTime      =>
            for {
              ceiling        <- c.as[TooActivationCeiling]
              minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            } yield DirectorsTime(tpe, ceiling, minPercentTime)
          case ScienceSubtype.FastTurnaround     =>
            for {
              ceiling        <- c.as[TooActivationCeiling]
              minPercentTime <- c.downField("minPercentTime").as[IntPercent]
              reviewerId     <-
                c.downField("reviewer")
                  .downField("id")
                  .success
                  .traverse(_.as[Option[ProgramUser.Id]])
              mentorId       <-
                c.downField("mentor").downField("id").success.traverse(_.as[Option[ProgramUser.Id]])
            } yield FastTurnaround(tpe,
                                   ceiling,
                                   minPercentTime,
                                   reviewerId.flatten,
                                   mentorId.flatten
            )
          case ScienceSubtype.LargeProgram       =>
            for {
              ceiling             <- c.as[TooActivationCeiling]
              minPercentTime      <- c.downField("minPercentTime").as[IntPercent]
              minPercentTotalTime <- c.downField("minPercentTotalTime").as[IntPercent]
              totalTime           <- c.downField("totalTime").as[TimeSpan]
              aeonMultiFacility   <- aeonMultiFacilityDecoder(c)
              jwstSynergy         <- c.downField("jwstSynergy").as[Boolean]
            } yield LargeProgram(tpe,
                                 ceiling,
                                 minPercentTime,
                                 minPercentTotalTime,
                                 totalTime,
                                 aeonMultiFacility,
                                 jwstSynergy
            )
          case ScienceSubtype.PoorWeather        =>
            Right(PoorWeather(tpe))
          case ScienceSubtype.Queue              =>
            for {
              ceiling           <- c.as[TooActivationCeiling]
              minPercentTime    <- c.downField("minPercentTime").as[IntPercent]
              partnerSplits     <- c.downField("partnerSplits").as[List[PartnerSplit]]
              exchangePartner   <- c.downField("exchangePartner").as[Option[ExchangePartner]]
              aeonMultiFacility <- aeonMultiFacilityDecoder(c)
              jwstSynergy       <- c.downField("jwstSynergy").as[Boolean]
              usLongTerm        <- c.downField("usLongTerm").as[Boolean]
              considerForBand3  <- c.downField("considerForBand3").as[ConsiderForBand3]
            } yield Queue(tpe,
                          ceiling,
                          minPercentTime,
                          partnerSplits,
                          exchangePartner,
                          aeonMultiFacility,
                          jwstSynergy,
                          usLongTerm,
                          considerForBand3
            )
          case ScienceSubtype.SystemVerification =>
            for {
              ceiling        <- c.as[TooActivationCeiling]
              minPercentTime <- c.downField("minPercentTime").as[IntPercent]
            } yield SystemVerification(tpe, ceiling, minPercentTime)

      Decoder.instance { c =>
        for {
          tpe <- c.downField("scienceSubtype").as[ScienceSubtype]
          pt  <- toProposalType(tpe, c)
        } yield pt
      }
    }
