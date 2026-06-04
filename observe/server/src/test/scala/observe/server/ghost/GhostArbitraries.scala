// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import lucuma.core.arb.ArbTime
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.model.Target as GemTarget
import lucuma.core.model.arb.ArbTarget
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.arb.ArbStepConfig.given
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.arb.ArbGhostDetector.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait GhostArbitraries extends ArbTime {

  import ArbTarget.given

  given Arbitrary[IFUTargetType.SiderealTarget] = Arbitrary {
    arbitrary[String].map(IFUTargetType.SiderealTarget.apply)
  }

  given Cogen[IFUTargetType.SiderealTarget] = Cogen[String].contramap(_.name)

  given Arbitrary[IFUTargetType.NonsiderealTarget] = Arbitrary {
    arbitrary[String].map(IFUTargetType.NonsiderealTarget.apply)
  }

  given Cogen[IFUTargetType.NonsiderealTarget] = Cogen[String].contramap(_.name)

  val ghostSRSingleTargetConfigGen: Gen[StandardResolutionMode.SingleTarget] = for {
    obsType         <- arbitrary[StepConfig]
    obsClass        <- arbitrary[ObserveClass]
    blueConfig      <- arbitrary[GhostDetector]
    redConfig       <- arbitrary[GhostDetector]
    basePos         <- arbitrary[Option[Coordinates]]
    fa1             <- arbitrary[GhostIfu1FiberAgitator]
    fa2             <- arbitrary[GhostIfu2FiberAgitator]
    ifu1TargetType  <- arbitrary[IFUTargetType.SiderealTarget]
    ifu1Coordinates <- arbitrary[Coordinates]
    targets         <- arbitrary[List[GemTarget]]
    mag             <- arbitrary[Option[Double]]
    ag              <- arbitrary[Option[TimeSpan]]
    sv              <- arbitrary[Option[TimeSpan]]
  } yield StandardResolutionMode.SingleTarget(
    obsType,
    obsClass,
    GhostDetector.Blue(blueConfig),
    GhostDetector.Red(redConfig),
    basePos,
    fa1,
    fa2,
    ifu1TargetType,
    ifu1Coordinates,
    targets,
    mag,
    ag,
    sv
  )

  given ghostSRSingleTargetConfigCogen: Cogen[StandardResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], IFUTargetType.SiderealTarget, Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1TargetType, x.ifu1Coordinates))

  val ghostSRDualTargetConfigGen: Gen[StandardResolutionMode.DualTarget] =
    for {
      obsType         <- arbitrary[StepConfig]
      obsClass        <- arbitrary[ObserveClass]
      blueConfig      <- arbitrary[GhostDetector]
      redConfig       <- arbitrary[GhostDetector]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[GhostIfu1FiberAgitator]
      fa2             <- arbitrary[GhostIfu2FiberAgitator]
      ifu1TargetType  <- arbitrary[IFUTargetType.SiderealTarget]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetType  <- arbitrary[IFUTargetType.SiderealTarget]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      mag             <- arbitrary[Option[Double]]
      ag              <- arbitrary[Option[TimeSpan]]
      sv              <- arbitrary[Option[TimeSpan]]
    } yield StandardResolutionMode.DualTarget(
      obsType,
      obsClass,
      GhostDetector.Blue(blueConfig),
      GhostDetector.Red(redConfig),
      basePos,
      fa1,
      fa2,
      ifu1TargetType,
      ifu1Coordinates,
      ifu2TargetType,
      ifu2Coordinates,
      targets,
      mag,
      ag,
      sv
    )

  given Cogen[StandardResolutionMode.DualTarget] =
    Cogen[
      (Option[Coordinates],
       IFUTargetType.SiderealTarget,
       Option[Coordinates],
       IFUTargetType.SiderealTarget,
       Option[Coordinates]
      )
    ]
      .contramap(x =>
        (x.baseCoords, x.ifu1TargetType, x.ifu1Coordinates, x.ifu2TargetType, x.ifu2Coordinates)
      )
  /*
  val ghostSRTargetSkyConfigGen: Gen[StandardResolutionMode.TargetPlusSky] =
    for {
      obsType         <- arbitrary[StepConfig]
      obsClass        <- arbitrary[ObserveClass]
      blueConfig      <- arbitrary[GhostDetector]
      redConfig       <- arbitrary[GhostDetector]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[GhostIfu1FiberAgitator]
      fa2             <- arbitrary[GhostIfu2FiberAgitator]
      ifu1TargetType  <- arbitrary[IFUTargetType.SiderealTarget]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      mag             <- arbitrary[Option[Double]]
      ag              <- arbitrary[Option[FiniteDuration]]
      sv              <- arbitrary[Option[FiniteDuration]]
    } yield StandardResolutionMode.TargetPlusSky(
      obsType,
      obsClass,
      GhostDetector.Blue(blueConfig),
      GhostDetector.Red(redConfig),
      basePos,
      fa1,
      fa2,
      ifu1TargetType,
      ifu1Coordinates,
      ifu2Coordinates,
      targets,
      rm,
      mag,
      ag,
      sv
    )

  given Cogen[StandardResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], IFUTargetType.SiderealTarget, Option[Coordinates], Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1TargetType, x.ifu1Coordinates, x.ifu2Coordinates))

  given Gen[StandardResolutionMode.SkyPlusTarget] =
    for {
      obsType         <- arbitrary[String]
      obsClass        <- arbitrary[String]
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[GhostIfu1FiberAgitator]
      fa2             <- arbitrary[GhostIfu2FiberAgitator]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetName  <- arbitrary[String]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      conditions      <- arbitrary[CurrentConditions]
      mag             <- arbitrary[Option[Double]]
      ag              <- arbitrary[Option[FiniteDuration]]
      sv              <- arbitrary[Option[FiniteDuration]]
    } yield StandardResolutionMode.SkyPlusTarget(
      obsType,
      obsClass,
      GhostDetector.Blue(blueConfig),
      GhostDetector.Red(redConfig),
      basePos,
      fa1,
      fa2,
      ifu1Coordinates,
      ifu2TargetName,
      ifu2Coordinates,
      targets,
      mag,
      ag,
      sv
    )

  given Cogen[StandardResolutionMode.SkyPlusTarget] =
    Cogen[(Option[Coordinates], Option[Coordinates], IFUTargetType.SiderealTarget, Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1Coordinates, x.ifu2TargetType, x.ifu2Coordinates))

  given Gen[HighResolutionMode.TargetPlusSky] =
    for {
      obsType    <- arbitrary[String]
      obsClass   <- arbitrary[String]
      blueConfig <- arbitrary[ChannelConfig]
      redConfig  <- arbitrary[ChannelConfig]
      basePos    <- arbitrary[Option[Coordinates]]
      hrifu1Name <- arbitrary[String]
      fa1             <- arbitrary[GhostIfu1FiberAgitator]
      fa2             <- arbitrary[GhostIfu2FiberAgitator]
      hrifu1Pos  <- arbitrary[Coordinates]
      hrifu2Pos  <- arbitrary[Coordinates]
      targets    <- arbitrary[List[GemTarget]]
      mag        <- arbitrary[Option[Double]]
      ag         <- arbitrary[Option[FiniteDuration]]
      sv         <- arbitrary[Option[FiniteDuration]]
    } yield HighResolutionMode.TargetPlusSky(
      obsType,
      obsClass,
      GhostDetector.Blue(blueConfig),
      GhostDetector.Red(redConfig),
      basePos,
      fa1,
      fa2,
      hrifu1Name,
      hrifu1Pos,
      hrifu2Pos,
      targets,
      rm,
      mag,
      ag,
      sv
    )

  given Cogen[HighResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], IFUTargetType.SiderealTarget, Option[Coordinates], Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1TargetType, x.ifu1Coordinates, x.ifu2Coordinates))

  given Arbitrary[GhostConfig] = Arbitrary {
    Gen.oneOf(
      ghostSRSingleTargetConfigGen,
      ghostSRDualTargetConfigGen,
      ghostSRTargetSkyConfigGen,
      ghostSRSkyTargetConfigGen,
      ghostHRTargetPlusSkyConfigGen
    )
  }

  object GhostHelpers {
    def extractSRIFU1Name(x: GhostConfig): Option[String] = x match {
      case x: StandardResolutionMode.SingleTarget => x.ifu1TargetType.name
      case x: StandardResolutionMode.DualTarget => x.ifu1TargetType.name
      case x: StandardResolutionMode.TargetPlusSky => x.ifu1TargetType.name
      case _: StandardResolutionMode.SkyPlusTarget                                             => Some("Sky")
      case _                                                                                   => None
    }

    def extractSRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: StandardResolutionMode => c.ifu1Coordinates
      case _                         => None
    }

    def extractSRIFU2Name(x: GhostConfig): Option[String] = x match {
      case x: StandardResolutionMode.DualTarget => x.ifu2TargetType.name
      case _: StandardResolutionMode.TargetPlusSky => Some("Sky")
      case x: StandardResolutionMode.SkyPlusTarget => x.ifu2TargetType.name
      case _                                       => None
    }

    def extractSRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case x: StandardResolutionMode.DualTarget => x.ifu2Coordinates
      case x: StandardResolutionMode.TargetPlusSky => x.ifu2Coordinates
      case StandardResolutionMode.SkyPlusTarget => x.ifu2Coordinates
      case _ => None
    }

    def extractHRIFU1Name(x: GhostConfig): Option[String] = x match {
      case c: HighResolutionMode => c.ifu1TargetType.name
      case _                     => None
    }

    def extractHRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode => c.ifu1Coordinates
      case _                     => None
    }

    def extractHRIFU2Name(x: GhostConfig): Option[String] = x match {
      case _: HighResolutionMode.TargetPlusSky => Some("Sky")
      case _                                   => None
    }

    def extractHRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode.TargetPlusSky => c.ifu2Coordinates
      case _                                   => None
    }
  }

  given Cogen[GhostConfig] = {
    import GhostHelpers._
    Cogen[
      (
        String,
        String,
        // ChannelConfig,
        // ChannelConfig,
        Option[Coordinates],
        // List[GemTarget],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates]
      )
    ].contramap(x =>
      (x.obsType,
       x.obsClass,
       // x.blueConfig,
       // x.redConfig,
       x.baseCoords,
       // x.userTargets,
       extractSRIFU1Name(x),
       extractSRIFU1Coordinates(x),
       extractSRIFU2Name(x),
       extractSRIFU2Coordinates(x),
       extractHRIFU1Name(x),
       extractHRIFU1Coordinates(x),
       extractHRIFU2Name(x),
       extractHRIFU2Coordinates(x)
      )
    )
  }
   */
}
