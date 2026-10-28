// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.effect.Async
import cats.effect.IO
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import lucuma.core.enums.ComaOption
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.Site
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig.M2GuideOn
import lucuma.core.model.TelescopeGuideConfig
import lucuma.horizons.HorizonsClient
import munit.CatsEffectSuite
import navigate.model.*
import navigate.model.Target.SiderealTarget
import navigate.model.config.NavigateEngineConfiguration
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.model.enums.LightSink
import navigate.server.tcs.TcsBaseControllerEpics.WfsGuideStates
import navigate.server.tcs.TcsNorthControllerSim
import navigate.server.tcs.TcsSouthControllerSim
import org.http4s.Response
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class NavigateEngineSpec extends CatsEffectSuite {

  private given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("navigate-engine")

  val guideOnCfg = TelescopeGuideConfig(
    mountGuide = MountGuideOption.MountGuideOn,
    m1Guide = M1GuideConfig.M1GuideOn(M1Source.OIWFS),
    m2Guide = M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.OIWFS)),
    dayTimeMode = Some(false),
    probeGuide = none
  )

  test("NavigateEngine must memorize requested guide configuration.") {
    withEngine(eng => eng.run(eng.enableGuide(guideOnCfg)) *> eng.assertGuide(guideOnCfg))
  }

  val targetRa  = "17:01:00.000000"
  val targetDec = "-21:10:59.999999"

  val target = SiderealTarget(
    objectName = "dummy",
    wavelength = Wavelength.fromIntPicometers(400 * 1000),
    coordinates =
      Coordinates.fromHmsDms.getOption(s"$targetRa $targetDec").getOrElse(Coordinates.Zero),
    epoch = Epoch.J2000,
    properMotion = ProperMotion(ProperMotion.μasyRA(1000), ProperMotion.μasyDec(2000)).some,
    radialVelocity = RadialVelocity.fromMetersPerSecond.getOption(BigDecimal.decimal(3000)),
    parallax = Parallax.fromMicroarcseconds(4000).some
  )

  val pwfs1Target = SiderealTarget(
    objectName = "pwfs1Dummy",
    wavelength = Wavelength.fromIntPicometers(550 * 1000),
    coordinates =
      Coordinates.fromHmsDms.getOption("17:00:58.75 -21:10:00.5").getOrElse(Coordinates.Zero),
    epoch = Epoch.J2000,
    properMotion = none,
    radialVelocity = none,
    parallax = none
  )

  val pwfs2Target = SiderealTarget(
    objectName = "pwfs2Dummy",
    wavelength = Wavelength.fromIntPicometers(800 * 1000),
    coordinates =
      Coordinates.fromHmsDms.getOption("17:01:09.999999 -21:10:01.0").getOrElse(Coordinates.Zero),
    epoch = Epoch.J2000,
    properMotion = none,
    radialVelocity = none,
    parallax = none
  )

  val oiwfsTarget = SiderealTarget(
    objectName = "oiwfsDummy",
    wavelength = Wavelength.fromIntPicometers(600 * 1000),
    coordinates = Coordinates.fromHmsDms
      .getOption("17:00:59.999999 -21:10:00.000001")
      .getOrElse(Coordinates.Zero),
    epoch = Epoch.J2000,
    properMotion = none,
    radialVelocity = none,
    parallax = none
  )

  val slewOptions = SlewOptions(
    ZeroChopThrow(true),
    ZeroSourceOffset(false),
    ZeroSourceDiffTrack(true),
    ZeroMountOffset(false),
    ZeroMountDiffTrack(true),
    ShortcircuitTargetFilter(false),
    ShortcircuitMountFilter(true),
    ResetPointing(false),
    StopGuide(true),
    ZeroGuideOffset(false),
    ZeroInstrumentOffset(true),
    AutoparkPwfs1(false),
    AutoparkPwfs2(true),
    AutoparkOiwfs(false),
    AutoparkGems(true),
    AutoparkAowfs(false)
  )

  val wfsTracking = TrackingConfig(true, false, false, true)

  val instrumentSpecifics: InstrumentSpecifics = InstrumentSpecifics(
    iaa = Angle.fromDoubleDegrees(123.45),
    focusOffset = Distance.fromLongMicrometers(2344),
    agName = "gmos",
    origin = Origin(Angle.fromMicroarcseconds(4567), Angle.fromMicroarcseconds(-8901))
  )

  test(
    "NavigateEngine must reset guide configuration after a slew command when slew option in on."
  ) {
    withEngine { eng =>
      eng.run(eng.enableGuide(guideOnCfg)) *>
        eng.run(eng.slew(slewOptions, allWfsTracking, none)) *>
        eng.assertGuide(NavigateEngine.GuideOff)
    }
  }

  test(
    "NavigateEngine must not reset guide configuration after a slew command when slew option in off."
  ) {
    withEngine { eng =>
      eng.run(eng.enableGuide(guideOnCfg)) *>
        eng.run(eng.slew(slewOptions.copy(stopGuide = StopGuide(false)), allWfsTracking, none)) *>
        eng.assertGuide(guideOnCfg)
    }
  }

  private val noTrackingStates =
    WfsGuideStates(TrackingConfig.noTracking, TrackingConfig.noTracking, TrackingConfig.noTracking)

  private def tcsConfig(
    pwfs1: Option[GuiderConfig],
    pwfs2: Option[GuiderConfig],
    oiwfs: Option[GuiderConfig]
  ): TcsConfig =
    TcsConfig(
      target,
      instrumentSpecifics,
      pwfs1,
      pwfs2,
      oiwfs,
      RotatorTrackConfig(Angle.Angle90, RotatorTrackingMode.Tracking),
      LightSink.GmosNorth,
      BafflesConfig
        .ManualConfig(CentralBafflePosition.Open, DeployableBafflePosition.Visible)
        .some,
      none
    )

  private val allWfsTracking: TcsConfig = tcsConfig(
    GuiderConfig(pwfs1Target, wfsTracking).some,
    GuiderConfig(pwfs2Target, wfsTracking).some,
    GuiderConfig(oiwfsTarget, wfsTracking).some
  )

  // getGuideDemand and getWfsGuideStates read a copy of the engine state that is only updated as
  // the engine emits events, which can happen after the command returns. Every command emits a
  // CommandEnd after all of its state changes, so waiting for it makes those changes visible.
  private class TestEngine(eng: NavigateEngine[IO], commandsEnded: SignallingRef[IO, Long]) {
    export eng.*

    def run(cmd: IO[CommandResult]): IO[Unit] =
      commandsEnded.get.flatMap(n => cmd *> commandsEnded.waitUntil(_ > n).timeout(5.seconds))

    def assertGuide(expected: TelescopeGuideConfig): IO[Unit] =
      getGuideDemand.map(r => assertEquals(r.tcsGuide, expected))

    def assertWfsStates(expected: WfsGuideStates): IO[Unit] =
      getWfsGuideStates.map(assertEquals(_, expected))
  }

  private def withEngine(f: TestEngine => IO[Unit]): IO[Unit] =
    (NavigateEngineSpec.buildEngine[IO], SignallingRef[IO].of(0L)).flatMapN { (eng, commandsEnded) =>
      eng.eventStream
        .evalMap {
          case _: NavigateEvent.CommandEnd => commandsEnded.update(_ + 1)
          case _                           => IO.unit
        }
        .compile
        .drain
        .background
        .use(_ => f(TestEngine(eng, commandsEnded)))
    }

  test("NavigateEngine must start with no tracking for all WFS.") {
    withEngine(_.assertWfsStates(noTrackingStates))
  }

  test("NavigateEngine must memorize requested PWFS1 probe tracking only.") {
    withEngine { eng =>
      eng.run(eng.pwfs1ProbeTracking(wfsTracking)) *>
        eng.assertWfsStates(noTrackingStates.copy(pwfs1 = wfsTracking))
    }
  }

  test("NavigateEngine must memorize requested PWFS2 probe tracking only.") {
    withEngine { eng =>
      eng.run(eng.pwfs2ProbeTracking(wfsTracking)) *>
        eng.assertWfsStates(noTrackingStates.copy(pwfs2 = wfsTracking))
    }
  }

  test("NavigateEngine must memorize requested OIWFS probe tracking only.") {
    withEngine { eng =>
      eng.run(eng.oiwfsProbeTracking(wfsTracking)) *>
        eng.assertWfsStates(noTrackingStates.copy(oiwfs = wfsTracking))
    }
  }

  test("NavigateEngine must memorize the last requested probe tracking.") {
    withEngine { eng =>
      eng.run(eng.pwfs1ProbeTracking(wfsTracking)) *>
        eng.assertWfsStates(noTrackingStates.copy(pwfs1 = wfsTracking)) *>
        eng.run(eng.pwfs1ProbeTracking(TrackingConfig.noTracking)) *>
        eng.assertWfsStates(noTrackingStates)
    }
  }

  test("NavigateEngine must memorize the WFS tracking requested in a TCS config.") {
    withEngine { eng =>
      eng.run(eng.tcsConfig(allWfsTracking)) *> eng.assertWfsStates(
        WfsGuideStates(wfsTracking, wfsTracking, wfsTracking)
      )
    }
  }

  test("NavigateEngine must reset to no tracking the WFS not included in a TCS config.") {
    withEngine { eng =>
      eng.run(eng.pwfs2ProbeTracking(wfsTracking)) *>
        eng.assertWfsStates(noTrackingStates.copy(pwfs2 = wfsTracking)) *>
        eng.run(
          eng.tcsConfig(
            tcsConfig(GuiderConfig(pwfs1Target, wfsTracking).some,
                      none,
                      GuiderConfig(oiwfsTarget, TrackingConfig.noTracking).some
            )
          )
        ) *>
        eng.assertWfsStates(noTrackingStates.copy(pwfs1 = wfsTracking))
    }
  }

  test("NavigateEngine must memorize the WFS tracking requested in a slew.") {
    withEngine { eng =>
      eng.run(
        eng.slew(
          slewOptions,
          tcsConfig(GuiderConfig(pwfs1Target, wfsTracking).some,
                    none,
                    GuiderConfig(oiwfsTarget, wfsTracking).some
          ),
          none
        )
      ) *> eng.assertWfsStates(WfsGuideStates(wfsTracking, TrackingConfig.noTracking, wfsTracking))
    }
  }

}

object NavigateEngineSpec {

  private def buildClient[F[_]: MonadCancelThrow](body: String): Client[F] = Client.apply[F] { _ =>
    Resource.liftK(Applicative[F].pure(Response[F](body = Stream.emits(body.getBytes("UTF-8")))))
  }

  def buildEngine[F[_]: {Async, Logger}]: F[NavigateEngine[F]] = {
    val client = buildClient("dummy")
    for {
      tcsNorth <- TcsNorthControllerSim.build[F]
      tcsSouth <- TcsSouthControllerSim.build[F]
      ret      <- NavigateEngine.build[F](
                    Site.GS,
                    Services(
                      OdbProxy.dummy,
                      client,
                      HorizonsClient(client),
                      tcsSouth,
                      tcsSouth,
                      tcsNorth
                    ),
                    NavigateEngineConfiguration.default
                  )
    } yield ret
  }

}
