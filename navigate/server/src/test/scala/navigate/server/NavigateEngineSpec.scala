// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.effect.Async
import cats.effect.IO
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.ComaOption
import lucuma.core.enums.Instrument
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
import munit.CatsEffectSuite
import navigate.model.*
import navigate.model.Target.SiderealTarget
import navigate.model.config.NavigateEngineConfiguration
import navigate.server.tcs.TcsNorthControllerSim
import navigate.server.tcs.TcsSouthControllerSim
import org.http4s.Response
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
    for {
      eng <- NavigateEngineSpec.buildEngine[IO]
      _   <- Stream.eval(eng.enableGuide(guideOnCfg)).merge(eng.eventStream.take(1)).compile.drain
      r   <- eng.getGuideDemand
    } yield assertEquals(r.tcsGuide, guideOnCfg)
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
    "NavigateEngine must reset guide configuration after a slew command when slew option in on.".flaky
  ) {
    for {
      eng <- NavigateEngineSpec.buildEngine[IO]
      _   <- Stream
               .evals(
                 List(
                   eng.enableGuide(guideOnCfg),
                   eng.slew(
                     slewOptions,
                     TcsConfig(
                       target,
                       instrumentSpecifics,
                       GuiderConfig(pwfs1Target, wfsTracking).some,
                       GuiderConfig(pwfs2Target, wfsTracking).some,
                       GuiderConfig(oiwfsTarget, wfsTracking).some,
                       RotatorTrackConfig(Angle.Angle90, RotatorTrackingMode.Tracking),
                       Instrument.GmosNorth
                     ),
                     none
                   )
                 ).sequence
               )
               .merge(eng.eventStream)
               .take(4)
               .compile
               .drain
      r   <- eng.getGuideDemand
    } yield assertEquals(r.tcsGuide, NavigateEngine.GuideOff)
  }

  test(
    "NavigateEngine must not reset guide configuration after a slew command when slew option in off."
  ) {
    for {
      eng <- NavigateEngineSpec.buildEngine[IO]
      _   <- Stream
               .evals(
                 List(
                   eng.enableGuide(guideOnCfg),
                   eng.slew(
                     slewOptions.copy(stopGuide = StopGuide(false)),
                     TcsConfig(
                       target,
                       instrumentSpecifics,
                       GuiderConfig(pwfs1Target, wfsTracking).some,
                       GuiderConfig(pwfs2Target, wfsTracking).some,
                       GuiderConfig(oiwfsTarget, wfsTracking).some,
                       RotatorTrackConfig(Angle.Angle90, RotatorTrackingMode.Tracking),
                       Instrument.GmosNorth
                     ),
                     none
                   )
                 ).sequence
               )
               .merge(eng.eventStream)
               .take(4)
               .compile
               .drain
      r   <- eng.getGuideDemand
    } yield assertEquals(r.tcsGuide, guideOnCfg)
  }

}

object NavigateEngineSpec {

  private def buildClient[F[_]: MonadCancelThrow](body: String): Client[F] = Client.apply[F] { _ =>
    Resource.liftK(Applicative[F].pure(Response[F](body = Stream.emits(body.getBytes("UTF-8")))))
  }

  def buildEngine[F[_]: {Async, Logger}]: F[NavigateEngine[F]] = for {
    tcsNorth <- TcsNorthControllerSim.build[F]
    tcsSouth <- TcsSouthControllerSim.build[F]
    ret      <- NavigateEngine.build[F](
                  Site.GS,
                  Systems(
                    OdbProxy.dummy,
                    buildClient("dummy"),
                    tcsSouth,
                    tcsSouth,
                    tcsNorth
                  ),
                  NavigateEngineConfiguration.default
                )
  } yield ret

}
