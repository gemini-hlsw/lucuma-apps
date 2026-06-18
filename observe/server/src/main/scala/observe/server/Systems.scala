// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Monad
import cats.effect.Async
import cats.effect.IO
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.kernel.Ref
import cats.effect.std.SecureRandom
import cats.syntax.all.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.http4s.Http4sWebSocketBackend
import clue.http4s.Http4sWebSocketClient
import clue.websocket.ReconnectionStrategy
import edu.gemini.epics.acm.CaService
import giapi.client.ghost.GhostClient
import giapi.client.igrins2.Igrins2Client
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.schemas.ObservationDB
import mouse.boolean.*
import observe.model.CurrentConditions
import observe.model.SystemOverrides
import observe.model.config.*
import observe.model.enums.ControlStrategy
import observe.model.odb.ObsRecordedIds
import observe.server.altair.*
import observe.server.flamingos2.*
import observe.server.gcal.*
import observe.server.gems.*
import observe.server.ghost.*
import observe.server.gmos.*
import observe.server.gmos.GmosController.GmosSite
import observe.server.gnirs.*
import observe.server.gsaoi.*
import observe.server.gws.*
import observe.server.igrins2.Igrins2Controller
import observe.server.igrins2.Igrins2ControllerDisabled
import observe.server.igrins2.Igrins2ControllerSim
import observe.server.keywords.*
import observe.server.odb.DummyOdbCommands
import observe.server.odb.DummyOdbProxy
import observe.server.odb.OdbCommandsImpl
import observe.server.odb.OdbProxy
import observe.server.tcs.*
import org.http4s.AuthScheme
import org.http4s.Credentials
import org.http4s.Headers
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

case class Systems[F[_]] private[server] (
  odb:                 OdbProxy[F],
  dhs:                 DhsClientProvider[F],
  tcsSouth:            TcsSouthController[F],
  tcsNorth:            TcsNorthController[F],
  gcal:                GcalController[F],
  flamingos2:          Flamingos2Controller[F],
  gmosSouth:           GmosSouthController[F],
  gmosNorth:           GmosNorthController[F],
  ghost:               GhostController[F],
  igrins2:             Igrins2Controller[F],
  gnirs:               GnirsController[F],
  //  gpi:                 GpiController[F],
  //  niri:                NiriController[F],
  //  nifs:                NifsController[F],
  altair:              AltairController[F],
  gems:                GemsController[F],
  guideDb:             GuideConfigDb[F],
  tcsKeywordReader:    TcsKeywordsReader[F],
  conditionSetReader:  CurrentConditions => ConditionSetReader[F],
  gcalKeywordReader:   GcalKeywordReader[F],
  gmosKeywordReader:   GmosKeywordReader[F],
  gnirsKeywordReader:  GnirsKeywordReader[F],
  /*  niriKeywordReader:   NiriKeywordReader[F],
                                           nifsKeywordReader:   NifsKeywordReader[F],*/
  altairKeywordReader: AltairKeywordReader[F],
  gemsKeywordsReader:  GemsKeywordReader[F],
  gwsKeywordReader:    GwsKeywordReader[F]
)

object Systems {

  case class Builder(
    settings:     ObserveEngineConfiguration,
    sso:          LucumaSSOConfiguration,
    service:      CaService,
    tops:         Map[String, String],
    instanceName: String
  )(using L: Logger[IO], T: Temporal[IO]) {
    val reconnectionStrategy: ReconnectionStrategy =
      (attempt, reason) =>
        // Web Socket close codes: https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
        if (reason.toOption.flatMap(_.toOption.flatMap(_.code)).exists(_ === 1000))
          none
        else // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
          FiniteDuration(
            math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
            TimeUnit.SECONDS
          ).some

    private val WsReconnectStrategy: ReconnectionStrategy =
      (attempt, _) =>
        // Increase the delay to get exponential backoff with a minimum of 1s and a max of 30s
        // TODO If it's a Not authorized, do not backoff, retry on constant period.
        FiniteDuration(
          math.min(30.0, math.pow(2, attempt.toDouble - 1)).toLong,
          TimeUnit.SECONDS
        ).some

    private val authHeader = Authorization(Credentials.Token(AuthScheme.Bearer, sso.serviceToken))

    def odbProxy[F[_]: {Async, Logger, Http4sHttpBackend, SecureRandom}]: F[OdbProxy[F]] =
      for
        fetchClient                    <- // Http client used ONLY for recording events.
          Http4sHttpClient.of[F, ObservationDB](settings.odbHttp, "ODB", Headers(authHeader))
        wsClient                       <- JdkWSClient.simple[F].allocated.map(_._1)
        given Http4sWebSocketBackend[F] = Http4sWebSocketBackend[F](wsClient)
        innerClient                    <-
          Http4sWebSocketClient.of[F, ObservationDB](settings.odbWs, "ODB-WS", WsReconnectStrategy)
        _                              <-
          innerClient.connect:
            Map(Authorization.name.toString -> authHeader.credentials.renderString.asJson).pure[F]
        odbCommands                    <-
          if (settings.odbNotifications)
            Ref
              .of[F, ObsRecordedIds](ObsRecordedIds.Empty)
              .map(OdbCommandsImpl[F](_)(using fetchClient))
          else
            DummyOdbCommands[F].pure[F]
      yield OdbProxy[F](odbCommands)(using innerClient)

    def dhs[F[_]: {Async, Logger}](site: Site, httpClient: Client[F]): F[DhsClientProvider[F]] =
      if (settings.systemControl.dhs.command)
        new DhsClientProvider[F] {
          override def dhsClient(instrumentName: String): DhsClient[F] = new DhsClientHttp[F](
            httpClient,
            settings.dhsServer,
            settings.dhsMaxSize,
            instrumentName
          )
        }.pure[F]
      else
        DhsClientSim
          .apply[F](site)
          .map(client =>
            new DhsClientProvider[F] {
              override def dhsClient(instrumentName: String): DhsClient[F] =
                client
            }
          )

    // TODO make instruments controllers generalized on F
    def gcal: IO[(GcalController[IO], GcalKeywordReader[IO])] =
      if (settings.systemControl.gcal.realKeywords)
        GcalEpics
          .instance[IO](service, tops)
          .map(epicsSys =>
            (
              if (settings.systemControl.gcal.command) GcalControllerEpics(epicsSys)
              else GcalControllerSim[IO],
              GcalKeywordsReaderEpics(epicsSys)
            )
          )
      else (GcalControllerSim[IO], DummyGcalKeywordsReader[IO]).pure[IO]

    def tcsSouth(
      tcsEpicsO: => Option[TcsEpics[IO]],
      site:      Site,
      gcdb:      GuideConfigDb[IO]
    ): TcsSouthController[IO] =
      tcsEpicsO
        .map { tcsEpics =>
          if (settings.systemControl.tcs.command && site === Site.GS)
            TcsSouthControllerEpics(tcsEpics, gcdb)
          else TcsSouthControllerSim[IO]
        }
        .getOrElse(TcsSouthControllerSim[IO])

    def tcsNorth(tcsEpicsO: => Option[TcsEpics[IO]], site: Site): TcsNorthController[IO] =
      tcsEpicsO
        .map { tcsEpics =>
          if (settings.systemControl.tcs.command && site === Site.GN)
            TcsNorthControllerEpics(tcsEpics)
          else TcsNorthControllerSim[IO]
        }
        .getOrElse(TcsNorthControllerSim[IO])

    def altair(
      tcsEpicsO: => Option[TcsEpics[IO]]
    ): IO[(AltairController[IO], AltairKeywordReader[IO])] =
      if (settings.systemControl.altair.realKeywords)
        AltairEpics.instance[IO](service, tops).map { altairEpics =>
          tcsEpicsO
            .map { tcsEpics =>
              if (settings.systemControl.altair.command && settings.systemControl.tcs.command)
                AltairControllerEpics.apply(altairEpics, tcsEpics)
              else
                AltairControllerSim[IO]
            }
            .map((_, AltairKeywordReaderEpics(altairEpics)))
            .getOrElse((AltairControllerSim[IO], AltairKeywordReaderEpics(altairEpics)))
        }
      else
        (AltairControllerSim[IO], AltairKeywordReaderDummy[IO]).pure[IO]

    def tcsObjects(gcdb: GuideConfigDb[IO], site: Site): IO[
      (
        TcsNorthController[IO],
        TcsSouthController[IO],
        TcsKeywordsReader[IO],
        AltairController[IO],
        AltairKeywordReader[IO],
        CurrentConditions => ConditionSetReader[IO]
      )
    ] =
      for {
        tcsEpicsO            <- settings.systemControl.tcs.realKeywords
                                  .option(TcsEpics.instance[IO](service, tops))
                                  .sequence
        a                    <- altair(tcsEpicsO)
        (altairCtr, altairKR) = a
        tcsNCtr               = tcsNorth(tcsEpicsO, site)
        tcsSCtr               = tcsSouth(tcsEpicsO, site, gcdb)
        tcsKR                 = tcsEpicsO.map(TcsKeywordsReaderEpics[IO]).getOrElse(DummyTcsKeywordsReader[IO])
        condsR                = tcsEpicsO
                                  .map(ConditionSetReaderEpics.apply(site, _))
                                  .getOrElse(DummyConditionSetReader(site))
      } yield (
        tcsNCtr,
        tcsSCtr,
        tcsKR,
        altairCtr,
        altairKR,
        condsR
      )

    def gems(
      gsaoiController: GsaoiGuider[IO],
      gsaoiEpicsO:     => Option[GsaoiEpics[IO]]
    ): IO[(GemsController[IO], GemsKeywordReader[IO])] =
      if (settings.systemControl.gems.realKeywords)
        GemsEpics.instance[IO](service, tops).map { gemsEpics =>
          gsaoiEpicsO
            .map { gsaoiEpics =>
              (
                if (settings.systemControl.gems.command && settings.systemControl.tcs.command)
                  GemsControllerEpics(gemsEpics, gsaoiController)
                else
                  GemsControllerSim[IO],
                GemsKeywordReaderEpics[IO](gemsEpics, gsaoiEpics)
              )
            }
            .getOrElse(
              (GemsControllerEpics(gemsEpics, gsaoiController), GemsKeywordReaderDummy[IO])
            )
        }
      else (GemsControllerSim[IO], GemsKeywordReaderDummy[IO]).pure[IO]

    def gsaoi(
      gsaoiEpicsO: => Option[GsaoiEpics[IO]]
    ): IO[(GsaoiFullHandler[IO], GsaoiKeywordReader[IO])] =
      gsaoiEpicsO
        .map { gsaoiEpics =>
          /*if (settings.systemControl.gsaoi.command) GsaoiControllerEpics(gsaoiEpics).pure[IO]
            else*/
          GsaoiControllerSim[IO]
            .map((_, GsaoiKeywordReaderEpics(gsaoiEpics)))
        }
        .getOrElse(GsaoiControllerSim[IO].map((_, GsaoiKeywordReaderDummy[IO])))

    def gemsObjects: IO[
      (GemsController[IO], GemsKeywordReader[IO], GsaoiController[IO], GsaoiKeywordReader[IO])
    ] =
      for {
        gsaoiEpicsO        <- settings.systemControl.gsaoi.realKeywords
                                .option(GsaoiEpics.instance[IO](service, tops))
                                .sequence
        a                  <- gsaoi(gsaoiEpicsO)
        (gsaoiCtr, gsaoiKR) = a
        b                  <- gems(gsaoiCtr, gsaoiEpicsO)
        (gemsCtr, gemsKR)   = b
      } yield (gemsCtr, gemsKR, gsaoiCtr, gsaoiKR)

    /*
     * Type parameters are
     * E: Instrument EPICS class
     * C: Instrument controller class
     * K: Instrument keyword reader class
     */
    def instObjects[F[_]: Monad, E, C, K](
      ctrl:                 ControlStrategy,
      epicsBuilder:         (CaService, Map[String, String]) => F[E],
      realCtrlBuilder:      (=> E) => C,
      simCtrlBuilder:       => F[C],
      realKeyReaderBuilder: E => K,
      simKeyReaderBuilder:  => K
    ): F[(C, K)] =
      if (ctrl.realKeywords)
        epicsBuilder(service, tops).flatMap(epicsSys =>
          (
            if (ctrl.command) realCtrlBuilder(epicsSys).pure[F]
            else simCtrlBuilder
          ).map((_, realKeyReaderBuilder(epicsSys)))
        )
      else
        simCtrlBuilder.map((_, simKeyReaderBuilder))

    def gnirs: IO[(GnirsController[IO], GnirsKeywordReader[IO])] =
      instObjects(
        settings.systemControl.gnirs,
        GnirsEpics.instance[IO],
        GnirsControllerEpics.apply[IO],
        GnirsControllerSim.apply[IO],
        GnirsKeywordReaderEpics[IO],
        GnirsKeywordReaderDummy[IO]
      )
    //
    //    def niri: IO[(NiriController[IO], NiriKeywordReader[IO])] =
    //      instObjects(
    //        settings.systemControl.niri,
    //        NiriEpics.instance[IO],
    //        NiriControllerEpics.apply[IO],
    //        NiriControllerSim.apply[IO],
    //        NiriKeywordReaderEpics[IO],
    //        NiriKeywordReaderDummy[IO]
    //      )
    //
    //    def nifs: IO[(NifsController[IO], NifsKeywordReader[IO])] =
    //      instObjects(
    //        settings.systemControl.nifs,
    //        NifsEpics.instance[IO],
    //        NifsControllerEpics.apply[IO],
    //        NifsControllerSim.apply[IO],
    //        NifsKeywordReaderEpics[IO],
    //        NifsKeywordReaderDummy[IO]
    //      )

    def gmosSouth(gmosEpicsO: Option[GmosEpics[IO]], site: Site): IO[GmosSouthController[IO]] =
      gmosEpicsO
        .filter(_ => settings.systemControl.gmos.command && site === Site.GS)
        .map(GmosSouthControllerEpics.apply[IO](_).pure[IO])
        .getOrElse(GmosControllerSim.south[IO])

    def gmosNorth(gmosEpicsO: Option[GmosEpics[IO]], site: Site): IO[GmosNorthController[IO]] =
      gmosEpicsO
        .filter(_ => settings.systemControl.gmos.command && site === Site.GN)
        .map(GmosNorthControllerEpics.apply[IO](_).pure[IO])
        .getOrElse(GmosControllerSim.north[IO])

    def gmosObjects(
      site: Site
    ): IO[(GmosSouthController[IO], GmosNorthController[IO], GmosKeywordReader[IO])] =
      for {
        gmosEpicsO   <- settings.systemControl.gmos.realKeywords
                          .option(GmosEpics.instance[IO](service, tops))
                          .sequence
        gmosSouthCtr <- gmosSouth(gmosEpicsO, site)
        gmosNorthCtr <- gmosNorth(gmosEpicsO, site)
        gmosKR        = gmosEpicsO.map(GmosKeywordReaderEpics[IO]).getOrElse(GmosKeywordReaderDummy[IO])
      } yield (gmosSouthCtr, gmosNorthCtr, gmosKR)

    def flamingos2: IO[Flamingos2Controller[IO]] =
      if (settings.systemControl.flamingos2.command)
        Flamingos2Epics.instance[IO](service, tops).map(Flamingos2ControllerEpics(_))
      else if (settings.instForceError) Flamingos2ControllerSimBad[IO](settings.failAt)
      else Flamingos2ControllerSim[IO]

    def ghost[F[_]: {Async, Logger}](
      httpClient:   Client[F],
      instanceName: String
    ): Resource[F, GhostController[F]] =
      if (settings.systemControl.ghost.command) {
        def ghostClient: Resource[F, GhostClient[F]] =
          GhostClient
            .ghostClient[F](s"ghost-observe-$instanceName", settings.ghostUrl.value.renderString)

        def ghostGDS: Resource[F, GdsClient[F]] =
          Resource.pure[F, GdsClient[F]](
            GdsClient.xmlrpc(if (settings.systemControl.ghost.command) httpClient
                             else GdsClient.xmlrpc.alwaysOkClient[F],
                             settings.ghostGds.value
            )
          )

        (ghostClient, ghostGDS).mapN(GhostController(_, _))
      } else
        Resource.eval(GhostControllerSim[F])

    def igrins2[F[_]: {Async, Logger}](
      httpClient:   Client[F],
      instanceName: String
    ): Resource[F, Igrins2Controller[F]] =
      if (settings.systemControl.igrins2.command) {
        def igrins2Client: Resource[F, Igrins2Client[F]] =
          Igrins2Client.igrins2Client[F](s"igrins2-observe-$instanceName",
                                         settings.igrins2Url.value.renderString
          )

        def igrins2GDS: Resource[F, GdsClient[F]] =
          Resource.pure[F, GdsClient[F]](
            GdsClient.json(if (settings.systemControl.igrins2.command) httpClient
                           else GdsClient.json.alwaysOkClient[F],
                           settings.igrins2Gds.value
            )
          )

        (igrins2Client, igrins2GDS).mapN(Igrins2Controller(_, _))
      } else
        Resource.eval(Igrins2ControllerSim[F])

    def gws: IO[GwsKeywordReader[IO]] =
      if (settings.systemControl.gws.realKeywords)
        GwsEpics.instance[IO](service, tops).map(GwsKeywordsReaderEpics[IO])
      else GwsKeywordsReaderDummy[IO].pure[IO]

    def build(site: Site, httpClient: Client[IO]): Resource[IO, Systems[IO]] =
      given Http4sHttpBackend[IO] = Http4sHttpBackend(httpClient)
      for {
        odbProxy                                          <- Resource.eval[IO, OdbProxy[IO]](odbProxy[IO])
        dhsClient                                         <- Resource.eval(dhs[IO](site, httpClient))
        gcdb                                              <- Resource.eval(GuideConfigDb.newDb[IO])
        gcals                                             <- Resource.eval(gcal)
        (gcalCtr, gcalKR)                                  = gcals
        v                                                 <- Resource.eval(tcsObjects(gcdb, site))
        (tcsGN, tcsGS, tcsKR, altairCtr, altairKR, condsR) = v
        w                                                 <- Resource.eval(gemsObjects)
        (gemsCtr, gemsKR, gsaoiCtr, gsaoiKR)               = w
        gnirsObjs                                         <- Resource.eval(gnirs)
        (gnirsCtr, gnirsKR)                                = gnirsObjs
        f2Controller                                      <- Resource.eval(flamingos2)
        igrins2Ctr                                        <- igrins2(httpClient, instanceName)
        //        (niriCtr, niriKR)                          <- Resource.eval(niri)
        //        (nifsCtr, nifsKR)                          <- Resource.eval(nifs)
        gms                                               <- Resource.eval(gmosObjects(site))
        (gmosSouthCtr, gmosNorthCtr, gmosKR)               = gms
        //        gpiController                              <- gpi[IO](httpClient, instanceName)
        ghostController                                   <- ghost[IO](httpClient, instanceName)
        gwsKR                                             <- Resource.eval(gws)
      } yield Systems[IO](
        odbProxy,
        dhsClient,
        tcsGS,
        tcsGN,
        gcalCtr,
        f2Controller,
        gmosSouthCtr,
        gmosNorthCtr,
        //        gsaoiCtr,
        //        gpiController,
        ghostController,
        //        niriCtr,
        //        nifsCtr,
        igrins2Ctr,
        gnirsCtr,
        altairCtr,
        gemsCtr,
        gcdb,
        tcsKR,
        condsR,
        gcalKR,
        gmosKR,
        gnirsKR,
        //        niriKR,
        //        nifsKR,
        //        gsaoiKR,
        altairKR,
        gemsKR,
        gwsKR
      )
  }

  private def decodeTops(s: String): Map[String, String] =
    s.split("[=,]")
      .grouped(2)
      .collect { case Array(k, v) =>
        k.trim -> v.trim
      }
      .toMap

  def build(
    site:         Site,
    httpClient:   Client[IO],
    settings:     ObserveEngineConfiguration,
    sso:          LucumaSSOConfiguration,
    service:      CaService,
    instanceName: String
  )(using T: Temporal[IO], L: Logger[IO]): Resource[IO, Systems[IO]] =
    Builder(settings, sso, service, decodeTops(settings.tops), instanceName).build(site, httpClient)

  def dummy[F[_]: {Async, Logger}]: F[Systems[F]] =
    GuideConfigDb
      .newDb[F]
      .map(guideDb =>
        new Systems(
          new DummyOdbProxy[F],
          DhsClientProvider.dummy[F],
          TcsSouthControllerSim[F],
          TcsNorthControllerSim[F],
          GcalControllerSim[F],
          Flamingos2ControllerDisabled[F],
          GmosControllerDisabled[F, GmosController.GmosSite.South.type]("south"),
          GmosControllerDisabled[F, GmosController.GmosSite.North.type]("north"),
          GhostControllerDisabled[F],
          Igrins2ControllerDisabled[F],
          GnirsControllerDisabled[F],
          AltairControllerSim[F],
          GemsControllerSim[F],
          guideDb,
          DummyTcsKeywordsReader[F],
          DummyConditionSetReader.apply[F](Site.GN),
          DummyGcalKeywordsReader[F],
          GmosKeywordReaderDummy[F],
          GnirsKeywordReaderDummy[F],
          AltairKeywordReaderDummy[F],
          GemsKeywordReaderDummy[F],
          GwsKeywordsReaderDummy[F]
        )
      )

  class OverriddenSystems[F[_]: {Async, Logger}](val systems: Systems[F]) {

    private val tcsSouthDisabled: TcsSouthController[F]     = new TcsSouthControllerDisabled[F]
    private val tcsNorthDisabled: TcsNorthController[F]     = new TcsNorthControllerDisabled[F]
    private val gemsDisabled: GemsController[F]             = new GemsControllerDisabled[F]
    private val altairDisabled: AltairController[F]         = new AltairControllerDisabled[F]
    private val dhsDisabled: DhsClientProvider[F]           = (_: String) => new DhsClientDisabled[F]
    private val gcalDisabled: GcalController[F]             = new GcalControllerDisabled[F]
    private val flamingos2Disabled: Flamingos2Controller[F] = new Flamingos2ControllerDisabled[F]
    private val igrins2Disabled: Igrins2Controller[F]       = new Igrins2ControllerDisabled[F]
    private val gmosSouthDisabled: GmosSouthController[F]   =
      new GmosControllerDisabled[F, GmosSite.South.type]("GMOS-S")
    private val gmosNorthDisabled: GmosNorthController[F]   =
      new GmosControllerDisabled[F, GmosSite.North.type]("GMOS-N")
    //    private val gsaoiDisabled: GsaoiController[F]           = new GsaoiControllerDisabled[F]
    //    private val gpiDisabled: GpiController[F]               = new GpiControllerDisabled[F](systems.gpi.statusDb)
    private val ghostDisabled: GhostController[F]           = new GhostControllerDisabled[F]
    //    private val nifsDisabled: NifsController[F]             = new NifsControllerDisabled[F]
    //    private val niriDisabled: NiriController[F]             = new NiriControllerDisabled[F]
    private val gnirsDisabled: GnirsController[F]           = new GnirsControllerDisabled[F]

    def tcsSouth(overrides: SystemOverrides): TcsSouthController[F] =
      if (overrides.isTcsEnabled.value) systems.tcsSouth
      else tcsSouthDisabled

    def tcsNorth(overrides: SystemOverrides): TcsNorthController[F] =
      if (overrides.isTcsEnabled.value) systems.tcsNorth
      else tcsNorthDisabled

    def gems(overrides: SystemOverrides): GemsController[F] =
      if (overrides.isTcsEnabled.value) systems.gems
      else gemsDisabled

    def altair(overrides: SystemOverrides): AltairController[F] =
      if (overrides.isTcsEnabled.value) systems.altair
      else altairDisabled

    def dhs(overrides: SystemOverrides): DhsClientProvider[F] =
      if (overrides.isDhsEnabled.value) systems.dhs
      else dhsDisabled

    def gcal(overrides: SystemOverrides): GcalController[F] =
      if (overrides.isGcalEnabled.value) systems.gcal
      else gcalDisabled

    def flamingos2(overrides: SystemOverrides): Flamingos2Controller[F] =
      if (overrides.isInstrumentEnabled) systems.flamingos2
      else flamingos2Disabled

    def gmosNorth(overrides: SystemOverrides): GmosNorthController[F] =
      if (overrides.isInstrumentEnabled.value) systems.gmosNorth
      else gmosNorthDisabled

    def gmosSouth(overrides: SystemOverrides): GmosSouthController[F] =
      if (overrides.isInstrumentEnabled.value) systems.gmosSouth
      else gmosSouthDisabled

    def igrins2(overrides: SystemOverrides): Igrins2Controller[F] =
      if (overrides.isInstrumentEnabled.value) systems.igrins2
      else igrins2Disabled

    //    def gsaoi(overrides: SystemOverrides): GsaoiController[F] =
    //      if (overrides.isInstrumentEnabled) systems.gsaoi
    //      else gsaoiDisabled
    //
    //    def gpi(overrides: SystemOverrides): GpiController[F] =
    //      if (overrides.isInstrumentEnabled) systems.gpi
    //      else gpiDisabled
    //
    def ghost(overrides: SystemOverrides): GhostController[F] =
      if (overrides.isInstrumentEnabled) systems.ghost
      else ghostDisabled
    //
    //    def nifs(overrides: SystemOverrides): NifsController[F] =
    //      if (overrides.isInstrumentEnabled) systems.nifs
    //      else nifsDisabled
    //
    //    def niri(overrides: SystemOverrides): NiriController[F] =
    //      if (overrides.isInstrumentEnabled) systems.niri
    //      else niriDisabled
    //
    def gnirs(overrides: SystemOverrides): GnirsController[F] =
      if (overrides.isInstrumentEnabled.value) systems.gnirs
      else gnirsDisabled

  }
}
