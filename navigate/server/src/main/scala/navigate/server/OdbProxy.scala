// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.MonadThrow
import cats.effect.kernel.Clock
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import lucuma.core.enums.Site
import lucuma.core.enums.SlewStage
import lucuma.core.model.Ephemeris
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.drain
import lucuma.schemas.odb.input.clientTimeNow
import navigate.queries.ObsQueriesGQL.ActiveNonsiderealTargetsQuery
import navigate.queries.ObsQueriesGQL.AddSlewEventMutation
import org.typelevel.log4cats.Logger

import java.time.LocalDate
import scala.language.implicitConversions

trait OdbProxy[F[_]] {
  def addSlewEvent(
    obsId: Observation.Id,
    stage: SlewStage
  ): F[Unit]
  def queryNonSiderealObs(site: Site, start: LocalDate, end: LocalDate): F[List[Ephemeris.Key]]
}

sealed trait OdbEventCommands[F[_]] {
  def addSlewEvent(
    obsId: Observation.Id,
    stage: SlewStage
  ): F[Unit]
  def queryNonSiderealObs(site: Site, start: LocalDate, end: LocalDate): F[List[Ephemeris.Key]]
}

object OdbProxy {
  def apply[F[_]](
    evCmds: OdbEventCommands[F]
  ): OdbProxy[F] =
    new OdbProxy[F] {
      export evCmds.*
    }

  def dummy[F[_]: Applicative]: OdbProxy[F] =
    OdbProxy[F](new DummyOdbCommands[F])

  class DummyOdbCommands[F[_]: Applicative] extends OdbEventCommands[F] {

    override def addSlewEvent(obsId: Observation.Id, stage: SlewStage): F[Unit] =
      Applicative[F].unit

    override def queryNonSiderealObs(
      site:  Site,
      start: LocalDate,
      end:   LocalDate
    ): F[List[Ephemeris.Key]] = List.empty.pure[F]
  }

  class OdbCommandsImpl[F[_]: MonadThrow: Clock](using
    L:      Logger[F],
    client: FetchClient[F, ObservationDB]
  ) extends OdbEventCommands[F] {

    override def addSlewEvent(obsId: Observation.Id, stage: SlewStage): F[Unit] =
      L.info(s"Adding slew event for obsId: $obsId, stage: $stage") *>
        clientTimeNow.flatMap: clientTime =>
          AddSlewEventMutation[F]
            .execute(obsId = obsId, stg = stage, clientTime = clientTime)
            .void

    private def extractNonsiderealTargets(
      m: ActiveNonsiderealTargetsQuery.Data.Observations.Matches
    ): List[Ephemeris.Key] =
      List(m.targetEnvironment.firstScienceTarget.flatMap(_.nonsidereal),
           m.targetEnvironment.blindOffsetTarget.flatMap(_.nonsidereal)
      ).flattenOption
        .flatMap(x => Ephemeris.Key.fromTypeAndDes.getOption((x.keyType, x.des)))

    // TODO: Query relevant non-sidereal guide targets
    override def queryNonSiderealObs(
      site:  Site,
      start: LocalDate,
      end:   LocalDate
    ): F[List[Ephemeris.Key]] =
      // Keep only the keys of each page, so full observations are not accumulated across pages.
      drain[F,
            (Observation.Id, List[Ephemeris.Key]),
            Observation.Id,
            (Boolean, List[(Observation.Id, List[Ephemeris.Key])])
      ](
        (after: Option[Observation.Id]) =>
          ActiveNonsiderealTargetsQuery[F]
            .query(site, start, end, after.orUnassign)
            .raiseGraphQLErrors
            .map(_.observations)
            .map(o => (o.hasMore, o.matches.map(m => (m.id, extractNonsiderealTargets(m))))),
        _._2,
        _._1,
        _._1
      ).map(_.flatMap(_._2).distinct)
  }
}
