// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server

import cats.Applicative
import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import lucuma.core.enums.Site
import lucuma.core.enums.SlewStage
import lucuma.core.model.Ephemeris
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import mouse.boolean.given
import navigate.queries.ObsQueriesGQL.ActiveNonsiderealTargetsQuery
import navigate.queries.ObsQueriesGQL.AddSlewEventMutation
import navigate.queries.ObsQueriesGQL.NonsiderealGuideTargetsQuery
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

  class OdbCommandsImpl[F[_]: MonadThrow](using
    L:      Logger[F],
    client: FetchClient[F, ObservationDB]
  ) extends OdbEventCommands[F] {

    override def addSlewEvent(obsId: Observation.Id, stage: SlewStage): F[Unit] =
      L.info(s"Adding slew event for obsId: $obsId, stage: $stage") *>
        AddSlewEventMutation[F]
          .execute(obsId = obsId, stg = stage)
          .void

    private def extractNonsiderealTargets(
      data: ActiveNonsiderealTargetsQuery.Data
    ): List[Ephemeris.Key] =
      data.observations.matches
        .map(_.targetEnvironment)
        .flatMap(te =>
          List(te.firstScienceTarget.flatMap(_.nonsidereal),
               te.blindOffsetTarget.flatMap(_.nonsidereal)
          )
        )
        .map(_.flatMap(x => Ephemeris.Key.fromTypeAndDes.getOption((x.keyType, x.des))))
        .flattenOption

    override def queryNonSiderealObs(
      site:  Site,
      start: LocalDate,
      end:   LocalDate
    ): F[List[Ephemeris.Key]] = ActiveNonsiderealTargetsQuery[F]
      .query(site, start, end)
      .raiseGraphQLErrors
      .flatMap(d =>
        d.observations.matches
          .map(obs =>
            NonsiderealGuideTargetsQuery[F]
              .query(obs.id)
              .map(
                _.data.flatMap(
                  _.observation.flatMap { x =>
                    val r = x.targetEnvironment.guideEnvironment.guideTargets
                      .map(
                        _.nonsidereal
                          .flatMap(x => Ephemeris.Key.fromTypeAndDes.getOption((x.keyType, x.des)))
                      )
                      .flattenOption
                    r.nonEmpty.option(r)
                  }
                )
              )
          )
          .sequence
          .map(l => (extractNonsiderealTargets(d) ++ l.flattenOption.flatten).distinct)
      )
  }
}
