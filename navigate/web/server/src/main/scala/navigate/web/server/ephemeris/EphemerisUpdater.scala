// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.ephemeris

import cats.Applicative
import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import fs2.io.file.Path
import lucuma.core.enums.Site
import lucuma.core.util.DateInterval
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Scalars.Timestamp
import navigate.model.OdbNonsidereal
import navigate.queries.ObsQueriesGQL.ActiveNonsiderealTargetsQuery
import navigate.queries.ObsQueriesGQL.ActiveNonsiderealTargetsQuery.Data
import org.typelevel.log4cats.Logger

trait EphemerisUpdater[F[_]] {
  def refreshEphemerides(dateInterval: DateInterval): F[Unit]
}

object EphemerisUpdater {

  private def extractNonsiderealTargets(data: Data): List[OdbNonsidereal] =
    data.observations.matches
      .map(_.targetEnvironment)
      .flatMap(te =>
        te.guideEnvironment.guideTargets.map(_.nonsidereal) :+ te.firstScienceTarget
          .flatMap(_.nonsidereal) :+ te.blindOffsetTarget.flatMap(_.nonsidereal)
      )
      .flattenOption

  case class EphemerisTimeRange(target: OdbNonsidereal, start: Timestamp, end: Timestamp)

  case class EphemerisFile(fileName: String, start: Timestamp, end: Timestamp)

  private def createEphemerisFiles[F[_]: Applicative](
    @annotation.unused targets: List[OdbNonsidereal]
  ): F[List[Throwable]] = List.empty[Throwable].pure[F]

  private def readEphemerisFiles[F[_]: Applicative](
    @annotation.unused filePath: Path
  ): F[List[EphemerisFile]] = List.empty[EphemerisFile].pure[F]

  private def threshFilesAndTargets[F[_]](
    @annotation.unused targets: List[OdbNonsidereal],
    files:                      List[EphemerisFile]
  ): (List[OdbNonsidereal], List[EphemerisFile]) = ???

  private def deleteFiles[F[_]: Applicative](@annotation.unused l: List[EphemerisFile]): F[Unit] =
    Applicative[F].unit

  private def reportErrors[F[_]: Applicative](@annotation.unused errors: List[Throwable]): F[Unit] =
    Applicative[F].unit

  def build[F[_]: MonadThrow](
    site:                 Site,
    filePath:             Path
  )(using
    @annotation.unused L: Logger[F],
    client:               FetchClient[F, ObservationDB]
  ): EphemerisUpdater[F] = new EphemerisUpdater[F] {
    /*
     * The process to refresh the ephemeris files in TCS must:
     * Collect all the non sidereal targets used by active observations for a given time interval (usually 24 hours)
     * Read the ephemeris files available to TCS and collect information of non sidereal target ids and time range
     * Split the ephemeris files between the ones that covered the required targets and the ones that don't. Delete the latest.
     * Split non sidereal targets between the ones covered by the existing files and the ones that are not covered.
     * For the ones that are not covered, read ephemeris data from Horizons, format and save them.
     * If files cannot be created, send notification emails.
     */
    override def refreshEphemerides(dateInterval: DateInterval): F[Unit] = for {
      files             <- readEphemerisFiles(filePath)
      targets           <- ActiveNonsiderealTargetsQuery[F]
                             .query(site, dateInterval.start, dateInterval.end)
                             .map(_.map(extractNonsiderealTargets))
                             .raiseGraphQLErrors
      (toLoad, toDelete) = threshFilesAndTargets(targets, files)
      _                 <- deleteFiles(toDelete)
      errors            <- createEphemerisFiles(toLoad)
      _                 <- reportErrors(errors).whenA(errors.nonEmpty)
    } yield ()

  }
}
