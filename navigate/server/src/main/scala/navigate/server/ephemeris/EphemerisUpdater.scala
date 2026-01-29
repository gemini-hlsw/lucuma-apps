// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.ephemeris

import cats.Applicative
import cats.MonadThrow
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.core.enums.Site
import lucuma.core.model.Ephemeris
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp
import navigate.model.OdbNonsidereal
import navigate.server.OdbProxy
import org.typelevel.log4cats.Logger

trait EphemerisUpdater[F[_]] {
  def refreshEphemerides(dateInterval: DateInterval): F[Unit]
}

object EphemerisUpdater {

  case class EphemerisTimeRange(ephemeris: Ephemeris.Key, start: Timestamp, end: Timestamp)

  private def createEphemerisFiles[F[_]: Applicative](
    @annotation.unused targets: List[OdbNonsidereal]
  ): F[List[Throwable]] = List.empty[Throwable].pure[F]

  private def readEphemerisFiles[F[_]: Async](
    filePath: Path
  ): F[List[EphemerisFile]] = Files.forAsync
    .list(filePath)
    .filter(_.extName === ".eph")
    .evalMap(EphemerisFile.EphemerisParser.parse)
    .compile
    .toList
    .map(_.flattenOption)

  private def threshFilesAndTargets[F[_]](
    @annotation.unused targets: List[OdbNonsidereal],
    files:                      List[EphemerisFile]
  ): (List[OdbNonsidereal], List[EphemerisFile]) = ???

  private def deleteFiles[F[_]: Applicative](@annotation.unused l: List[EphemerisFile]): F[Unit] =
    Applicative[F].unit

  private def reportErrors[F[_]: Applicative](@annotation.unused errors: List[Throwable]): F[Unit] =
    Applicative[F].unit

  def build[F[_]: {MonadThrow, Async}](
    site:                 Site,
    filePath:             Path,
    odbProxy:             OdbProxy[F]
  )(using
    @annotation.unused L: Logger[F]
  ): EphemerisUpdater[F] = new EphemerisUpdater[F] {
    /*
     * The process to refresh the ephemeris files in TCS must:
     * Collect all the non sidereal targets used by active observations for a given time interval (usually 24 hours)
     * Read the ephemeris files available to TCS and collect information of non sidereal target ids and time range
     * Split the ephemeris files between the ones that covered the required targets and the ones that don't. Delete the ones that cover only the past.
     * Split non sidereal targets between the ones covered by the existing files and the ones that are not covered.
     * For the ones that are not covered, read ephemeris data from Horizons, format and save them.
     * If files cannot be created, send notification emails.
     */
    override def refreshEphemerides(dateInterval: DateInterval): F[Unit] = for {
      files             <- readEphemerisFiles(filePath)
      targets           <- odbProxy.queryNonSiderealObs(site, dateInterval.start, dateInterval.end)
      (toLoad, toDelete) = threshFilesAndTargets(targets, files)
      _                 <- deleteFiles(toDelete)
      errors            <- createEphemerisFiles(toLoad)
      _                 <- reportErrors(errors).whenA(errors.nonEmpty)
    } yield ()

  }
}
