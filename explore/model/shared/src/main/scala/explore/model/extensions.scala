// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.MonadThrow
import cats.syntax.all.*
import cats.syntax.all.given
import lucuma.core.enums.ArcType
import lucuma.core.math.Arc
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.model.Target
import lucuma.schemas.model.CoordinatesAt
import org.typelevel.log4cats.Logger

import scala.annotation.targetName

object extensions:

  // TODO: HONSIDEREAL: Is this being used?
  extension (target: Target)
    def baseCoordsOrRegion: Option[Either[Coordinates, Region]] = target match
      case Target.Sidereal(_, tracking, _, _) => tracking.baseCoordinates.asLeft.some
      case Target.Nonsidereal(_, _, _)        => none
      case Target.Opportunity(_, region, _)   => region.asRight.some

    def regionOrBaseCoords: Option[Either[Region, CoordinatesAt]] = target match
      case Target.Sidereal(_, tracking, _, _) =>
        CoordinatesAt(tracking.epoch.toInstant, tracking.baseCoordinates).asRight.some
      case Target.Nonsidereal(_, _, _)        => none
      case Target.Opportunity(_, region, _)   => region.asLeft.some

  // TODO: NONSIDEREAL: Moved to explore.syntax.ui until comment below, can remove when all
  // the necessary target tables are updated.
  extension [A](arc: Arc[A])
    def format(f: A => String): String = arc match
      case Arc.Empty()             => "Empty"
      case Arc.Full()              => "Full"
      case Arc.Partial(start, end) => s"${f(start)} - ${f(end)}"
    def toArcType: ArcType             = arc match
      case Arc.Empty()       => ArcType.Empty
      case Arc.Full()        => ArcType.Full
      case Arc.Partial(_, _) => ArcType.Partial

  extension (coordsOrRegion: Option[Either[Coordinates, Region]])
    def ra: Option[Either[RightAscension, Arc[RightAscension]]] =
      coordsOrRegion.map(_.bimap(_.ra, _.raArc))
    def dec: Option[Either[Declination, Arc[Declination]]]      =
      coordsOrRegion.map(_.bimap(_.dec, _.decArc))

  extension (raOrArc: Option[Either[RightAscension, Arc[RightAscension]]])
    @targetName("formatRA")
    def format(f: RightAscension => String): String = raOrArc match
      case None                                  => ""
      case Some(Left(ra))                        => f(ra)
      case Some(Right(arc: Arc[RightAscension])) => arc.format(f)

  extension (decOrArc: Option[Either[Declination, Arc[Declination]]])
    @targetName("formatDec")
    def format(f: Declination => String): String = decOrArc match
      case None                               => ""
      case Some(Left(dec))                    => f(dec)
      case Some(Right(arc: Arc[Declination])) => arc.format(f)
  // TODO: End of moved to explore.syntax.ui

  extension [F[_]: {MonadThrow, Logger}, A](f: F[A])
    def logErrors(msg: String = ""): F[A] =
      f.onError:
        case e => Logger[F].error(e)(msg)

  extension [F[_]: {MonadThrow, Logger}, A](s: fs2.Stream[F, A])
    def onErrorLog(msg: String = ""): fs2.Stream[F, A] =
      s.handleErrorWith { e =>
        fs2.Stream.eval(Logger[F].error(e)(msg)) >> fs2.Stream.raiseError[F](e)
      }
