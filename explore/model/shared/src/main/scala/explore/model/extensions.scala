// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.syntax.all.given
import lucuma.core.enums.ArcType
import lucuma.core.math.Arc
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.SiderealTargetWithId
import lucuma.schemas.model.TargetWithId

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import scala.annotation.targetName
import org.typelevel.log4cats.Logger

object extensions:
  // TODO Move this to lucuma-schemas (and remove this logic from TargetWithId)
  extension (target: Target.Sidereal)
    def at(i: Instant): Target.Sidereal = {
      val ldt            = LocalDateTime.ofInstant(i, ZoneOffset.UTC)
      val epoch          = Epoch.Julian.fromLocalDateTime(ldt).getOrElse(target.tracking.epoch)
      val trackingUpdate = (tracking: SiderealTracking) =>
        tracking.at(i).fold(tracking) { c =>
          val update = SiderealTracking.baseCoordinates.replace(c) >>> SiderealTracking.epoch
            .replace(epoch)
          update(tracking)
        }

      Target.Sidereal.tracking.modify(trackingUpdate)(target)
    }

  extension (target: Target)
    // If the target is sidereal, update it to the given instant.
    // Someday we may need to handle nonsidereal targets...
    def at(i: Instant): Target = target match
      case st @ Target.Sidereal(_, _, _, _) => st.at(i)
      case Target.Nonsidereal(_, _, _)      => target
      case Target.Opportunity(_, _, _)      => target

    // When we have nonsidereals...
    def coordsOrRegion: Option[Either[Coordinates, Region]] = target match
      case Target.Sidereal(_, tracking, _, _) => tracking.baseCoordinates.asLeft.some
      case Target.Nonsidereal(_, _, _)        => none
      case Target.Opportunity(_, region, _)   => region.asRight.some

  extension (targetWithId: TargetWithId)
    def at(i: Instant): TargetWithId =
      TargetWithId.target.replace(targetWithId.target.at(i))(targetWithId)

  extension (targets: NonEmptyList[TargetWithId])
    def baseTracking: Option[Tracking] =
      Tracking.fromAsterism(targets.map(_.target))

    def toSidereal: List[SiderealTargetWithId] =
      targets.toList.map(_.toSidereal).flattenOption

    // This uses ObjectTracking.orRegionFromAsterism, which treats any asterism with a
    // ToO as a ToO and returns the region of the first ToO it finds. Since we "shouldn't"
    // have asterisms with multiple ToOs, this is probably fine.
    def coordsOrRegionAt(vizTime: Option[Instant]): Option[Either[Coordinates, Region]] =
      Tracking
        .orRegionFromAsterism(targets.map(_.target)) match
        case Left(tracking) =>
          vizTime.fold(tracking.baseCoordinates.asLeft.some)(v => tracking.at(v).map(_.asLeft))
        case Right(region)  => region.asRight.some

    def isMixed: Boolean =
      targets
        .map {
          _.target match
            case Target.Sidereal(_, _, _, _) => 0
            case Target.Nonsidereal(_, _, _) => 1
            case Target.Opportunity(_, _, _) => 2
        }
        .distinct
        .size > 1

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

  extension (tracking: Tracking)
    def baseCoordinates: Coordinates = tracking match
      case SiderealTracking(baseCoordinates, _, _, _, _) => baseCoordinates
      case ConstantTracking(coordinates)                 => coordinates
      case CompositeTracking(nel)                        => Coordinates.centerOf(nel.map(_.baseCoordinates))
      case _                                             => sys.error("Non sidereals are not supported")

  extension [F[_]: {MonadThrow, Logger}, A](f: F[A])
    def logErrors(msg: String = ""): F[A] =
      f.onError:
        case e => Logger[F].error(e)(msg)

  extension [F[_]: MonadThrow: Logger, A](s: fs2.Stream[F, A])
    def onErrorLog(msg: String = ""): fs2.Stream[F, A] =
      s.handleErrorWith { e =>
        fs2.Stream.eval(Logger[F].error(e)(msg)) >> fs2.Stream.raiseError[F](e)
      }
