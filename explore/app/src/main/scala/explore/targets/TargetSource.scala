// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.effect.Async
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.TargetList
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.clients.SimbadClient
import lucuma.core.enums.CatalogName
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SiderealTracking
import lucuma.horizons.HorizonsClient
import org.typelevel.log4cats.Logger

sealed trait TargetSource[F[_]]:
  def name: String
  def existing: Boolean

  // whether the target source can be previewed in Aladin. Non-sidereal sources,
  // for example, cannot be previewed because they don't have a location.
  def canPreview: Boolean

  def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]]

object TargetSource:
  case class FromProgram[F[_]: Async](targets: TargetList, filterToOs: Boolean = false)
      extends TargetSource[F]:
    val name = "Program"

    val existing: Boolean   = true
    val canPreview: Boolean = true

    def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      val matches =
        targets.values
          .filter(twid =>
            twid.target.name.value.toLowerCase.startsWith(name.value.toLowerCase)
              && twid.disposition === TargetDisposition.Science
              && !(filterToOs && twid.isTargetOfOpportunity)
          )
          .toList
          .sortBy(_.target.name.value.toLowerCase)
          .map(twid => TargetSearchResult(twid.toOptId, none))
      List(matches.pure)

    override def toString(): String = name

  case class FromSimbad[F[_]: {Async, Logger as L}](
    simbad: SimbadClient[F]
  ) extends TargetSource[F]:
    val name: String = CatalogName.Simbad.tag.capitalize

    val existing: Boolean   = false
    val canPreview: Boolean = true

    // if rv/px/pm are not set, set them to 0
    private def adjustTracking(t: TargetSearchResult) =
      val p =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.parallax)
          .modify:
            case a @ Some(_) => a
            case _           => Parallax.Zero.some
      val m =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.properMotion)
          .modify:
            case a @ Some(_) => a
            case _           => ProperMotion.Zero.some
      val v =
        TargetSearchResult.siderealTracking
          .andThen(SiderealTracking.radialVelocity)
          .modify:
            case a @ Some(_) => a
            case _           => RadialVelocity.Zero.some
      (p >>> m >>> v)(t)

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      val escapedName: String = name.value.replaceAll("\\*", "\\\\*")

      val regularSearch: F[List[CatalogTargetResult]] =
        NonEmptyString
          .from(escapedName)
          .toOption
          .map: term =>
            // Swallow errors, return empty
            simbad
              .search(name = term)
              .flatMap(
                _.fold(e => L.warn(s"Problems calling simbad $e").as(List.empty), List(_).pure[F])
              )
          .getOrElse(List.empty.pure[F])

      // This a heuristic based on observed Simbad behavior.
      val wildcardSearches: List[F[List[CatalogTargetResult]]] = List(
        NonEmptyString.unsafeFrom(s"$escapedName*"),
        NonEmptyString.unsafeFrom(
          s"${escapedName.replaceFirst("([A-Za-z-\\.]+)(\\S.*)", "$1 $2")}*"
        ),
        NonEmptyString.unsafeFrom(s"NAME $escapedName*")
      ).distinct.map(term =>
        L.debug(s"Searching Simbad: [$term]") >>
          simbad.search(term, wildcard = true, 100.some)
      )

      (regularSearch +: wildcardSearches).map:
        _.map:
          _.map: r =>
            TargetSearchResult.fromCatalogTargetResult(r)
          .map(adjustTracking)

    override def toString: String = CatalogName.Simbad.toString

  case class FromHorizons[F[_]: {Async, Logger as L}](
    horizons: HorizonsClient[F]
  ) extends TargetSource[F]:
    val name: String = "Horizons"

    val existing: Boolean   = false
    val canPreview: Boolean = false

    override def searches(name: NonEmptyString): List[F[List[TargetSearchResult]]] =
      List(
        HorizonsClient.Search.Comet(name.value),
        HorizonsClient.Search.Asteroid(name.value),
        HorizonsClient.Search.MajorBody(name.value)
      ).map: search =>
        L.debug(s"Searching Horizons: [$search]") >>
          horizons
            .resolve(search)
            .flatMap(
              _.fold(
                e => L.warn(s"Problems calling horizons $e").as(List.empty),
                _.map((ek, name) =>
                  NonEmptyString
                    .from(name)
                    .toOption
                    .map(n => TargetSearchResult.fromHorizonsSearchResult(n, ek))
                ).flatten.pure[F]
              )
            )

    override def toString: String = name

  // TODO Test
  given orderTargetSource[F[_]]: Order[TargetSource[F]] = Order.from {
    // doesn't make sense to have more than one of each source, but we'll put FromProgram first,
    // then Simbad then Horizons.
    case (TargetSource.FromProgram(_, _), _) => -1
    case (_, TargetSource.FromProgram(_, _)) => 1
    case (TargetSource.FromSimbad(_), _)     => -1
    case (_, TargetSource.FromSimbad(_))     => 1
    case _                                   => 0
  }

  given reuseTargetSource[F[_]]: Reusability[TargetSource[F]] = Reusability.byEq
