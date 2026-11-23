// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.utils

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all.*
import coulomb.Quantity
import crystal.ViewF
import crystal.ViewOptF
import crystal.react.ReuseViewOptF
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.optics.*
import lucuma.core.util.Enumerated
import monocle.function.At.at
import monocle.function.Index
import monocle.function.Index.index
import org.scalajs.dom

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

extension [A](list: List[A])
  def modFirstWhere(find: A => Boolean, mod: A => A): List[A] =
    list.indexWhere(find) match
      case -1 => list
      case n  => (list.take(n) :+ mod(list(n))) ++ list.drop(n + 1)

  def removeFirstWhere(find: A => Boolean): List[A] =
    list.indexWhere(find) match
      case -1 => list
      case n  => list.take(n) ++ list.drop(n + 1)

extension [A, B, C, D](list: List[(A, B, C, D)])
  def unzip4: (List[A], List[B], List[C], List[D]) =
    list.foldRight((List.empty[A], List.empty[B], List.empty[C], List.empty[D]))((tuple, accum) =>
      (tuple._1 :: accum._1, tuple._2 :: accum._2, tuple._3 :: accum._3, tuple._4 :: accum._4)
    )

/**
 * Like `view.zoom(index(i))`, but the zoomed value is given instead of being looked up. `zoom`
 * evaluates its getter on the current value right away, which for lists is O(i), so zooming into
 * every element was O(n²). The getter is still used for the previous and current values passed to
 * modification callbacks.
 */
private def zoomAtIndex[F[_]: Monad, S, A](view: ViewF[F, S], i: Int, a: A)(using
  Index[S, Int, A]
): ViewF[F, A] =
  val atIndex = index[S, Int, A](i)
  // It's safe to "get" since we are only invoking for existing indices.
  val getA    = atIndex.getOption.andThen(_.get)
  ViewF(a,
        (f, cb) =>
          view.modCB(atIndex.modify(f), (previous, current) => cb(getA(previous), getA(current)))
  )

extension [F[_]: Monad, A](listView: ViewF[F, List[A]])
  @targetName("ListView_toListOfViews")
  def toListOfViews: List[ViewF[F, A]] =
    listView.get.zipWithIndex.map((a, i) => zoomAtIndex(listView, i, a))

extension [F[_]: Monad, A](nelView: ViewF[F, NonEmptyList[A]])
  @targetName("NelView_toNelOfViews")
  def toNelOfViews: NonEmptyList[ViewF[F, A]] =
    nelView.get.zipWithIndex.map((a, i) => zoomAtIndex(nelView, i, a))

extension [F[_], K, V](mapView: ViewF[F, Map[K, V]])
  @targetName("MapView_toListOfViews")
  def toListOfViews: List[(K, ViewF[F, V])] =
    // It's safe to "get" since we are only invoking for existing keys.
    mapView.get.keys.toList.map(k =>
      k -> mapView.zoom(at[Map[K, V], K, Option[V]](k)).zoom(_.get)(f => _.map(f))
    )

extension [F[_], K, V](sortedMapView: ViewF[F, SortedMap[K, V]])
  @targetName("SortedMapView_toListOfViews")
  def toListOfViews: List[(K, ViewF[F, V])] =
    // It's safe to "get" since we are only invoking for existing keys.
    sortedMapView.get.keys.toList.map(k =>
      k -> sortedMapView.zoom(at[SortedMap[K, V], K, Option[V]](k)).zoom(_.get)(f => _.map(f))
    )

given [A: Enumerated, B: Enumerated]: Enumerated[(A, B)] =
  Enumerated
    .fromNEL(NonEmptyList.fromListUnsafe((Enumerated[A].all, Enumerated[B].all).tupled))
    .withTag { case (a, b) => s"${Enumerated[A].tag(a)}, ${Enumerated[B].tag(b)} " }

// Coulomb implicits
extension [F[_], N, U](self: ViewF[F, Quantity[N, U]])
  def stripQuantity: ViewF[F, N] = self.as(quantityIso[N, U])

extension [F[_], N, U](self: ViewOptF[F, Quantity[N, U]])
  def stripQuantity: ViewOptF[F, N] = self.as(quantityIso[N, U])

extension [F[_], N, U](self: ReuseViewOptF[F, Quantity[N, U]])
  def stripQuantity: ReuseViewOptF[F, N] = self.as(quantityIso[N, U])

extension [A](list: List[A])
  def zipWithMappedIndex[B](f: Int => B): List[(A, B)] =
    list.zipWithIndex.map((v, i) => (v, f(i)))

given Conversion[NonEmptyString, VdomNode] with
  def apply(s: NonEmptyString): VdomNode =
    s.value

def showEnvironment[F[_]: Sync](env: ExecutionEnvironment): F[Unit] =
  Sync[F]
    .delay:
      val nonProdBanner = dom.document.createElement("div")
      nonProdBanner.id = "non-prod-banner"
      nonProdBanner.textContent = env.tag
      dom.document.body.appendChild(nonProdBanner)
    .whenA:
      env =!= ExecutionEnvironment.Production &&
      dom.document.querySelector("#non-prod-banner") == null
