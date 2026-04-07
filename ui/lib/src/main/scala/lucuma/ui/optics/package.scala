// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.optics

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics.SplitEpi
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan
import monocle.*
import monocle.function.At.atMap

import scala.collection.immutable.SortedSet

// This only behaves as a lawful lens as long as A and B are both null or both set.
def unsafeDisjointOptionZip[S, A, B](
  l1: Lens[S, Option[A]],
  l2: Lens[S, Option[B]]
): Lens[S, Option[(A, B)]] =
  Lens((s: S) => (l1.get(s), l2.get(s)).tupled)((ab: Option[(A, B)]) =>
    (s: S) => l2.replace(ab.map(_._2))(l1.replace(ab.map(_._1))(s))
  )

extension [A, B](iso: Iso[A, B])
  def option: Iso[Option[A], Option[B]] =
    Iso[Option[A], Option[B]](_.map(iso.get))(_.map(iso.reverseGet))

val OptionNonEmptyStringIso: Iso[Option[NonEmptyString], String] =
  Iso[Option[NonEmptyString], String](_.foldMap(_.value))(s => NonEmptyString.from(s).toOption)

// Note: truncates to Int.MaxValue - shouldn't have durations longer than that...
val TimeSpanSecondsSplitEpi: SplitEpi[TimeSpan, NonNegInt] = SplitEpi(
  ts => NonNegInt.unsafeFrom(math.min(ts.toSeconds.longValue, Int.MaxValue.toLong).toInt),
  secs => TimeSpan.unsafeFromDuration(secs.value.toLong.seconds)
)

def SortedSetFromList[A: Ordering]: SplitEpi[List[A], SortedSet[A]] =
  SplitEpi[List[A], SortedSet[A]](SortedSet.from(_), _.toList)

extension [From, To](self: Iso[From, To]) {
  def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  def asAdjuster: Adjuster[From, To] =
    new Adjuster[From, To] {
      def modify(f: To => To): From => From = self.modify(f)
      def set(to:   To): From => From       = self.replace(to)
    }
}

extension [From, To](self: Lens[From, To]) {
  def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  def asAdjuster: Adjuster[From, To] =
    new Adjuster[From, To] {
      def modify(f: To => To): From => From = self.modify(f)
      def set(to:   To): From => From       = self.replace(to)
    }

  def composeGetAdjust[X](other: GetAdjust[To, X]): GetAdjust[From, X] =
    new GetAdjust[From, X](
      self.asGetter.andThen(other.getter),
      asAdjuster.andThen(other.adjuster)
    )

  inline def asGetAdjust: GetAdjust[From, To] =
    new GetAdjust[From, To](self.asGetter, asAdjuster)
}

extension [From, To](self: Prism[From, To]) {
  def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  def asAdjuster: Adjuster[From, To] =
    new Adjuster[From, To] {
      def modify(f: To => To): From => From = self.modify(f)
      def set(to:   To): From => From       = self.replace(to)
    }
}

extension [From, To](self: Optional[From, To]) {
  def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  def asAdjuster: Adjuster[From, To] =
    new Adjuster[From, To] {
      def modify(f: To => To): From => From = self.modify(f)
      def set(to:   To): From => From       = self.replace(to)
    }
}

extension [From, To](self: Traversal[From, To]) {
  def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  inline def asAdjuster: Adjuster[From, To] =
    Adjuster(self.modify)
}

extension [From, To](self: Setter[From, To]) {
  def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    asAdjuster.andThen(other)

  def asAdjuster: Adjuster[From, To] =
    new Adjuster[From, To] {
      def modify(f: To => To): From => From = self.modify(f)
      def set(to:   To): From => From       = self.replace(to)
    }
}

extension [S, A](getAdjust: GetAdjust[S, Option[A]]) {
  def composeOptionLens[B](other: Lens[A, B]): GetAdjust[S, Option[B]] =
    GetAdjust(
      getAdjust.getter.composeOptionLens(other),
      getAdjust.adjuster.composeOptionLens(other)
    )

  def composeOptionGetAdjust[B](other: GetAdjust[A, B]): GetAdjust[S, Option[B]] =
    GetAdjust(
      getAdjust.getter.composeOptionGetter(other.getter),
      getAdjust.adjuster.composeOptionGetAdjust(other)
    )

  def composeOptionOptionLens[B](other: Lens[A, Option[B]]): GetAdjust[S, Option[B]] =
    composeOptionOptionGetAdjust(other.asGetAdjust)

  def composeOptionOptionGetAdjust[B](other: GetAdjust[A, Option[B]]): GetAdjust[S, Option[B]] =
    GetAdjust(
      getAdjust.getter.composeOptionOptionGetter(other.getter),
      getAdjust.adjuster.composeOptionOptionGetAdjust(other)
    )
}

extension [S, A](lens: Lens[S, Option[A]]) {
  def composeOptionLens[B](other: Lens[A, B]): GetAdjust[S, Option[B]] =
    GetAdjust(
      lens.asGetter.composeOptionLens(other),
      lens.asAdjuster.composeOptionLens(other)
    )

  def composeOptionOptionLens[B](other: Lens[A, Option[B]]): GetAdjust[S, Option[B]] =
    composeOptionOptionGetAdjust(other.asGetAdjust)

  def composeOptionOptionGetAdjust[B](other: GetAdjust[A, Option[B]]): GetAdjust[S, Option[B]] =
    GetAdjust(
      lens.asGetter.composeOptionOptionGetter(other.getter),
      lens.asAdjuster.composeOptionOptionGetAdjust(other)
    )
}

extension [S, A](getter: Getter[S, Option[A]]) {
  def composeOptionLens[B](other: Lens[A, B]): Getter[S, Option[B]] =
    composeOptionGetter(other.asGetter)

  def composeOptionGetter[B](other: Getter[A, B]): Getter[S, Option[B]] =
    Getter(
      getter.some.andThen(other).headOption
    )

  def composeOptionOptionGetter[B](other: Getter[A, Option[B]]): Getter[S, Option[B]] =
    Getter(
      getter.some.andThen(other.some).headOption
    )
}

extension [S, A](setter: Adjuster[S, Option[A]]) {
  def composeOptionLens[B](other: Lens[A, B]): Adjuster[S, Option[B]] =
    composeOptionGetAdjust(other.asGetAdjust)

  def composeOptionGetAdjust[B](other: GetAdjust[A, B]): Adjuster[S, Option[B]] =
    Adjuster { (modOptB: (Option[B] => Option[B])) =>
      setter.modify { optA =>
        optA.flatMap[A] { a =>
          modOptB(other.get(a).some).map(b => other.set(b)(a))
        }
      }
    }

  def composeOptionOptionLens[B](other: Lens[A, Option[B]]): Adjuster[S, Option[B]] =
    composeOptionOptionGetAdjust(other.asGetAdjust)

  def composeOptionOptionGetAdjust[B](other: GetAdjust[A, Option[B]]): Adjuster[S, Option[B]] =
    Adjuster { (modOptB: (Option[B] => Option[B])) =>
      setter.modify { optA =>
        optA.flatMap[A] { a =>
          modOptB(other.get(a)).map(b => other.set(b.some)(a))
        }
      }
    }
}

def getWithDefault[A](default: => A): Lens[Option[A], A] =
  Lens[Option[A], A](_.getOrElse(default))(a => _ => a.some)

// This should be safe to use with Maps that have .withDefault(...)
def atMapWithDefault[K, V](k: K, default: => V): Lens[Map[K, V], V] =
  atMap.at(k).andThen(getWithDefault(default))
