// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.epics

import cats.Applicative
import cats.ApplicativeThrow
import cats.FlatMap
import cats.Monad
import cats.MonadThrow
import cats.Parallel
import cats.arrow.FunctionK
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.Mutex
import cats.syntax.all.*
import fs2.Stream
import mouse.boolean.*
import navigate.epics.Channel.StreamEvent
import navigate.epics.EpicsSystem.TelltaleChannel

import scala.concurrent.duration.FiniteDuration

/**
 * <code>VerifiedEpics</code> keeps a program that accesses EPICS channels, together with a list of
 * the channels involved. Calling the method <code>verifiedRun</code> on a
 * <code>VerifiedEpics</code> instance will produce a program that adds a verification of all the
 * channels involved before executing the original program. Several constructors and combiners are
 * provided.
 */
object VerifiedEpics {

  trait ChannelTracker[F[_], A] {
    val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]]
    val run: A
  }

  case class Const[F[_], A](v: A) extends ChannelTracker[F, A] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map.empty
    override val run: A                                                  = v
  }

  case class Apply[F[_], A, B](fa: ChannelTracker[F, A], fab: ChannelTracker[F, A => B])
      extends ChannelTracker[F, B] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] =
      merge(fa.systems, fab.systems)
    override val run: B                                                  = fab.run(fa.run)
  }

  def merge[F[_]](
    m1: Map[TelltaleChannel[F], Set[RemoteChannel[F]]],
    m2: Map[TelltaleChannel[F], Set[RemoteChannel[F]]]
  ): Map[TelltaleChannel[F], Set[RemoteChannel[F]]] =
    (m1.toList ++ m2.toList).groupMap(_._1)(_._2).view.mapValues(_.reduce(_ ++ _)).toMap

  def pure[F[_], A](v: A): ChannelTracker[F, A] = Const(v)
  def unit[F[_]]: ChannelTracker[F, Unit]       = pure[F, Unit](())

  extension [F[_], A](v: ChannelTracker[F, A]) {

    /**
     * Transforms the tracked value as a whole. For a `VerifiedEpics[F, G, A]` this maps the `G[A]`
     * program, not its result; use `.map` for that.
     */
    def mapRun[B](ff: A => B): ChannelTracker[F, B] = Apply(v, pure(ff))
  }

  type VerifiedEpics[F[_], G[_], A] = ChannelTracker[F, G[A]]

  case class LiftF[F[_], G[_], A, B](fa: G[A]) extends VerifiedEpics[F, G, A] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map.empty
    override val run: G[A]                                               = fa
  }

  case class IfF[F[_], G[_]: FlatMap, A](
    cond:     G[Boolean],
    trueVal:  VerifiedEpics[F, G, A],
    falseVal: VerifiedEpics[F, G, A]
  ) extends VerifiedEpics[F, G, A] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] =
      merge(trueVal.systems, falseVal.systems)
    override val run: G[A]                                               = cond.flatMap(_.fold(trueVal.run, falseVal.run))
  }

  case class Get[F[_]: MonadThrow, A](tt: TelltaleChannel[F], ch: Channel[F, A])
      extends VerifiedEpics[F, F, A] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map(tt -> Set(ch))
    override val run: F[A]                                               = ch.getOption.flatMap(
      _.map(_.pure[F]).getOrElse(
        MonadThrow[F].raiseError(new Throwable(s"Cannot read value for channel ${ch.getName}"))
      )
    )
  }

  case class GetOption[F[_], A](tt: TelltaleChannel[F], ch: Channel[F, A])
      extends VerifiedEpics[F, F, Option[A]] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map(tt -> Set(ch))
    override val run: F[Option[A]]                                       = ch.getOption
  }

  case class Put[F[_]: FlatMap, A](tt: TelltaleChannel[F], ch: Channel[F, A], fa: F[A])
      extends VerifiedEpics[F, F, Unit] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map(tt -> Set(ch))
    override val run: F[Unit]                                            = fa.flatMap(ch.put)
  }

  case class EventStream[F[_]: {Dispatcher, Concurrent}, A](
    tt: TelltaleChannel[F],
    ch: Channel[F, A]
  ) extends VerifiedEpics[F, Resource[F, *], Stream[F, StreamEvent[A]]] {
    override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map(tt -> Set(ch))
    override val run: Resource[F, Stream[F, StreamEvent[A]]]             = ch.eventStream
  }

  def pureF[F[_], G[_]: Applicative, A](v: A): VerifiedEpics[F, G, A]    = pure[F, G[A]](v.pure[G])
  def unit[F[_], G[_]: Applicative]: VerifiedEpics[F, G, Unit]           =
    pure[F, G[Unit]](Applicative[G].unit)
  def liftF[F[_], G[_], A](f:              G[A]): VerifiedEpics[F, G, A] = LiftF(f)
  def readChannel[F[_]: MonadThrow, A](
    tt: TelltaleChannel[F],
    ch: Channel[F, A]
  ): VerifiedEpics[F, F, A]                                              =
    Get(tt, ch)
  def readOptionChannel[F[_], A](
    tt: TelltaleChannel[F],
    ch: Channel[F, A]
  ): VerifiedEpics[F, F, Option[A]]                                      =
    GetOption(tt, ch)
  def writeChannel[F[_]: FlatMap, A](tt: TelltaleChannel[F], ch: Channel[F, A])(
    fa: F[A]
  ): VerifiedEpics[F, F, Unit]                                           = Put(tt, ch, fa)
  def eventStream[F[_]: {Dispatcher, Concurrent}, A](
    tt: TelltaleChannel[F],
    ch: Channel[F, A]
  ): VerifiedEpics[F, Resource[F, *], Stream[F, StreamEvent[A]]]         = EventStream(tt, ch)
  def ifF[F[_], G[_]: FlatMap, A](cond: G[Boolean])(trueVal: => VerifiedEpics[F, G, A])(
    falseVal: => VerifiedEpics[F, G, A]
  ): VerifiedEpics[F, G, A]                                              = IfF(cond, trueVal, falseVal)

  extension [F[_], G[_]: Monad, A](v: VerifiedEpics[F, G, A]) {
    def flatMap[B](ff: G[A] => VerifiedEpics[F, G, B]): VerifiedEpics[F, G, B] =
      new ChannelTracker[F, G[B]] {
        private val fb                                                       = ff(v.run)
        override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] =
          merge(v.systems, fb.systems)
        override val run: G[B]                                               = v.run.flatMap(x => ff(x.pure[G]).run)
      }

    def productR[B](b: VerifiedEpics[F, G, B]): VerifiedEpics[F, G, B] = flatMap(_ => b)

    def *>[B](b: VerifiedEpics[F, G, B]): VerifiedEpics[F, G, B] = productR(b)

  }

  extension [F[_]: {Async, Parallel}, G[_]: {Async, ([L[_]] =>> FunctionK[F, L]) as fK}, A](
    v: VerifiedEpics[F, G, A]
  ) {
    def verifiedRun(connectionTimeout: FiniteDuration): G[A] =
      fK.apply(
        v.systems
          .map { case (k, v) => EpicsSystem(k, v) }
          .map(x => x.connectionsCheck(connectionTimeout).map((x, _)))
          .toList
          .parSequence
      ).flatMap { y =>
        val bads =
          y.collect { case (s, (false, l)) => (s, l.collect { case (c, false) => c }) }.map {
            case (r, m) =>
              r.telltale.sysName ++
                m.isEmpty.fold("", m.mkString(" (channels ", ", ", ")"))
          }
        bads.isEmpty.fold(
          v.run,
          Async[G]
            .raiseError(new Throwable(s"Cannot connect to remote systems ${bads.mkString(", ")}"))
        )
      }

  }

  extension [F[_], A](v: VerifiedEpics[F, F, A]) {

    /**
     * Wraps the (unexecuted) program in a pure Resource, so it can be combined with Resource based
     * programs.
     */
    def inResource: VerifiedEpics[F, Resource[F, *], F[A]] = v.mapRun(Resource.pure[F, F[A]])
  }

  extension [F[_], G[_], A](v: VerifiedEpics[F, G, A]) {

    /**
     * Captures errors raised by the program in its result.
     */
    def attemptRun(using ApplicativeThrow[G]): VerifiedEpics[F, G, Either[Throwable, A]] =
      v.mapRun(_.attempt)

    def liftK[K[_]](using fK: FunctionK[G, K]): VerifiedEpics[F, K, A] =
      new VerifiedEpics[F, K, A] {
        override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = v.systems
        override val run: K[A]                                               = fK.apply(v.run)
      }

    /**
     * Makes the program run holding the mutex. The channels involved are the same. The verification
     * of the channels, if any, is done by the caller before running the program, so it is not
     * included in the locked section.
     */
    def locked(mutex: Mutex[G])(using MonadCancelThrow[G]): VerifiedEpics[F, G, A] =
      new VerifiedEpics[F, G, A] {
        override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = v.systems
        override val run: G[A]                                               = mutex.lock.surround(v.run)
      }
  }

  extension [F[_]: {Async, Parallel}, G[_]: {Async, [L[_]] =>> FunctionK[F, L]}, A](
    v: VerifiedEpics[F, G, A]
  ) {
    def attempt(connectionTimeout: FiniteDuration): VerifiedEpics[F, G, Either[Throwable, A]] =
      new VerifiedEpics[F, G, Either[Throwable, A]] {
        override val systems: Map[TelltaleChannel[F], Set[RemoteChannel[F]]] = Map.empty
        override val run: G[Either[Throwable, A]]                            = v.verifiedRun(connectionTimeout).attempt
      }
  }

  def applicativeChannelTracker[F[_]]: Applicative[[A] =>> ChannelTracker[F, A]] =
    new Applicative[[A] =>> ChannelTracker[F, A]] {
      override def pure[A](x: A): ChannelTracker[F, A] = VerifiedEpics.pure(x)

      override def ap[A, B](ff: ChannelTracker[F, A => B])(
        fa: ChannelTracker[F, A]
      ): ChannelTracker[F, B] = Apply(fa, ff)
    }

  /**
   * Applicative over the program result: `.map`, `mapN` and `tupled` on a `VerifiedEpics[F, G, A]`
   * work on `A` and sequence the `G` effects.
   *
   * Use `mapRun` to transform the `G[A]` program itself.
   */
  given applicativeVerifiedEpics[F[_], G[_]: Applicative]
    : Applicative[[A] =>> VerifiedEpics[F, G, A]] =
    applicativeChannelTracker[F].compose[G]

}
