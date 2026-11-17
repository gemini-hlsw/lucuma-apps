// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.ApplicativeThrow
import cats.Functor
import cats.MonadError
import cats.MonadThrow
import cats.data.*
import cats.syntax.all.*
import observe.model.*
import observe.server.engine.*

extension [F[_]: MonadThrow, A](s: EitherT[F, ObserveFailure, A])
  def liftF: F[A] =
    s.value.flatMap(_.liftTo[F])

extension [F[_], A, B](fa: EitherT[F, A, B])
  def widenRethrowT[T](using
    me: MonadError[F, T],
    at: A <:< T
  ): F[B] =
    fa.leftMap(at).rethrowT

extension (r: Either[Throwable, Response])
  def toResult[F[_]]: Result = r.fold(
    e =>
      e match {
        case e: ObserveFailure => Result.Error(ObserveFailure.explain(e))
        case e: Throwable      =>
          Result.Error(ObserveFailure.explain(ObserveFailure.ObserveException(e)))
      },
    r => Result.OK(r)
  )

extension [F[_]: ApplicativeThrow](r: F[Result])
  def safeResult: F[Result] = r.recover {
    case e: ObserveFailure => Result.Error(ObserveFailure.explain(e))
    case e: Throwable      => Result.Error(ObserveFailure.explain(ObserveFailure.ObserveException(e)))
  }

extension [F[_]: ApplicativeThrow, A <: Response](x: F[A])
  def toAction(kind:                                 ActionType): Action[F] = fromF[F](kind, x.attempt.map(_.toResult))

extension [F[_]: Functor](x: F[ConfigResult[F]])
  def toAction(kind: ActionType): Action[F] =
    fromF[F](kind, x.map(r => Result.OK(Response.Configured(r.handler.subsystem))))
