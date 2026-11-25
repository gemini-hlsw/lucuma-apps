// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.effect.*
import cats.effect.std.Semaphore
import cats.effect.syntax.all.*
import cats.syntax.all.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

/** Runs the side effects of undoable changes. */
trait UndoEffectRunner[F[_]]:
  /** Runs `fa`; a failure runs `rollback` if `fa` was not applied remotely, then `onFailure`. */
  def submit(fa: F[Unit], rollback: F[Unit], onFailure: F[Unit]): F[Unit]

object UndoEffectRunner:
  /** Runs every effect right away; `isRejection` tells errors proving nothing was applied. */
  def unordered[F[_]: MonadCancelThrow](
    isRejection: Throwable => Boolean,
    onError:     Throwable => F[Unit]
  ): UndoEffectRunner[F] =
    new UndoEffectRunner[F]:
      def submit(fa: F[Unit], rollback: F[Unit], onFailure: F[Unit]): F[Unit] =
        fa.onError(e => onError(e).attempt >> rollback.whenA(isRejection(e)) >> onFailure)

  /** For contexts with no remote calls: runs every effect right away and never rolls back. */
  def local[F[_]: MonadCancelThrow]: UndoEffectRunner[F] =
    unordered(_ => false, _ => MonadCancelThrow[F].unit)

  /** Like `unordered`, but runs one effect at a time, in order, each within `timeout`. */
  def serial[F[_]: Temporal](
    timeout:     FiniteDuration,
    isRejection: Throwable => Boolean,
    onError:     Throwable => F[Unit],
    onTimeout:   F[Unit]
  ): F[UndoEffectRunner[F]] =
    Semaphore[F](1).map: lock =>
      val runner: UndoEffectRunner[F] = unordered(isRejection, onError)
      new UndoEffectRunner[F]:
        def submit(fa: F[Unit], rollback: F[Unit], onFailure: F[Unit]): F[Unit] =
          lock.permit.surround:
            runner.submit(
              fa.timeoutAndForget(timeout).onError { case _: TimeoutException => onTimeout },
              rollback,
              onFailure
            )
