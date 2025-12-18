// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.MonadThrow
import cats.syntax.all.given
import org.typelevel.log4cats.Logger

object extensions:

  extension [F[_]: {MonadThrow, Logger}, A](f: F[A])
    def logErrors(msg: String = ""): F[A] =
      f.onError:
        case e => Logger[F].error(e)(msg)

  extension [F[_]: {MonadThrow, Logger}, A](s: fs2.Stream[F, A])
    def onErrorLog(msg: String = ""): fs2.Stream[F, A] =
      s.handleErrorWith { e =>
        fs2.Stream.eval(Logger[F].error(e)(msg)) >> fs2.Stream.raiseError[F](e)
      }
