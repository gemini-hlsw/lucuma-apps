// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.utils

import cats.Applicative
import cats.effect.std.Random
import org.typelevel.log4cats.Logger
import retry.*
import retry.RetryDetails.*
import retry.RetryPolicies.*

import java.util as ju
import scala.concurrent.duration.*

import ju.concurrent.TimeUnit

trait RetryHelpers:
  def retryPolicy[F[_]: Applicative: Random] =
    capDelay(
      FiniteDuration.apply(5, TimeUnit.SECONDS),
      fullJitter[F](FiniteDuration.apply(10, TimeUnit.MILLISECONDS))
    ).join(limitRetries[F](12))

  def logError[F[_]: Logger](msg: String)(err: Throwable, details: RetryDetails): F[Unit] =
    details.nextStepIfUnsuccessful match
      case NextStep.DelayAndRetry(_) =>
        Logger[F].warn(err)(s"$msg failed - Will retry. Retries so far: [${details.retriesSoFar}]")
      case NextStep.GiveUp           =>
        Logger[F].error(err)(s"$msg failed - Giving up after [${details.retriesSoFar}] retries.")

object RetryHelpers extends RetryHelpers
