// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.effect.Sync
import cats.syntax.all.*
import lucuma.core.model.User

// Persistence for `UserPreferences`.
trait UserPreferencesStorage[F[_]]:
  def load(userId: User.Id): F[UserPreferences]
  def save(userId: User.Id, prefs: UserPreferences): F[Unit]

object UserPreferencesStorage:
  def build[F[_]: Sync as F](
    underlying: UserPreferencesStorage[F]
  ): UserPreferencesStorage[F] =
    new UserPreferencesStorage[F]:
      def load(userId: User.Id): F[UserPreferences]              =
        underlying.load(userId).handleError(_ => UserPreferences.Default)
      def save(userId: User.Id, prefs: UserPreferences): F[Unit] =
        underlying.save(userId, prefs).handleErrorWith(_ => F.unit)
