// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.Sync
import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.syntax.*
import lucuma.core.model.User
import observe.ui.model.UserPreferences
import observe.ui.model.UserPreferencesStorage
import org.scalajs.dom

// `UserPreferencesStorage` backed by `window.localStorage`.
//
// Preferences are stored as a single JSON blob, namespaced per user:
//   observe.prefs.<user-id>   e.g. observe.prefs.u-26fd21b3
//
// Failures are intentionally swallowed by `UserPreferencesStorage.withDefaultPolicy`
final case class LocalUserPreferences[F[_]: Sync as F]() extends UserPreferencesStorage[F]:
  private def key(userId: User.Id): String = s"observe.prefs.${userId.show}"

  def load(userId: User.Id): F[UserPreferences] =
    F.delay:
      Option(dom.window.localStorage.getItem(key(userId)))
        .flatMap(str => decode[UserPreferences](str).toOption)
        .getOrElse(UserPreferences.Default)
    .handleError(_ => UserPreferences.Default)

  def save(userId: User.Id, prefs: UserPreferences): F[Unit] =
    F.delay:
      dom.window.localStorage.setItem(key(userId), prefs.asJson.noSpaces)
    .void
