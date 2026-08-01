// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.ui.enums.Theme
import monocle.Focus
import monocle.Lens
import observe.model.enums.ObserveLogLevel

// UI-only preferences for Observe.
//
// Observe keeps them entirely in the browser, as a single JSON blob per user in `window.localStorage`.
// See `UserPreferencesStorage` / `LocalUserPreferences` for the persistence seam.
//
// As for explore we are lenient in decoding if a field or the whole preferences is missing we
// use sane defaults.
//
// When adding a field:
//   - give it a default used by `Default`,
//   - decode it as `Option[...]` in the `Decoder` below so an older blob (missing the field) still
//     loads instead of failing the whole decode.
case class UserPreferences(
  isAudioActivated: IsAudioActivated,
  theme:            Theme,
  logLevel:         ObserveLogLevel,
  // When false the log panel uses the site timezone (local); when true it uses UTC.
  logTimeIsUTC:     Boolean
) derives Eq

object UserPreferences:
  val Default: UserPreferences = UserPreferences(
    isAudioActivated = IsAudioActivated.True,
    theme = Theme.Dark,
    logLevel = ObserveLogLevel.Info,
    logTimeIsUTC = false
  )

  val isAudioActivated: Lens[UserPreferences, IsAudioActivated] =
    Focus[UserPreferences](_.isAudioActivated)
  val theme: Lens[UserPreferences, Theme]        =
    Focus[UserPreferences](_.theme)
  val logLevel: Lens[UserPreferences, ObserveLogLevel] =
    Focus[UserPreferences](_.logLevel)
  val logTimeIsUTC: Lens[UserPreferences, Boolean] =
    Focus[UserPreferences](_.logTimeIsUTC)

  // Lenient: each field is decoded as an Option and falls back to its default when absent, so a
  // blob written by an older (or newer) version of the app degrades gracefully instead of failing.
  // Any remaining decode failure (corrupt JSON, unknown enum value, ...) is additionally caught at
  // the storage layer, which replaces the whole blob with `Default` -- the "use defaults on
  // failure" policy.
  given Decoder[UserPreferences] =
    Decoder.instance: c =>
      for
        audio    <- c.downField("isAudioActivated").as[Option[IsAudioActivated]]
        theme    <- c.downField("theme").as[Option[Theme]]
        logLevel <- c.downField("logLevel").as[Option[ObserveLogLevel]]
        logUtc   <- c.downField("logTimeIsUTC").as[Option[Boolean]]
      yield UserPreferences(
        isAudioActivated = audio.getOrElse(Default.isAudioActivated),
        theme = theme.getOrElse(Default.theme),
        logLevel = logLevel.getOrElse(Default.logLevel),
        logTimeIsUTC = logUtc.getOrElse(Default.logTimeIsUTC)
      )

  given Encoder[UserPreferences] =
    Encoder.forProduct4("isAudioActivated", "theme", "logLevel", "logTimeIsUTC"): p =>
      (p.isAudioActivated, p.theme, p.logLevel, p.logTimeIsUTC)
