// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder
import monocle.Focus
import monocle.Lens

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
  isAudioActivated: IsAudioActivated
) derives Eq

object UserPreferences:
  val Default: UserPreferences = UserPreferences(isAudioActivated = IsAudioActivated.True)

  val isAudioActivated: Lens[UserPreferences, IsAudioActivated] =
    Focus[UserPreferences](_.isAudioActivated)

  // Lenient: each field is decoded as an Option and falls back to its default when absent, so a
  // blob written by an older (or newer) version of the app degrades gracefully instead of failing.
  // Any remaining decode failure (corrupt JSON, etc.) is additionally caught at the storage layer,
  // which replaces the whole blob with `Default` -- the "use defaults on failure" policy.
  given Decoder[UserPreferences] =
    Decoder.instance: c =>
      c.downField("isAudioActivated")
        .as[Option[IsAudioActivated]]
        .map(audio => UserPreferences(audio.getOrElse(Default.isAudioActivated)))

  given Encoder[UserPreferences] =
    Encoder.forProduct1("isAudioActivated")(_.isAudioActivated)
