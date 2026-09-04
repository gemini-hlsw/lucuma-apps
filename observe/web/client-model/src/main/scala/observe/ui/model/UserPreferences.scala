// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.react.table.ColumnFilters
import lucuma.ui.enums.Theme
import monocle.Focus
import monocle.Lens
import observe.model.enums.ObserveLogLevel

// UI-only preferences for Observe.
//
// When adding a field:
//   - give it a default used by `Default`,
//   - decode it as `Option[...]` in the `Decoder` below so an older blob (missing the field) still
//     loads instead of failing the whole decode.
case class UserPreferences(
  isAudioActivated:     IsAudioActivated,
  theme:                Theme,
  logLevel:             ObserveLogLevel,
  logTimeIsUTC:         Boolean,
  obsListGlobalFilter:  String,
  obsListColumnFilters: ColumnFilters
) derives Eq

object UserPreferences:
  val Default: UserPreferences = UserPreferences(
    isAudioActivated = IsAudioActivated.True,
    theme = Theme.Dark,
    logLevel = ObserveLogLevel.Info,
    logTimeIsUTC = false,
    obsListGlobalFilter = "",
    obsListColumnFilters = ColumnFilters.Empty
  )

  val isAudioActivated: Lens[UserPreferences, IsAudioActivated]  =
    Focus[UserPreferences](_.isAudioActivated)
  val theme: Lens[UserPreferences, Theme]                        =
    Focus[UserPreferences](_.theme)
  val logLevel: Lens[UserPreferences, ObserveLogLevel]           =
    Focus[UserPreferences](_.logLevel)
  val logTimeIsUTC: Lens[UserPreferences, Boolean]               =
    Focus[UserPreferences](_.logTimeIsUTC)
  val obsListGlobalFilter: Lens[UserPreferences, String]         =
    Focus[UserPreferences](_.obsListGlobalFilter)
  val obsListColumnFilters: Lens[UserPreferences, ColumnFilters] =
    Focus[UserPreferences](_.obsListColumnFilters)

  // Lenient: each field is decoded as an Option and falls back to its default when absent, so a
  // blob written by an older (or newer) version of the app degrades gracefully instead of failing.
  given Decoder[UserPreferences] =
    Decoder.instance: c =>
      for
        audio    <- c.downField("isAudioActivated").as[Option[IsAudioActivated]]
        theme    <- c.downField("theme").as[Option[Theme]]
        logLevel <- c.downField("logLevel").as[Option[ObserveLogLevel]]
        logUtc   <- c.downField("logTimeIsUTC").as[Option[Boolean]]
        gf       <- c.downField("obsListGlobalFilter").as[Option[String]]
        cf       <- c.downField("obsListColumnFilters").as[Option[ColumnFilters]]
      yield UserPreferences(
        isAudioActivated = audio.getOrElse(Default.isAudioActivated),
        theme = theme.getOrElse(Default.theme),
        logLevel = logLevel.getOrElse(Default.logLevel),
        logTimeIsUTC = logUtc.getOrElse(Default.logTimeIsUTC),
        obsListGlobalFilter = gf.getOrElse(Default.obsListGlobalFilter),
        obsListColumnFilters = cf.getOrElse(Default.obsListColumnFilters)
      )

  given Encoder[UserPreferences] =
    Encoder.forProduct6(
      "isAudioActivated",
      "theme",
      "logLevel",
      "logTimeIsUTC",
      "obsListGlobalFilter",
      "obsListColumnFilters"
    ): p =>
      (
        p.isAudioActivated,
        p.theme,
        p.logLevel,
        p.logTimeIsUTC,
        p.obsListGlobalFilter,
        p.obsListColumnFilters
      )
