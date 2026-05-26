// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import coulomb.integrations.cats.all.given
import io.circe.Decoder
import io.circe.Encoder
import io.circe.refined.*
import lucuma.core.circe.coulomb.given
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.refined.given
import monocle.Focus
import monocle.Lens

case class CurrentConditions(
  ce: Option[CloudExtinction],
  iq: Option[ImageQuality],
  sb: Option[SkyBackground],
  wv: Option[WaterVapor]
) derives Eq,
      Encoder.AsObject,
      Decoder

object CurrentConditions:

  val Unknown: CurrentConditions =
    CurrentConditions(
      none,
      none,
      none,
      none
    )

  val Worst: CurrentConditions =
    CurrentConditions(
      CloudExtinction.Preset.ThreePointZero.toCloudExtinction.some,
      ImageQuality.Preset.TwoPointZero.toImageQuality.some,
      SkyBackground.Bright.some,
      WaterVapor.Wet.some
    )

  val Nominal: CurrentConditions =
    CurrentConditions(
      CloudExtinction.Preset.OnePointZero.toCloudExtinction.some,
      ImageQuality.Preset.OnePointZero.toImageQuality.some,
      SkyBackground.Gray.some,
      WaterVapor.Wet.some
    )

  val Best: CurrentConditions =
    CurrentConditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudExtinction.Preset.PointOne.toCloudExtinction.some,
      ImageQuality.Preset.PointOne.toImageQuality.some,
      SkyBackground.Darkest.some,
      WaterVapor.VeryDry.some
    )

  val Default: CurrentConditions =
    Unknown // Taken from ODB

  val ce: Lens[CurrentConditions, Option[CloudExtinction]] = Focus[CurrentConditions](_.ce)
  val iq: Lens[CurrentConditions, Option[ImageQuality]]    = Focus[CurrentConditions](_.iq)
  val sb: Lens[CurrentConditions, Option[SkyBackground]]   = Focus[CurrentConditions](_.sb)
  val wv: Lens[CurrentConditions, Option[WaterVapor]]      = Focus[CurrentConditions](_.wv)
