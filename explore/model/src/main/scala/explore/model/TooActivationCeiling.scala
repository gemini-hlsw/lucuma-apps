// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.TooActivation
import monocle.Focus
import monocle.Lens

// The most disruptive Target of Opportunity activation any observation in a program may
// declare. `effective` is `explicit` when set and `default` otherwise; only `explicit` is
// editable, and it is all the API accepts as input.
case class TooActivationCeiling(
  effective: TooActivation,
  default:   TooActivation,
  explicit:  Option[TooActivation]
) derives Eq

object TooActivationCeiling:
  val effective: Lens[TooActivationCeiling, TooActivation]        =
    Focus[TooActivationCeiling](_.effective)
  val default: Lens[TooActivationCeiling, TooActivation]          =
    Focus[TooActivationCeiling](_.default)
  val explicit: Lens[TooActivationCeiling, Option[TooActivation]] =
    Focus[TooActivationCeiling](_.explicit)

  val Default: TooActivationCeiling =
    TooActivationCeiling(TooActivation.None, TooActivation.None, none)

  // The three fields are siblings of the proposal type's own fields, not a nested object,
  // so this decodes off the proposal cursor itself.
  given Decoder[TooActivationCeiling] = Decoder.instance: c =>
    for
      effective <- c.get[TooActivation]("tooActivationCeiling")
      default   <- c.get[TooActivation]("defaultTooActivationCeiling")
      explicit  <- c.get[Option[TooActivation]]("explicitTooActivationCeiling")
    yield TooActivationCeiling(effective, default, explicit)
