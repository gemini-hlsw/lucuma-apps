// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import algebra.instances.all.given
import cats.*
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import coulomb.*
import coulomb.integrations.cats.all.given
import coulomb.units.accepted.ArcSecond
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.util.NewType
import lucuma.schemas.model.navigate.LightSource
import observe.server.Length
import observe.server.tcs.*

import scala.language.implicitConversions

import Length.given

/**
 * Created by jluhrs on 7/30/15.
 *
 * Most of the code deals with representing the state of the TCS subsystems.
 */

object TcsController {

  /* Data type for science fold position. */
  case class LightPath(source: LightSource, sink: LightSinkName) derives Eq

  object LightPath {

    given Show[LightPath] = Show.fromToString

  }

  object OffsetP extends NewType[Quantity[Double, ArcSecond]]
  type OffsetP = OffsetP.Type

  object OffsetQ extends NewType[Quantity[Double, ArcSecond]]
  type OffsetQ = OffsetQ.Type

  case class InstrumentOffset(p: OffsetP, q: OffsetQ)

  object InstrumentOffset {

    given Eq[InstrumentOffset] = Eq.by(f => (f.p.value.value, f.q.value.value))

  }

  case class TelescopeConfig(
    offsetA:  Option[InstrumentOffset],
    wavelA:   Option[Wavelength],
    defocusB: Option[Length]
  ) derives Eq

  object TelescopeConfig {
    given Show[TelescopeConfig] = Show.fromToString
  }

  sealed trait Subsystem extends Product with Serializable

  object Subsystem {
    // Instrument internal WFS
    case object OIWFS extends Subsystem

    // Peripheral WFS 1
    case object PWFS1 extends Subsystem

    // Peripheral WFS 2
    case object PWFS2 extends Subsystem

    // Internal AG mechanisms (science fold, AC arm)
    case object AGUnit extends Subsystem

    // Mount and cass-rotator
    case object Mount extends Subsystem

    // Primary mirror
    case object M1 extends Subsystem

    // Secondary mirror
    case object M2 extends Subsystem

    // Gemini Adaptive Optics System (GeMS or Altair)
    case object Gaos extends Subsystem

    val allList: List[Subsystem]                = List(PWFS1, PWFS2, OIWFS, AGUnit, Mount, M1, M2, Gaos)
    given Order[Subsystem]                      = Order.from { case (a, b) =>
      allList.indexOf(a) - allList.indexOf(b)
    }
    val allButGaos: NonEmptySet[Subsystem]      =
      NonEmptySet.of(OIWFS, PWFS1, PWFS2, AGUnit, Mount, M1, M2)
    val allButGaosNorOi: NonEmptySet[Subsystem] =
      NonEmptySet.of(PWFS1, PWFS2, AGUnit, Mount, M1, M2)

    given Show[Subsystem] = Show.show(_.productPrefix)
    given Eq[Subsystem]   = Eq.fromUniversalEquals
  }

  /**
   * Configuration of a step. Guiding is the step's own guide state. The guide configuration
   * received from Navigate is only shown to the user, and plays no part in configuring the step.
   */
  case class TcsConfig[S <: Site](
    tc:         TelescopeConfig,
    lightPath:  LightPath,
    instrument: Instrument,
    guiding:    StepGuideState
  )

  object TcsConfig {
    given [S <: Site]: Show[TcsConfig[S]] = Show.show { x =>
      s"(telConfig = ${x.tc.show}, lightPath = ${x.lightPath.show}, guiding = ${x.guiding})"
    }
  }
}
