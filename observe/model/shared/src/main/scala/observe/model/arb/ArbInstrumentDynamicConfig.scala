// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig.given
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import observe.model.InstrumentDynamicConfig
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbInstrumentDynamicConfig:
  given Arbitrary[InstrumentDynamicConfig] = Arbitrary:
    Gen.oneOf(
      arbitrary[gmos.DynamicConfig.GmosNorth].map(InstrumentDynamicConfig.GmosNorth(_)),
      arbitrary[gmos.DynamicConfig.GmosSouth].map(InstrumentDynamicConfig.GmosSouth(_)),
      arbitrary[Flamingos2DynamicConfig].map(InstrumentDynamicConfig.Flamingos2(_)),
      arbitrary[Igrins2DynamicConfig].map(InstrumentDynamicConfig.Igrins2(_))
    )

  given Cogen[InstrumentDynamicConfig] =
    Cogen[
      Either[
        Either[gmos.DynamicConfig.GmosNorth, gmos.DynamicConfig.GmosSouth],
        Either[Flamingos2DynamicConfig, Igrins2DynamicConfig]
      ]
    ].contramap:
      case InstrumentDynamicConfig.GmosNorth(c)  => Left(Left(c))
      case InstrumentDynamicConfig.GmosSouth(c)  => Left(Right(c))
      case InstrumentDynamicConfig.Flamingos2(c) => Right(Left(c))
      case InstrumentDynamicConfig.Igrins2(c)    => Right(Right(c))

object ArbInstrumentDynamicConfig extends ArbInstrumentDynamicConfig
